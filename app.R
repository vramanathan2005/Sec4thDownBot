library(dplyr)
library(purrr)
library(stringr)
library(googledrive)
library(DT)            # For the clickable datatables
library(httr)
library(jsonlite)
library(memoise)
library(glue)
library(FNN)
library(shiny)
library(showtext)
library(png)
library(tidymodels) 
library(xgboost)

options(shiny.timeout = 900)
# ---------- generic helpers ----------
# ---------- Coach-Decision API (Cloud Run) ----------
coach_api <- memoise(function(team, distance, yards_to_goal,
                              clock_minutes, clock_seconds,
                              period, score_diff,
                              offense_timeouts, defense_timeouts) {
  
  res <- httr::POST(
    url    = "https://sec-coach-api-230270473808.us-central1.run.app/predict_decision",
    encode = "json",
    body   = list(
      team              = team,
      distance          = distance,
      yards_to_goal     = yards_to_goal,
      clock_minutes     = clock_minutes,
      clock_seconds     = clock_seconds,
      period            = period,
      score_diff        = score_diff,
      offense_timeouts  = offense_timeouts,
      defense_timeouts  = defense_timeouts
    )
  )
  httr::stop_for_status(res)
  httr::content(res, as = "parsed")$coach_decision
})


sim_url    <- function(slug)
  glue::glue("https://raw.githubusercontent.com/vramanathan2005/",
             "Sec4thDownBot/main/similarity/{slug}_sim.rds")

drive_url  <- function(id)
  glue::glue("https://drive.google.com/uc?export=download&id={id}")

load_rds_url <- function(url) {
  tmp <- tempfile(fileext = ".rds")
  download.file(url, tmp, mode = "wb", quiet = TRUE)
  readRDS(tmp)
}

# ---------- memoised accessors ----------
get_coach_model   <- memoise(function(team) {
  slug <- names(pretty_names)[pretty_names == team]
  load_rds_url(model_url(slug))
})

get_similarity    <- memoise(function(team) {
  slug <- names(pretty_names)[pretty_names == team]
  load_rds_url(sim_url(slug))
})

get_global_model <- memoise(function() {
  download_drive_rds("1tZdo45LcPH0YTR5lIMWYPrUMCmij2eOD")
})


get_wp_api <- memoise(function(distance, yards_to_goal,
                               clock_minutes, clock_seconds,
                               period, score_diff,
                               offense_timeouts, defense_timeouts,
                               goal_to_go, under_two) {
  
  res <- httr::POST(
    url = "https://wp-api-230270473808.us-central1.run.app/predict_wp",  # ← new URL
    body = list(
      distance          = distance,
      yards_to_goal     = yards_to_goal,
      clock_minutes     = clock_minutes,
      clock_seconds     = clock_seconds,
      period            = period,
      score_diff        = score_diff,
      offense_timeouts  = offense_timeouts,
      defense_timeouts  = defense_timeouts,
      goal_to_go        = goal_to_go,
      under_two         = under_two
    ),
    encode = "json"          # ← json not form (optional but recommended)
  )
  
  httr::stop_for_status(res)
  jsonlite::fromJSON(httr::content(res, as = "text", encoding = "UTF-8"))$wp
})



options(timeout = 600)
drive_deauth()                                 # use public, no‑login access
download_drive_rds <- function(id) {           # id = the long string
  tmp <- tempfile(fileext = ".rds")            # temp file
  googledrive::drive_download(                 # one HTTP request
    googledrive::as_id(id),
    path      = tmp,
    overwrite = TRUE,
    type      = "raw"                          # skip Google’s HTML wrapper
  )
  readRDS(tmp)                                 # return the object
}

as_drive_dl <- function(id) {
  glue("https://drive.google.com/uc?export=download&id={id}")
}

download_and_read_rds <- function(url_path) {
  temp_file <- tempfile(fileext = ".rds")
  tryCatch({
    # Download the file to the temporary location
    download.file(url_path, destfile = temp_file, mode = "wb", quiet = TRUE)
    # Read the RDS file from the temporary location
    readRDS(temp_file)
  }, error = function(e) {
    message(paste("Error downloading or reading RDS from:", url_path))
    message(e$message)
    return(NULL) # Return NULL if there's an error
  }, finally = {
    # Clean up the temporary file
    if (file.exists(temp_file)) {
      unlink(temp_file)
    }
  })
}
 

## ---- helper look‑ups --------------------------------------------------
pretty_names <- c(
  alabama            = "Alabama",
  arkansas           = "Arkansas",
  auburn             = "Auburn",
  florida            = "Florida",
  georgia            = "Georgia",
  kentucky           = "Kentucky",
  lsu                = "LSU",
  mississippi_state  = "Mississippi State",
  missouri           = "Missouri",
  ole_miss           = "Ole Miss",
  oklahoma           = "Oklahoma",
  south_carolina     = "South Carolina",
  tennessee          = "Tennessee",
  texas              = "Texas",
  texas_a_m          = "Texas A&M",   # <- ampersand kept!
  vanderbilt         = "Vanderbilt"
)

# ──────────────────────────────────────────────────────────────
#  🔒 MODEL‑EVALUATION SNIPPET — COMMENTED OUT
#  These metrics were handy while iterating locally, but the app
#  doesn’t need to recompute them on startup.  Keep for reference;
#  remove entirely if you prefer a cleaner file.
# ──────────────────────────────────────────────────────────────

# # Alabama
# alabama_preds <- predict(alabama_model$model, new_data = alabama_model$test_data) %>%
#   bind_cols(alabama_model$test_data)
# metrics(alabama_preds, truth = decision, estimate = .pred_class)
# conf_mat(alabama_preds, truth = decision, estimate = .pred_class)
#
# # Arkansas
# arkansas_preds <- predict(arkansas_model$model, new_data = arkansas_model$test_data) %>%
#   bind_cols(arkansas_model$test_data)
# metrics(arkansas_preds, truth = decision, estimate = .pred_class)
# conf_mat(arkansas_preds, truth = decision, estimate = .pred_class)
#
# # Auburn
# auburn_preds <- predict(auburn_model$model, new_data = auburn_model$test_data) %>%
#   bind_cols(auburn_model$test_data)
# metrics(auburn_preds, truth = decision, estimate = .pred_class)
# conf_mat(auburn_preds, truth = decision, estimate = .pred_class)
#
# # Florida
# florida_preds <- predict(florida_model$model, new_data = florida_model$test_data) %>%
#   bind_cols(florida_model$test_data)
# metrics(florida_preds, truth = decision, estimate = .pred_class)
# conf_mat(florida_preds, truth = decision, estimate = .pred_class)
#
# # Georgia
# georgia_preds <- predict(georgia_model$model, new_data = georgia_model$test_data) %>%
#   bind_cols(georgia_model$test_data)
# metrics(georgia_preds, truth = decision, estimate = .pred_class)
# conf_mat(georgia_preds, truth = decision, estimate = .pred_class)
#
# # Kentucky
# kentucky_preds <- predict(kentucky_model$model, new_data = kentucky_model$test_data) %>%
#   bind_cols(kentucky_model$test_data)
# metrics(kentucky_preds, truth = decision, estimate = .pred_class)
# conf_mat(kentucky_preds, truth = decision, estimate = .pred_class)
#
# # LSU
# lsu_preds <- predict(lsu_model$model, new_data = lsu_model$test_data) %>%
#   bind_cols(lsu_model$test_data)
# metrics(lsu_preds, truth = decision, estimate = .pred_class)
# conf_mat(lsu_preds, truth = decision, estimate = .pred_class)
#
# # Mississippi State
# mississippi_state_preds <- predict(mississippi_state_model$model, new_data = mississippi_state_model$test_data) %>%
#   bind_cols(mississippi_state_model$test_data)
# metrics(mississippi_state_preds, truth = decision, estimate = .pred_class)
# conf_mat(mississippi_state_preds, truth = decision, estimate = .pred_class)
#
# # Missouri
# missouri_preds <- predict(missouri_model$model, new_data = missouri_model$test_data) %>%
#   bind_cols(missouri_model$test_data)
# metrics(missouri_preds, truth = decision, estimate = .pred_class)
# conf_mat(missouri_preds, truth = decision, estimate = .pred_class)
#
# # Ole Miss
# ole_miss_preds <- predict(ole_miss_model$model, new_data = ole_miss_model$test_data) %>%
#   bind_cols(ole_miss_model$test_data)
# metrics(ole_miss_preds, truth = decision, estimate = .pred_class)
# conf_mat(ole_miss_preds, truth = decision, estimate = .pred_class)
#
# # Oklahoma
# oklahoma_preds <- predict(oklahoma_model$model, new_data = oklahoma_model$test_data) %>%
#   bind_cols(oklahoma_model$test_data)
# metrics(oklahoma_preds, truth = decision, estimate = .pred_class)
# conf_mat(oklahoma_preds, truth = decision, estimate = .pred_class)
#
# # South Carolina
# south_carolina_preds <- predict(south_carolina_model$model, new_data = south_carolina_model$test_data) %>%
#   bind_cols(south_carolina_model$test_data)
# metrics(south_carolina_preds, truth = decision, estimate = .pred_class)
# conf_mat(south_carolina_preds, truth = decision, estimate = .pred_class)
#
# # Tennessee
# tennessee_preds <- predict(tennessee_model$model, new_data = tennessee_model$test_data) %>%
#   bind_cols(tennessee_model$test_data)
# metrics(tennessee_preds, truth = decision, estimate = .pred_class)
# conf_mat(tennessee_preds, truth = decision, estimate = .pred_class)
#
# # Texas
# texas_preds  <- predict(texas_model$model, new_data = texas_model$test_data) %>%
#   bind_cols(texas_model$test_data)
# metrics(texas_preds,  truth = decision, estimate = .pred_class)
# conf_mat(texas_preds,  truth = decision, estimate = .pred_class)
#
# # Texas A&M
# texas_a_m_preds <- predict(texas_a_m_model$model, new_data = texas_a_m_model$test_data) %>%
#   bind_cols(texas_a_m_model$test_data)
# metrics(texas_a_m_preds, truth = decision, estimate = .pred_class)
# conf_mat(texas_a_m_preds, truth = decision, estimate = .pred_class)
#
# # Vanderbilt
# vanderbilt_preds <- predict(vanderbilt_model$model, new_data = vanderbilt_model$test_data) %>%
#   bind_cols(vanderbilt_model$test_data)
# metrics(vanderbilt_preds, truth = decision, estimate = .pred_class)
# conf_mat(vanderbilt_preds, truth = decision, estimate = .pred_class)



# ---------- WIN-PROBABILITY SIMULATION  ---------------------------------
# Safe null coalescing
`%||%` <- function(a, b) if (!is.null(a)) a else b
# always return a single numeric, even if API gives a vector/list
to_scalar_wp <- function(x) {
  if (length(x) == 0) return(NA_real_)  # API failure → NA
  as.numeric(x[[1]])                    # take the first element
}

simulate_wp_options <- function(situation) {
  
  # helper: hit the Cloud-Run API once for a single-row tibble
  wp_api <- function(row) {
    get_wp_api(
      distance = row$distance,
      yards_to_goal = row$yards_to_goal,
      clock_minutes = row$clock.minutes,
      clock_seconds = row$clock.seconds,
      period = row$period,
      score_diff = row$score_diff,
      offense_timeouts = row$offense_timeouts,
      defense_timeouts = row$defense_timeouts,
      goal_to_go = row$goal_to_go,
      under_two = row$under_two
    )
    
  }
  
  # Unpack the situation
  cur_ytg     <- situation$yards_to_goal
  cur_dist    <- situation$distance
  cur_min     <- situation$clock.minutes
  cur_sec     <- situation$clock.seconds
  cur_q       <- situation$period
  cur_diff    <- situation$score_diff
  cur_o_to    <- situation$offense_timeouts
  cur_d_to    <- situation$defense_timeouts
  
  # Constants
  avg_play_time <- 5
  avg_punt_net  <- 40
  kickoff_ytg   <- 75
  new_clock <- function(m, s, t) {
    tot <- m * 60 + s - t
    list(minutes = max(tot %/% 60, 0), seconds = max(tot %% 60, 0))
  }
  
  # 1 GO – convert
  go_clk <- new_clock(cur_min, cur_sec, avg_play_time)
  conv_ytg <- cur_ytg - cur_dist
  made_td  <- conv_ytg <= 0
  
  go_convert <- if (made_td) {
    tibble::tibble(
      yards_to_goal    = kickoff_ytg,
      distance         = 10,
      clock.minutes    = go_clk$minutes,
      clock.seconds    = go_clk$seconds,
      period           = cur_q,
      score_diff       = cur_diff + 7,
      offense_timeouts = cur_d_to,
      defense_timeouts = cur_o_to,
      goal_to_go       = 0L,
      under_two        = as.integer(go_clk$minutes < 2)
    )
  } else {
    new_dist <- ifelse(conv_ytg <= 10, conv_ytg, 10)
    tibble::tibble(
      yards_to_goal    = conv_ytg,
      distance         = new_dist,
      clock.minutes    = go_clk$minutes,
      clock.seconds    = go_clk$seconds,
      period           = cur_q,
      score_diff       = cur_diff,
      offense_timeouts = cur_o_to,
      defense_timeouts = cur_d_to,
      goal_to_go       = as.integer(new_dist == conv_ytg),
      under_two        = as.integer(go_clk$minutes < 2)
    )
  }
  
  # 2 GO – fail
  go_fail <- tibble::tibble(
    yards_to_goal    = 100 - cur_ytg,
    distance         = 10,
    clock.minutes    = go_clk$minutes,
    clock.seconds    = go_clk$seconds,
    period           = cur_q,
    score_diff       = cur_diff,
    offense_timeouts = cur_d_to,
    defense_timeouts = cur_o_to,
    goal_to_go       = 0L,
    under_two        = as.integer(go_clk$minutes < 2)
  )
  
  # 3 & 4 FIELD GOAL (make/miss)
  fg_clk <- new_clock(cur_min, cur_sec, avg_play_time)
  snap_dist <- 7
  if (cur_ytg <= 42) {
    fg_make <- tibble::tibble(
      yards_to_goal    = kickoff_ytg,
      distance         = 10,
      clock.minutes    = fg_clk$minutes,
      clock.seconds    = fg_clk$seconds,
      period           = cur_q,
      score_diff       = cur_diff + 3,
      offense_timeouts = cur_d_to,
      defense_timeouts = cur_o_to,
      goal_to_go       = 0L,
      under_two        = as.integer(fg_clk$minutes < 2)
    )
    miss_spot <- max(cur_ytg, cur_ytg + snap_dist)
    fg_miss <- tibble::tibble(
      yards_to_goal    = 100 - miss_spot,
      distance         = 10,
      clock.minutes    = fg_clk$minutes,
      clock.seconds    = fg_clk$seconds,
      period           = cur_q,
      score_diff       = cur_diff,
      offense_timeouts = cur_d_to,
      defense_timeouts = cur_o_to,
      goal_to_go       = 0L,
      under_two        = as.integer(fg_clk$minutes < 2)
    )
  } else {
    fg_make <- fg_miss <- NULL
  }
  
  # 5 PUNT
  punt_clk <- new_clock(cur_min, cur_sec, avg_play_time)
  punt_ytg <- max(1, cur_ytg - avg_punt_net)
  punt <- tibble::tibble(
    yards_to_goal    = 100 - punt_ytg,
    distance         = 10,
    clock.minutes    = punt_clk$minutes,
    clock.seconds    = punt_clk$seconds,
    period           = cur_q,
    score_diff       = cur_diff,
    offense_timeouts = cur_d_to,
    defense_timeouts = cur_o_to,
    goal_to_go       = 0L,
    under_two        = as.integer(punt_clk$minutes < 2)
  )
  
  # Simulate WP for each scenario
  wp_current    <- to_scalar_wp( wp_api(situation)   )
  wp_go_convert <- to_scalar_wp( wp_api(go_convert)  )
  wp_go_fail    <- to_scalar_wp( wp_api(go_fail)     )
  wp_fg_make    <- if (!is.null(fg_make)) to_scalar_wp( wp_api(fg_make) ) else NA_real_
  wp_fg_miss    <- if (!is.null(fg_miss)) to_scalar_wp( wp_api(fg_miss) ) else NA_real_
  wp_punt       <- to_scalar_wp( wp_api(punt)        )
  
  options <- c(
    "Go for it (Convert)",
    "Go for it (Fail)",
    "Field Goal (Make)",
    "Field Goal (Miss)",
    "Punt"
  )
  
  wps <- c(
    wp_go_convert,
    wp_go_fail,
    wp_fg_make %||% NA_real_,
    wp_fg_miss %||% NA_real_,
    wp_punt
  )
  
  wp_gain <- round(wps - wp_current, 3)
  
  tibble::tibble(
    Option       = options,
    Projected_WP = round(wps, 3),
    Best         = seq_along(wp_gain) == which.max(replace(wp_gain, is.na(wp_gain), -99))
  )
}


# ---------- 1A. 50‑neighbour pull ----------
get_similar_plays <- function(sim_data, input_situation, k = 50) {
  
  query <- matrix(c(
    input_situation$distance,
    input_situation$yards_to_goal,
    input_situation$clock.minutes,
    input_situation$clock.seconds,
    as.integer(input_situation$period),
    input_situation$score_diff,
    input_situation$offense_timeouts,
    input_situation$defense_timeouts,
    as.integer(input_situation$distance == input_situation$yards_to_goal),
    as.integer(input_situation$clock.minutes < 2)
  ), nrow = 1)
  
  nn   <- FNN::get.knnx(sim_data$matrix, query, k = k)
  sim_data$raw[nn$nn.index[1, ], ]          # return full 50‑row tibble
}

# ---------- 1B. tag how each historical play was called ----------
tag_decision <- function(df) {
  df %>% 
    mutate(
      play_type    = as.character(play_type),   # guarantees a character col
      fg_distance  = yards_to_goal + 17,
      seconds_left = clock.minutes * 60 + clock.seconds,
      decision = case_when(
        str_detect(play_type, "Field Goal") &
          (fg_distance > 60 |
             (score_diff <= -4 & seconds_left < 80 & offense_timeouts == 0)) ~ "Go",
        str_detect(play_type, "Punt") &
          score_diff <= 0 & seconds_left < 40 & offense_timeouts == 0       ~ "Go",
        str_detect(play_type, "Field Goal")                                 ~ "Field Goal",
        str_detect(play_type, "Punt")                                       ~ "Punt",
        TRUE                                                                ~ "Go"
      )
    )
}


# ---------- 1C. summarise frequency + average WP gain ----------
summarise_neighbours <- function(df_tagged) {
  df_tagged %>% 
    mutate(wp_gain = wp_after - wp_before) %>%          # added WP
    group_by(decision) %>% 
    summarise(
      n_plays      = n(),
      pct_plays    = n_plays / 50,
      mean_wp_gain = mean(wp_gain, na.rm = TRUE),
      .groups = "drop"
    ) %>% 
    arrange(desc(n_plays))
}



addResourcePath("custom_fonts", "www/custom_fonts")

# ------------------------------------------------
# UI
# ------------------------------------------------
ui <- fluidPage(
  tags$head(
    tags$link(href = "https://fonts.googleapis.com/css2?family=Roboto+Slab:wght@400;700&display=swap", rel = "stylesheet"),
    tags$style(HTML(
      "
      @font-face {
        font-family: 'arkansas';
        src: url('custom_fonts/arkansas.ttf') format('truetype');
      }
      @font-face {
        font-family: 'Cascadia_Code';
        src: url('custom_fonts/CascadiaCode-Bold.ttf') format('truetype');
      }
      @font-face {
        font-family: 'gators';
        src: url('custom_fonts/gators.ttf') format('truetype');
      }
      @font-face {
        font-family: 'Graduate';
        src: url('custom_fonts/Graduate-Regular.ttf') format('truetype');
      }
      @font-face {
        font-family: 'Merriweather';
        src: url('custom_fonts/Merriweather_48pt-Regular.ttf') format('truetype');
      }
      @font-face {
        font-family: 'Montserrat';
        src: url('custom_fonts/Montserrat-ExtraBoldItalic.ttf') format('truetype');
      }
      @font-face {
        font-family: 'roboto_slab_bold';
        src: url('custom_fonts/RobotoSlab-Bold.ttf') format('truetype');
      }
      @font-face {
        font-family: 'UnitedSansRegBold';
        src: url('custom_fonts/UnitedSansRegBold.ttf') format('truetype');
      }
      @font-face {
        font-family: 'secbot';
        src: url('custom_fonts/secbot.ttf') format('truetype');
      }

      body {
        background-color: white;
        color: #bf5700;
        font-weight: 700;
      }
      .title {
        background-color: #bf5700;
        color: white;
        padding: 20px;
        margin-bottom: 20px;
        text-align: left;
        font-size: 28px;
        font-weight: 600;
        border-radius: 5px;
        display: flex;
        align-items: center;
        gap: 15px;
      }
      .warning-box {
        background-color: #fef3e2;
        color: #bf5700;
        padding: 10px 15px;
        border-radius: 6px;
        font-weight: 600;
        font-size: 16px;
        margin-bottom: 15px;
        border: 1px solid #e8c9a0;
      }"
    ))
    ,
    uiOutput("dynamicFontStyle")
  ),
  uiOutput("dynamicTitle"),
  
  sidebarLayout(
    sidebarPanel(
      # -- 1. Team -----------------------------------------------------------
      selectInput(
        inputId  = "team_choice",
        label    = "Select SEC Team Coach Model:",
        choices  = c("",                       # empty first row → nothing selected
                     "Alabama", "Arkansas", "Auburn", "Florida", "Georgia",
                     "Kentucky", "LSU", "Mississippi State", "Missouri",
                     "Ole Miss", "Oklahoma", "South Carolina", "Tennessee",
                     "Texas", "Texas A&M", "Vanderbilt"),
        selected = ""                          # start blank
      ),
      
      # -- 2. Field-position inputs (start NA so we can test later) ----------
      numericInput("distance",
                   "Distance to First Down (yards):",
                   value = NA, min = 1),
      
      numericInput("yards_to_goal",
                   "Yards to Goal:",
                   value = NA, min = 1, max = 99),
      
      # -- 3. Time / score inputs (keep your previous defaults) --------------
      selectInput("period", "Quarter:", choices = c("1", "2", "3", "4")),
      numericInput("clock_minutes",  "Clock Minutes:",   5, min = 0, max = 15),
      numericInput("clock_seconds",  "Clock Seconds:",   0, min = 0, max = 59),
      numericInput("offense_score",  "Offense Score:",  21),
      numericInput("defense_score",  "Defense Score:",  24),
      numericInput("offense_timeouts","Offense TOs:",    3, min = 0, max = 3),
      numericInput("defense_timeouts","Defense TOs:",    3, min = 0, max = 3)
    )
    ,
    
    mainPanel(
      uiOutput("scoreboard"),
      plotOutput("fieldPosition", width = "1200px", height = "500px"),
      uiOutput("warningBox"),
      uiOutput("recommendedPlay"),
      uiOutput("coachDecision"),
      tableOutput("wpTable"),
      textOutput("playExplanation"),
      DTOutput("decisionSummary"),   # <-- new clickable summary
      DTOutput("decisionDetails"),   # <-- detail table that morphs
    )
  ),
  tags$div(
    id = "playDetailsModal",
    style = "display:none; position:fixed; top:20%; left:25%; width:50%; background:white; border:3px solid #333; border-radius:8px; z-index:1000; padding:20px; box-shadow: 0 4px 12px rgba(0,0,0,0.3);",
    tags$div(class = "modal-body", style = "font-size: 16px; line-height: 1.4;"),
    tags$button("Close", onclick = "document.getElementById('playDetailsModal').style.display='none'",
                style = "margin-top:15px; float:right; background:#ccc; padding:5px 10px; border:none; border-radius:4px;")
  )
  
)



# ------------------------------------------------
# SERVER
# ------------------------------------------------
server <- function(input, output) {
  inputs_ready <- reactive({
    nzchar(input$team_choice) &&          
      !is.na(input$distance)    &&          
      !is.na(input$yards_to_goal)          
  })
  font_add("Montserrat", regular = "www/custom_fonts/Montserrat-ExtraBoldItalic.ttf")
  font_add("roboto_slab_bold", regular = "www/custom_fonts/RobotoSlab-Bold.ttf")
  font_add("arkansas", regular = "www/custom_fonts/arkansas.ttf")
  font_add("Merriweather", regular = "www/custom_fonts/Merriweather_48pt-Regular.ttf")
  font_add("gators", regular = "www/custom_fonts/gators.ttf")
  font_add("Cascadia_Code", regular = "www/custom_fonts/CascadiaCode-Bold.ttf")
  font_add("secbot", regular = "www/custom_fonts/secbot.ttf")
  font_add("Graduate", regular = "www/custom_fonts/Graduate-Regular.ttf")
  font_add("UnitedSansRegBold", regular = "www/custom_fonts/UnitedSansRegBold.ttf")
  showtext::showtext_auto() 
  team_assets <- reactive({
    if (input$team_choice == "Alabama") {
      list(
        sec_logo = "www/alabamasec.png",
        midfield_logo = "www/Alabama_Crimson_Tide_logo.svg.png",
        endzone_left = "ALABAMA",
        endzone_right = "CRIMSON TIDE",
        endzone_left_fill = "#9E1B32",
        endzone_right_fill = "#9E1B32",
        endzone_left_text = "#FFFFFF",
        endzone_right_text = "#FFFFFF",
        main_color = "#9E1B32",
        header_logo = "https://upload.wikimedia.org/wikipedia/commons/thumb/1/12/Alabama_Athletics_logo.svg/800px-Alabama_Athletics_logo.svg.png",
        endzone_font = "Montserrat"
      )
    } else if (input$team_choice == "Arkansas") {
      list(
        sec_logo = "www/arkansassec.png",
        midfield_logo = "www/Arkansas_razorbacks_logo.png",
        endzone_left = "ARKANSAS",
        endzone_right = "RAZORBACKS",
        endzone_left_fill = "#9D2235",
        endzone_right_fill = "#9D2235",
        endzone_left_text = "#FFFFFF",
        endzone_right_text = "#FFFFFF",
        main_color = "#9D2235",
        header_logo = "https://upload.wikimedia.org/wikipedia/en/3/30/Arkansas_razorbacks_logo.png",
        endzone_font = "arkansas"
        
      )
    } else if (input$team_choice == "Auburn") {
      list(
        sec_logo = "www/auburnsec.png",
        midfield_logo = "www/Auburn_Tigers_logo.svg.png",
        endzone_left = "AUBURN",
        endzone_right = "TIGERS",
        endzone_left_fill = "#2e7d32",
        endzone_right_fill = "#2e7d32",
        endzone_left_text = "#FFFFFF",
        endzone_right_text = "#FFFFFF",
        main_color = "#0C2340",
        header_logo = "https://upload.wikimedia.org/wikipedia/commons/1/15/Auburn_Tigers_logo.svg",
        endzone_font = "Merriweather"
        
      )
    } else if (input$team_choice == "Florida") {
      list(
        sec_logo = "www/floridasec.png",
        midfield_logo = "www/Florida_Gators_gator_logo.svg.png",
        endzone_left = "FLORIDA",
        endzone_right = "GATORS",
        endzone_left_fill = "#FA4616",
        endzone_right_fill = "#FA4616",
        endzone_left_text = "#0021A5",
        endzone_right_text = "#0021A5",
        main_color = "#0021A5",
        header_logo = "https://upload.wikimedia.org/wikipedia/en/thumb/1/14/Florida_Gators_gator_logo.svg/1200px-Florida_Gators_gator_logo.svg.png",
        endzone_font = "gators"
      )
    } else if (input$team_choice == "Georgia") {
      list(
        sec_logo = "www/georgiasec.png",
        midfield_logo = "www/Georgia_Athletics_logo.svg.png",
        endzone_left = "GEORGIA",
        endzone_right = "BULLDOGS",
        endzone_left_fill = "#2e7d32",
        endzone_right_fill = "#2e7d32",
        endzone_left_text = "#FFFFFF",
        endzone_right_text = "#FFFFFF",
        main_color = "#BA0C2F",
        header_logo = "https://upload.wikimedia.org/wikipedia/commons/thumb/8/80/Georgia_Athletics_logo.svg/1200px-Georgia_Athletics_logo.svg.png",
        endzone_font = "Cascadia_Code"
        
      )
    } else if (input$team_choice == "Kentucky") {
      list(
        sec_logo = "www/kentuckysec.png",
        midfield_logo = "www/kentucky.png",
        endzone_left = "KENTUCKY",
        endzone_right = "KENTUCKY",
        endzone_left_fill = "#0033A0",
        endzone_right_fill = "#0033A0",
        endzone_left_text = "#FFFFFF",
        endzone_right_text = "#FFFFFF",
        main_color = "#0033A0",
        header_logo = "https://upload.wikimedia.org/wikipedia/commons/thumb/b/b6/Kentucky_Wildcats_logo.svg/1200px-Kentucky_Wildcats_logo.svg.png",
        endzone_font = "roboto_slab_bold"
        
      )
    } else if (input$team_choice == "LSU") {
      list(
        sec_logo = "www/lsusec.png",
        midfield_logo = "www/lsu.png",
        endzone_left_image = "www/lsuyellow.png",  # yellow logo on purple
        endzone_right_image = "www/LSU_Athletics_logo.png",  # purple logo on yellow
        endzone_left_fill = "#461D7C",   # LSU purple
        endzone_right_fill = "#FDD023",  # LSU yellow
        endzone_left_text = NA,  # not used
        endzone_right_text = NA,
        main_color = "#461D7C",
        header_logo = "https://upload.wikimedia.org/wikipedia/commons/thumb/4/4a/LSU_Athletics_logo.svg/1280px-LSU_Athletics_logo.svg.png",
        endzone_font = "Cascadia_Code"
      )
    } else if (input$team_choice == "Mississippi State") {
      list(
        sec_logo = "www/missstate.png",
        midfield_logo = "www/msstate.png",
        endzone_left = "MISSISSIPPI STATE",
        endzone_right = "MISSISSIPPI STATE",
        endzone_left_fill = "#660000",
        endzone_right_fill = "#660000",
        endzone_left_text = "#FFFFFF",
        endzone_right_text = "#FFFFFF",
        main_color = "#660000",
        header_logo = "https://upload.wikimedia.org/wikipedia/commons/thumb/3/36/Mississippi_State_Bulldogs_logo.svg/1200px-Mississippi_State_Bulldogs_logo.svg.png",
        endzone_font = "roboto_slab_bold"
      )
    } else if (input$team_choice == "Missouri") {
      list(
        sec_logo = "www/missourisec.png",
        midfield_logo = "www/missouri.png",
        endzone_left_image = "www/mizzou.png",
        endzone_right_image = "www/tigers.png",
        endzone_left_fill = "#000000",
        endzone_right_fill = "#000000",
        endzone_left_text = "#F1B82D",
        endzone_right_text = "#F1B82D",
        main_color = "#000000",
        header_logo = "https://upload.wikimedia.org/wikipedia/en/thumb/2/2c/Missouri_Tigers_logo.svg/1200px-Missouri_Tigers_logo.svg.png",
        endzone_font = "Cascadia_Code"
      )
    } else if (input$team_choice == "Ole Miss") {
      list(
        sec_logo = "www/olemisssec.png",
        midfield_logo = "www/olemiss.png",
        endzone_left = "OLE MISS",
        endzone_right = "REBELS",
        endzone_left_fill = "#08294D",
        endzone_right_fill = "#08294D",
        endzone_left_text = "#D01C29",
        endzone_right_text = "#D01C29",
        main_color = "#08294D",
        header_logo = "https://upload.wikimedia.org/wikipedia/commons/thumb/7/7c/Ole-miss_logo_from_NCAA.svg/1920px-Ole-miss_logo_from_NCAA.svg.png",
        endzone_font = "roboto_slab_bold"
      )
    } else if (input$team_choice == "Oklahoma") {
      list(
        sec_logo = "www/ousec.png",
        midfield_logo = "www/ou.png",
        endzone_left = "OKLAHOMA",
        endzone_right = "OKLAHOMA",
        endzone_left_fill = "#841617",
        endzone_right_fill = "#841617",
        endzone_left_text = "#FFFFFF",
        endzone_right_text = "#FFFFFF",
        main_color = "#841617",
        header_logo = "https://upload.wikimedia.org/wikipedia/commons/thumb/6/61/Oklahoma_Sooners_logo.svg/1280px-Oklahoma_Sooners_logo.svg.png",
        endzone_font = "secbot"
      )
    } else if (input$team_choice == "South Carolina") {
      list(
        sec_logo = "www/southcarolinasec.png",
        midfield_logo = "www/southcarolina.png",
        endzone_left = "CAROLINA",
        endzone_right = "GAMECOCKS",
        endzone_left_fill = "#2e7d32",
        endzone_right_fill = "#2e7d32",
        endzone_left_text = "#FFFFFF",
        endzone_right_text = "#FFFFFF",
        main_color = "#73000A",
        header_logo = "https://upload.wikimedia.org/wikipedia/commons/thumb/9/94/South_Carolina_Gamecocks_logo.svg/1200px-South_Carolina_Gamecocks_logo.svg.png",
        endzone_font = "secbot"
      )
    } else if (input$team_choice == "Tennessee") {
      list(
        sec_logo = "www/tennesseesec.png",
        midfield_logo = "www/tennessee.png",
        endzone_left_image = "www/tennesseecheckerboard.png",
        endzone_right_image = "www/tennesseecheckerboard.png",
        endzone_left_fill = "#2e7d32",
        endzone_right_fill = "#2e7d32",
        endzone_left_text = "#FFFFFF",
        endzone_right_text = "#FFFFFF",
        main_color = "#FF8200",
        header_logo = "https://upload.wikimedia.org/wikipedia/commons/thumb/e/e3/Tennessee_Volunteers_logo.svg/1200px-Tennessee_Volunteers_logo.svg.png",
        endzone_font = "Cascadia_Code"
      )
    } else if (input$team_choice == "Texas"){
      list(
        sec_logo = "www/Screenshot_2025-05-06_at_9.45.37_PM-removebg-preview.png",
        midfield_logo = "www/longhorns.png",
        endzone_left = "TEXAS",
        endzone_right = "LONGHORNS",
        endzone_left_fill = "#BF5700",
        endzone_right_fill = "#BF5700",
        endzone_left_text = "#FFFFFF",
        endzone_right_text = "#FFFFFF",
        main_color = "#BF5700",
        header_logo = "https://upload.wikimedia.org/wikipedia/commons/8/8d/Texas_Longhorns_logo.svg",
        endzone_font = "roboto_slab_bold"
      )
    } else if (input$team_choice == "Texas A&M") {
      list(
        sec_logo = "www/a&msec.png",
        midfield_logo = "www/texasa&m.png",
        endzone_left = "TEXAS A&M",
        endzone_right = "TEXAS A&M",
        endzone_left_fill = "#500000",
        endzone_right_fill = "#500000",
        endzone_left_text = "#FFFFFF",
        endzone_right_text = "#FFFFFF",
        main_color = "#500000",
        header_logo = "https://upload.wikimedia.org/wikipedia/commons/thumb/e/ee/Texas_A%26M_University_logo.svg/1200px-Texas_A%26M_University_logo.svg.png",
        endzone_font = "Graduate"
      )
    } else if (input$team_choice == "Vanderbilt") {
      list(
        sec_logo = "www/vandysec.png",
        midfield_logo = "www/vanderbilt.png",
        endzone_left = "VANDERBILT",
        endzone_right = "VANDERBILT",
        endzone_left_fill = "#2e7d32",
        endzone_right_fill = "#2e7d32",
        endzone_left_text = "#B3A369",
        endzone_right_text = "#B3A369",
        main_color = "#000000",
        header_logo = "https://upload.wikimedia.org/wikipedia/commons/thumb/3/3d/Vanderbilt_Athletics_logo.svg/1200px-Vanderbilt_Athletics_logo.svg.png",
        endzone_font = "UnitedSansRegBold"
      )
    }
  })
  
  
  
  latest_best_play <- reactiveVal(NULL)
  
  team_box_colors <- function(team) {
    switch(team,
           "Global" = list(bg = "#003087", border = "#FFD100", text = "#FFFFFF"), # SEC navy & gold
           "Alabama" = list(bg = "#9E1B32", border = "#660000", text = "#FFFFFF"),
           "Arkansas" = list(bg = "#9D2235", border = "#660000", text = "#FFFFFF"),
           "Auburn" = list(bg = "#0C2340", border = "#E87722", text = "#FFFFFF"),
           "Florida" = list(bg = "#0021A5", border = "#FA4616", text = "#FFFFFF"),
           "Georgia" = list(bg = "#BA0C2F", border = "#000000", text = "#FFFFFF"),
           "Kentucky" = list(bg = "#0033A0", border = "#005BBB", text = "#FFFFFF"),
           "LSU" = list(bg = "#461D7C", border = "#FDD023", text = "#FFFFFF"),
           "Mississippi State" = list(bg = "#660000", border = "#CCCCCC", text = "#FFFFFF"),
           "Missouri" = list(bg = "#000000", border = "#F1B82D", text = "#FFFFFF"),
           "Ole Miss" = list(bg = "#08294D", border = "#D01C29", text = "#FFFFFF"),
           "Oklahoma" = list(bg = "#841617", border = "#660000", text = "#FFFFFF"),
           "South Carolina" = list(bg = "#73000A", border = "#000000", text = "#FFFFFF"),
           "Tennessee" = list(bg = "#FF8200", border = "#FFB13A", text = "#FFFFFF"),
           "Texas" = list(bg = "#BF5700", border = "#7C2C00", text = "#FFFFFF"),
           "Texas A&M" = list(bg = "#500000", border = "#000000", text = "#FFFFFF"),
           "Vanderbilt" = list(bg = "#866D4B", border = "#000000", text = "#FFFFFF"),
           
           # fallback
           list(bg = "#EEEEEE", border = "#333333", text = "#000000")
    )
  }
  
  output$warningBox <- renderUI({
    if (any(is.null(input$distance), is.null(input$yards_to_goal))) {
      div(class = "warning-box", "Please enter all inputs!")
    }
  })
  
  output$fieldPosition <- renderPlot({
    req(inputs_ready()) 
    assets <- team_assets()  # get the dynamic assets
    endzone_font <- if (input$team_choice == "Alabama") {
      "Montserrat"
    } else if (input$team_choice == "Arkansas") {
      "arkansas"
    } else if (input$team_choice == "Auburn") {
      "Merriweather"
    } else if (input$team_choice == "Florida") {
      "gators"  
    } else if (input$team_choice == "Georgia") {
      "Cascadia_Code" 
    } else if (input$team_choice == "Kentucky") {
      "roboto_slab_bold"  
    } else if (input$team_choice == "LSU") {
      "Cascadia_Code" 
    } else if (input$team_choice == "Mississippi State") {
      "roboto_slab_bold"
    } else if (input$team_choice == "Missouri") {
      "Cascadia_Code"
    } else if (input$team_choice == "Ole Miss") {
      "roboto_slab_bold"  
    } else if (input$team_choice == "Oklahoma") {
      "secbot"  
    } else if (input$team_choice == "South Carolina") {
      "secbot"
    } else if (input$team_choice == "Tennessee") {
      "Cascadia_Code"
    } else if (input$team_choice == "Texas") {
      "roboto_slab_bold"  
    } else if (input$team_choice == "Texas A&M") {
      "Graduate"
    } else if (input$team_choice == "Vanderbilt") {
      "UnitedSansRegBold"
    } else {
      "roboto_slab_bold" 
    }
    
    
    first_down_position <- 10 + (100 - input$yards_to_goal) + input$distance
    ball_position <- 10 + (100 - input$yards_to_goal)
    
    seclogo <- png::readPNG(assets$sec_logo)
    midfield_logo <- png::readPNG(assets$midfield_logo)
    football_icon <- png::readPNG("www/football.png")
    
    plot(NULL, xlim = c(0, 120), ylim = c(0, 1),
         xaxt = 'n', yaxt = 'n', bty = 'n', xlab = '', ylab = '')
    
    rect(0, 0, 120, 1, col = '#2e7d32', border = NA)
    rect(0, 0, 10, 1, col = assets$main_color, border = NA)
    # Draw left end zone
    rect(0, 0, 10, 1, col = assets$endzone_left_fill, border = NA)
    # Draw right end zone
    rect(110, 0, 120, 1, col = assets$endzone_right_fill, border = NA)
    
    # Draw end zone text
    # TENNESSEE full-width image override
    # Tennessee custom rendering
    if (input$team_choice == "Tennessee") {
      # LEFT end zone
      if (!is.null(assets$endzone_left_image)) {
        left_img <- png::readPNG(assets$endzone_left_image)
        left_img <- aperm(left_img, c(2, 1, 3))[, nrow(left_img):1, ]
        rasterImage(left_img, 1.2, 0.15, 8.8, 0.85)
      }
      
      # RIGHT end zone
      if (!is.null(assets$endzone_right_image)) {
        right_img <- png::readPNG(assets$endzone_right_image)
        right_img <- aperm(right_img, c(2, 1, 3))[, nrow(right_img):1, ]
        rasterImage(right_img, 111.2, 0.15, 118.8, 0.85)
      }
    } else {
      use_same_text <- (
        (is.null(assets$endzone_left_image) || is.na(assets$endzone_left_image)) &&
          (is.null(assets$endzone_right_image) || is.na(assets$endzone_right_image)) &&
          !is.null(assets$endzone_left) && !is.null(assets$endzone_right) &&
          !is.na(assets$endzone_left) && !is.na(assets$endzone_right) &&
          assets$endzone_left == assets$endzone_right
      )
      
      left_cex <- if (input$team_choice == "Mississippi State") 3.8 else 5.8
      right_cex <- if (input$team_choice == "Mississippi State") 3.8 else if (use_same_text) 5.8 else 4.8
      
      # Default LEFT
      if (!is.null(assets$endzone_left_image) && !is.na(assets$endzone_left_image)) {
        img <- png::readPNG(assets$endzone_left_image)
        img <- aperm(img, c(2, 1, 3))[, nrow(img):1, ]
        rasterImage(img, 1.2, 0.15, 8.8, 0.85)
      } else {
        text(5, 0.5, assets$endzone_left,
             col = assets$endzone_left_text,
             cex = left_cex,
             srt = 90,
             family = endzone_font)
      }
      
      # Default RIGHT
      if (!is.null(assets$endzone_right_image) && !is.na(assets$endzone_right_image)) {
        img <- png::readPNG(assets$endzone_right_image)
        img <- aperm(img, c(2, 1, 3))[, nrow(img):1, ]
        rasterImage(img, 111.2, 0.15, 118.8, 0.85)
      } else {
        text(115, 0.5, assets$endzone_right,
             col = assets$endzone_right_text,
             cex = right_cex,
             srt = 270,
             family = endzone_font)
      }
    }
    
    
    
    
    
    abline(v = seq(10, 110, by = 5), col = 'white')
    text(seq(20, 100, by = 10), 0.12, labels = c('10','20','30','40','50','40','30','20','10'),
         col = 'white', cex = 2, family = 'roboto_slab_bold')
    text(seq(20, 100, by = 10), 0.88, labels = c('10','20','30','40','50','40','30','20','10'),
         col = 'white', cex = 2, srt = 180, family = 'roboto_slab_bold')
    
    for (x in 11:109) {
      if (x %% 5 == 0) {
        segments(x - 0.25, 0.29, x + 0.25, 0.29, col = 'white', lwd = 3)
        segments(x - 0.25, 0.71, x + 0.25, 0.71, col = 'white', lwd = 3)
      } else {
        segments(x, 0.26, x, 0.29, col = 'white', lwd = 3)
        segments(x, 0.71, x, 0.74, col = 'white', lwd = 3)
      }
    }
    
    rasterImage(seclogo, 33.5, 0.12, 36.5, 0.21)
    rasterImage(seclogo, 83.5, 0.79, 86.5, 0.88)
    
    rasterImage(midfield_logo, 50, 0.35, 70, 0.65)
    
    segments(ball_position, 0, ball_position, 1, col = 'blue', lwd = 3)
    if (!is.null(first_down_position) && first_down_position < 110) {
      segments(first_down_position, 0, first_down_position, 1, col = 'yellow', lwd = 3)
    }
    rasterImage(football_icon, ball_position - 1, 0.45, ball_position + 1, 0.55, interpolate = FALSE)
  })
  
  get_fit <- function(x) if ("model" %in% names(x)) x$model else x
  
  output$recommendedPlay <- renderUI({
    req(inputs_ready()) 
    
    situation <- tibble(
      distance = input$distance,
      yards_to_goal = input$yards_to_goal,
      clock.minutes = input$clock_minutes,
      clock.seconds = input$clock_seconds,
      period = as.integer(input$period),
      score_diff = input$offense_score - input$defense_score,
      offense_timeouts = input$offense_timeouts,
      defense_timeouts = input$defense_timeouts,
      fg_distance = input$yards_to_goal + 17,
      goal_to_go = as.integer(input$distance == input$yards_to_goal), # Change to as.integer
      under_two = as.integer(input$clock_minutes < 2)                 # Change to as.integer
    )
    
    pred <- predict(get_fit(get_global_model()), new_data = situation)
    play <- pred$.pred_class
    
    div(
      style = "background-color: #002D72; border: 4px solid #FFC72C; color: white;
              padding: 20px; border-radius: 8px; font-size: 24px; font-weight: 700;
              max-width: 600px; margin: 10px auto;",
      paste("Recommended Play (All FBS Model):", play)
    )
  })
  

  
  output$coachDecision <- renderUI({
    req(inputs_ready()) 
    
    situation <- tibble(
      distance = input$distance,
      yards_to_goal = input$yards_to_goal,
      clock.minutes = input$clock_minutes,
      clock.seconds = input$clock_seconds,
      period = as.integer(input$period),
      score_diff = input$offense_score - input$defense_score,
      offense_timeouts = input$offense_timeouts,
      defense_timeouts = input$defense_timeouts,
      fg_distance = input$yards_to_goal + 17,
      goal_to_go = as.integer(input$distance == input$yards_to_goal), # Change to as.integer
      under_two = as.integer(input$clock_minutes < 2)                 # Change to as.integer
    )
    
    play <- coach_api(
      input$team_choice,
      input$distance, input$yards_to_goal,
      input$clock_minutes, input$clock_seconds,
      as.integer(input$period),
      input$offense_score - input$defense_score,
      input$offense_timeouts, input$defense_timeouts
    )
    
    colors <- switch(input$team_choice,
                     "Global" = list(bg = "#002D72", border = "#FFC72C", text = "#FFFFFF"),
                     "Alabama" = list(bg = "#7D001C", border = "#FFFFFF", text = "#FFFFFF"),
                     "Arkansas" = list(bg = "#9D2235", border = "#FFCCCB", text = "#FFFFFF"),
                     "Auburn" = list(bg = "#0C2340", border = "#F47920", text = "#FFFFFF"),
                     "Florida" = list(bg = "#0021A5", border = "#FA4616", text = "#FFFFFF"),
                     "Georgia" = list(bg = "#BA0C2F", border = "#000000", text = "#FFFFFF"),
                     "Kentucky" = list(bg = "#005DAA", border = "#FFFFFF", text = "#FFFFFF"),
                     "LSU" = list(bg = "#461D7C", border = "#FDB827", text = "#FFFFFF"),
                     "Mississippi State" = list(bg = "#660000", border = "#FFFFFF", text = "#FFFFFF"),
                     "Missouri" = list(bg = "#000000", border = "#F1B82D", text = "#FFFFFF"),
                     "Ole Miss" = list(bg = "#08294D", border = "#D01C29", text = "#FFFFFF"),
                     "Oklahoma" = list(bg = "#841617", border = "#FFFFFF", text = "#FFFFFF"),
                     "South Carolina" = list(bg = "#73000A", border = "#000000", text = "#FFFFFF"),
                     "Tennessee" = list(bg = "#FF8200", border = "#FFFFFF", text = "#FFFFFF"),
                     "Texas" = list(bg = "#BF5700", border = "#FFFFFF", text = "#FFFFFF"),
                     "Texas A&M" = list(bg = "#500000", border = "#FFFFFF", text = "#FFFFFF"),
                     "Vanderbilt" = list(bg = "#000000", border = "#B3A369", text = "#FFFFFF")
    )
    
    div(
      style = paste0("background-color: ", colors$bg, "; border: 4px solid ", colors$border, "; color: ", colors$text, ";
                    padding: 20px; border-radius: 8px; font-size: 24px; font-weight: 700;
                    max-width: 600px; margin: 10px auto;"),
      paste(input$team_choice, "Coach's Decision Model:", play)
    )
  })
  
  output$wpTable <- renderUI({
    req(inputs_ready()) 
    
    situation <- tibble(
      yards_to_goal = input$yards_to_goal,
      distance = input$distance,
      clock.minutes = input$clock_minutes,
      clock.seconds = input$clock_seconds,
      period = as.integer(input$period),
      score_diff = input$offense_score - input$defense_score,
      offense_timeouts = input$offense_timeouts,
      defense_timeouts = input$defense_timeouts,
      goal_to_go = factor(input$distance == input$yards_to_goal, levels = c(FALSE, TRUE)),
      under_two = factor(input$clock_minutes < 2, levels = c(FALSE, TRUE))
    )
    
    wp_df <- simulate_wp_options(situation)
    
    
    colors <- switch(input$team_choice,
                     "Alabama" = list(bg = "#7D001C", border = "#FFFFFF", text = "#FFFFFF"),
                     "Arkansas" = list(bg = "#9D2235", border = "#FFFFFF", text = "#FFFFFF"),
                     "Auburn" = list(bg = "#0C2340", border = "#FF6600", text = "#FFFFFF"),
                     "Florida" = list(bg = "#0021A5", border = "#FA4616", text = "#FFFFFF"),
                     "Georgia" = list(bg = "#BA0C2F", border = "#000000", text = "#FFFFFF"),
                     "Kentucky" = list(bg = "#005DAA", border = "#FFFFFF", text = "#FFFFFF"),
                     "LSU" = list(bg = "#461D7C", border = "#FDB827", text = "#FFFFFF"),
                     "Mississippi State" = list(bg = "#660000", border = "#FFFFFF", text = "#FFFFFF"),
                     "Missouri" = list(bg = "#000000", border = "#F1B82D", text = "#FFFFFF"),
                     "Ole Miss" = list(bg = "#0066CC", border = "#CC0000", text = "#FFFFFF"),
                     "Oklahoma" = list(bg = "#841617", border = "#FFCCCB", text = "#FFFFFF"),
                     "South Carolina" = list(bg = "#73000A", border = "#000000", text = "#FFFFFF"),
                     "Tennessee" = list(bg = "#FF8200", border = "#FFFFFF", text = "#FFFFFF"),
                     "Texas" = list(bg = "#BF5700", border = "#FFFFFF", text = "#FFFFFF"),
                     "Texas A&M" = list(bg = "#500000", border = "#FFFFFF", text = "#FFFFFF"),
                     "Vanderbilt" = list(bg = "#000000", border = "#B3A369", text = "#FFFFFF"),
                     list(bg = "#222222", border = "#CCCCCC", text = "#FFFFFF")
    )
    
    # Build HTML table manually so colors apply:
    # ---- build rows ----
    rows <- purrr::map_chr(
      seq_len(nrow(wp_df)),
      function(i) {
        rw <- wp_df[i, ]                 # keep it a tibble, names are preserved
        row_bg <- if (rw$Best) "#d4edda" else "white"
        proj_wp <- sprintf("%.3f", rw$Projected_WP)
        
        paste0(
          "<tr style='background:", row_bg, "; color:", colors$bg, ";'>",
          "<td style='padding:8px 15px; border:1px solid ", colors$border, ";'>", rw$Option, "</td>",
          "<td style='padding:8px 15px; text-align:right; border:1px solid ", colors$border, ";'>", proj_wp, "</td>",
          "</tr>"
        )
      }
    )
    
    # ---- table shell ----
    table_html <- paste0(
      "<table style='width:100%; border-collapse:collapse; border:2px solid ", colors$border, ";'>",
      "<thead style='background:", colors$bg, "; color:", colors$text, ";'>",
      "<tr>",
      "<th style='text-align:left;  padding:8px 15px; border:1px solid ", colors$border, ";'>Option</th>",
      "<th style='text-align:right; padding:8px 15px; border:1px solid ", colors$border, ";'>Projected&nbsp;WP</th>",
      "</tr>",
      "</thead>",
      "<tbody>", paste(rows, collapse = ""), "</tbody>",
      "</table>"
    )
    
    
    
    div(
      style = paste0(
        "background-color: ", colors$bg, "; ",
        "border: 4px solid ", colors$border, "; ",
        "color: ", colors$text, "; ",
        "padding: 20px; border-radius: 8px; ",
        "font-size: 20px; font-weight: 700; ",
        "max-width: 600px; margin: 10px auto;"
      ),
      HTML(table_html)
    )
  })
  
  
  
  output$playExplanation <- renderText({
    play <- latest_best_play()
    if (is.null(play)) return("")
    switch(play,
           "Go for it" = "Go for it based on distance & win probability.",
           "Field Goal" = "Field goal attempt recommended due to field position.",
           "Punt" = "Punt suggested due to field position & time left.",
           "Other recommendation.")
  })
  
  output$dynamicTitle <- renderUI({
    assets <- team_assets()
    div(
      style = paste0(
        "background-color: ", assets$main_color, ";",
        "color: white; padding: 20px; margin-bottom: 20px; ",
        "text-align: left; font-size: 28px; font-weight: 600; border-radius: 5px; ",
        "display: flex; align-items: center; gap: 15px;"
      ),
      div(
        style = "background-color: white; padding: 5px; border-radius: 4px;",
        img(src = assets$header_logo, height = "40px")
      ),
      "4th Down Bot"
    )
  })
  
  
  output$scoreboard <- renderUI({
    assets <- team_assets()
    div(
      style = "display: flex; justify-content: center; align-items: center; padding: 8px 12px; border-radius: 10px; font-weight: bold; margin-bottom: 10px; width: 100%; max-width: 900px; margin-left: auto; margin-right: auto;",
      
      # OFFENSE block
      div(
        style = paste0(
          "background-color: ", assets$main_color, 
          "; color: white; display: flex; flex-direction: row; align-items: center; justify-content: space-between; padding: 5px 10px; border-radius: 6px 0 0 6px; flex: 1;"
        ),
        
        div(
          style = "background-color: white; padding: 3px; border-radius: 4px; margin-right: 8px;",
          img(src = assets$header_logo, height = "28px")
        ),
        
        div(style = "text-align: center;", 
            div(style = "font-size: 14px;", "OFFENSE"), 
            div(style = "font-size: 28px;", input$offense_score)
        ),
        
        div(
          style = "display: flex; flex-direction: column; gap: 2px; margin-left: 10px;",
          lapply(1:3, function(i) {
            timeout_color <- if (i <= input$offense_timeouts) "#ffffff" else "#666666"
            div(style = paste0(
              "width: 6px; height: 10px; background-color: ", timeout_color, "; border-radius: 1px;")
            )
          })
        )
      ),
      
      # MIDDLE block
      div(
        style = "background-color: #333; color: white; padding: 5px 15px; text-align: center; flex: 1.5; border-left: 2px solid #666; border-right: 2px solid #666;",
        div(style = "font-size: 13px;", paste("4th &", input$distance)),
        div(style = "font-size: 20px;", sprintf("%02d:%02d", input$clock_minutes, input$clock_seconds)),
        div(style = "font-size: 13px;", paste("Q", input$period))
      ),
      
      # DEFENSE block
      div(
        style = "background-color: #003087; color: white; display: flex; flex-direction: row; align-items: center; justify-content: space-between; padding: 5px 10px; border-radius: 0 6px 6px 0; flex: 1;",
        div(
          style = "display: flex; flex-direction: column; gap: 2px; margin-right: 10px;",
          lapply(1:3, function(i) {
            timeout_color <- if (i <= input$defense_timeouts) "#ffffff" else "#666666"
            div(style = paste0("width: 6px; height: 10px; background-color: ", timeout_color, "; border-radius: 1px;"))
          })
        ),
        div(style = "text-align: center;", div(style = "font-size: 14px;", "DEFENSE"), div(style = "font-size: 28px;", input$defense_score))
      )
    )
  })
  output$dynamicFontStyle <- renderUI({
    font_family <- team_assets()$endzone_font
    req(input$team_choice, font_family)
    
    colors <- switch(input$team_choice,
                     "Alabama" = list(bg = "#7D001C"),
                     "Arkansas" = list(bg = "#9D2235"),
                     "Auburn" = list(bg = "#0C2340"),
                     "Florida" = list(bg = "#0021A5"),
                     "Georgia" = list(bg = "#BA0C2F"),
                     "Kentucky" = list(bg = "#005DAA"),
                     "LSU" = list(bg = "#461D7C"),
                     "Mississippi State" = list(bg = "#660000"),
                     "Missouri" = list(bg = "#000000"),
                     "Ole Miss" = list(bg = "#0066CC"),
                     "Oklahoma" = list(bg = "#841617"),
                     "South Carolina" = list(bg = "#73000A"),
                     "Tennessee" = list(bg = "#FF8200"),
                     "Texas" = list(bg = "#BF5700"),
                     "Texas A&M" = list(bg = "#500000"),
                     "Vanderbilt" = list(bg = "#000000"),
                     list(bg = "#222222")
    )
    
    tags$style(HTML(sprintf("
    html, body, h1, h2, h3, h4, h5, h6, div, span, p, th, td, button, input, select, label {
      font-family: '%s', sans-serif !important;
    }
    select, option, label {
      color: %s !important;
    }
  ", font_family, colors$bg)))
  })
  
  nn50 <- reactive({
    req(inputs_ready()) 
    
    situation <- tibble(
      distance         = input$distance,
      yards_to_goal    = input$yards_to_goal,
      clock.minutes    = input$clock_minutes,
      clock.seconds    = input$clock_seconds,
      period           = as.integer(input$period),
      score_diff       = input$offense_score - input$defense_score,
      offense_timeouts = input$offense_timeouts,
      defense_timeouts = input$defense_timeouts
    )
    
    get_similar_plays(
      get_similarity(input$team_choice),
      situation,
      k = 50
    ) %>% 
      tag_decision()
  })
  style_all_team_colour <- function(dt_obj, colour){
    dt_obj %>% formatStyle(
      columns = names(dt_obj$x$data),
      color   = colour
    )
  }
  
  output$decisionSummary <- renderDT({
    assets <- team_assets()
    
    # --- build the DT ---
    dt <- datatable(
      summarise_neighbours(nn50()),
      rownames  = FALSE,
      selection = "single",
      colnames  = c("Decision", "# Plays", "% of 50", "Avg WP +"),
      options   = list(pageLength = 5, dom = "t"),
      caption   = htmltools::tags$caption(
        style = paste0("caption-side: top; text-align: left;",
                       "font-size: 20px; font-weight: 700;",
                       "color:", assets$main_color, ";"),
        "Historical Outcomes of the 50 Most Similar 4th‑Down Situations (2019‑24)")
    ) |>
      formatPercentage("pct_plays", 1) |>
      formatRound("mean_wp_gain", 3)
    
    # --- colour every cell in team colour ---
    dt <- style_all_team_colour(dt, assets$main_color)
    
    dt                      # <- **return the object**
  })
  
  
  selected_decision <- reactiveVal(NULL)
  
  observeEvent(input$decisionSummary_rows_selected, {
    row <- input$decisionSummary_rows_selected
    if (length(row) == 1) {
      # pull the decision name from the summary table
      dec <- summarise_neighbours(nn50())$decision[row]
      selected_decision(dec)
    }
  })
  output$decisionDetails <- renderDT({
    req(selected_decision())
    assets <- team_assets()
    
    # --- filter the 50 similar plays to the clicked decision ---
    raw <- nn50() %>% 
      filter(decision == selected_decision()) %>% 
      mutate(
        Clock  = sprintf("%02d:%02d", clock.minutes, clock.seconds),
        `WP +` = round(wp_after - wp_before, 3)
      ) %>% 
      rename(
        Year           = year,
        Distance       = distance,
        `Yards to Goal`= yards_to_goal
      ) %>% 
      select(
        Year, Week = week, Offense = pos_team, Opponent = def_pos_team,
        Decision = decision, `Play Type` = play_type,
        Distance, `Yards to Goal`,
        Clock, Quarter = period, `Score Diff` = score_diff,
        EPA, `WP Before` = wp_before, `WP After` = wp_after, `WP +`,
        Description = play_text
      )
    
    
    # --- build & style the DT ---
    dt <- datatable(
      raw,
      rownames = FALSE,
      options  = list(pageLength = 10, scrollX = TRUE)
    ) |>
      formatRound(c("EPA", "WP Before", "WP After", "WP +"), 3)
    
    dt <- style_all_team_colour(dt, assets$main_color) |>
      htmlwidgets::onRender(
        sprintf(
          "function(el){ $(el).closest('.datatables').css({
                 'border':'3px solid %s',
                 'border-radius':'6px',
                 'padding':'4px'}); }",
          assets$main_color
        )
      )
    
    dt
  })
  
  
  
  
  
}


shinyApp(ui = ui, server = server)



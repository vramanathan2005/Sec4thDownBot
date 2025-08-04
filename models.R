library(cfbfastR)
library(dplyr)
library(purrr)
library(tidymodels)
library(themis)
library(stringr)


# -------------------------
# Load full PBP once (2019–2024)
# -------------------------
pbp_all <- bind_rows(lapply(2019:2024, load_cfb_pbp))
# Just filter once globally
pbp_4th_all <- pbp_all %>% 
  filter(down == 4)

# -------------------------
# SEC Playcaller plan
# -------------------------
sec_playcaller_plan <- tibble::tibble(
  Team = c(
    "Alabama", "Arkansas", "Auburn", "Florida", "Georgia",
    "Kentucky", "LSU", "Mississippi State", "Missouri", "Ole Miss",
    "Oklahoma", "South Carolina", "Tennessee", "Texas", "Texas A&M", "Vanderbilt"
  ),
  Schools = list(
    c("Fresno State", "Washington", "Alabama"),
    c("Missouri State", "Texas A&M", "Arkansas"),
    c("Liberty", "Auburn"),
    c("Louisiana", "Florida"),
    c("Colorado State", "South Carolina", "Auburn", "Georgia"),
    c("Washington", "Missouri", "Boise State", "Kentucky"),
    c("Louisiana Tech", "LSU"),
    c("UCF", "Ole Miss", "Oklahoma", "Mississippi State"),
    c("Fresno State", "Missouri"),
    c("Florida Atlantic", "Ole Miss"),
    c("Houston Baptist", "Western Kentucky", "Washington State"),
    c("Oklahoma", "South Carolina"),
    c("UCF", "Tennessee"),
    c("Alabama", "Texas"),
    c("Kansas State", "Texas A&M"),
    c("Pittsburg State", "TCU", "New Mexico State", "Vanderbilt")
  ),
  Years = list(
    list(2019:2021, 2022:2023, 2024),
    list(2020:2022, 2023, 2024),
    list(2019:2022, 2023:2024),
    list(2019:2021, 2022:2024),
    list(2019, 2020, 2021, 2022:2024),
    list(2019, 2020:2022, 2023, 2024),
    list(2019:2021, 2022:2024),
    list(2019, 2020:2021, 2022:2023, 2024),
    list(2019:2022, 2023:2024),
    list(2019, 2020:2024),
    list(2019, 2021:2022, 2023:2024),
    list(2019:2020, 2021:2024),
    list(2019:2020, 2021:2024),
    list(2019:2020, 2021:2024),
    list(2019:2023, 2024),
    list(2019, 2021, 2022:2023, 2024)
  )
)

# -------------------------
# Team-specific 4th down PBP
# -------------------------
sec_coach_datasets_4th <- sec_playcaller_plan %>%
  mutate(
    Playcaller_Data_4th = purrr::pmap(
      list(Schools, Years),
      function(schools, years) {
        purrr::map2_dfr(
          schools, years,
          function(school, year_range) {
            pbp_4th_all %>%
              filter(pos_team == school, season %in% year_range) %>%
              mutate(Source_School = school)
          }
        )
      }
    )
  )

# -------------------------
# Extract each one to its own object
# -------------------------
alabama_df <- sec_coach_datasets_4th %>% filter(Team == "Alabama") %>% pull(Playcaller_Data_4th) %>% .[[1]]
arkansas_df <- sec_coach_datasets_4th %>% filter(Team == "Arkansas") %>% pull(Playcaller_Data_4th) %>% .[[1]]
auburn_df <- sec_coach_datasets_4th %>% filter(Team == "Auburn") %>% pull(Playcaller_Data_4th) %>% .[[1]]
florida_df <- sec_coach_datasets_4th %>% filter(Team == "Florida") %>% pull(Playcaller_Data_4th) %>% .[[1]]
georgia_df <- sec_coach_datasets_4th %>% filter(Team == "Georgia") %>% pull(Playcaller_Data_4th) %>% .[[1]]
kentucky_df <- sec_coach_datasets_4th %>% filter(Team == "Kentucky") %>% pull(Playcaller_Data_4th) %>% .[[1]]
lsu_df <- sec_coach_datasets_4th %>% filter(Team == "LSU") %>% pull(Playcaller_Data_4th) %>% .[[1]]
mississippi_state_df <- sec_coach_datasets_4th %>% filter(Team == "Mississippi State") %>% pull(Playcaller_Data_4th) %>% .[[1]]
missouri_df <- sec_coach_datasets_4th %>% filter(Team == "Missouri") %>% pull(Playcaller_Data_4th) %>% .[[1]]
ole_miss_df <- sec_coach_datasets_4th %>% filter(Team == "Ole Miss") %>% pull(Playcaller_Data_4th) %>% .[[1]]
oklahoma_df <- sec_coach_datasets_4th %>% filter(Team == "Oklahoma") %>% pull(Playcaller_Data_4th) %>% .[[1]]
south_carolina_df <- sec_coach_datasets_4th %>% filter(Team == "South Carolina") %>% pull(Playcaller_Data_4th) %>% .[[1]]
tennessee_df <- sec_coach_datasets_4th %>% filter(Team == "Tennessee") %>% pull(Playcaller_Data_4th) %>% .[[1]]
texas_df <- sec_coach_datasets_4th %>% filter(Team == "Texas") %>% pull(Playcaller_Data_4th) %>% .[[1]]
texas_a_m_df <- sec_coach_datasets_4th %>% filter(Team == "Texas A&M") %>% pull(Playcaller_Data_4th) %>% .[[1]]
vanderbilt_df <- sec_coach_datasets_4th %>% filter(Team == "Vanderbilt") %>% pull(Playcaller_Data_4th) %>% .[[1]]


build_coach_decision_model <- function(pbp_4th) {
  library(themis)
  library(tidymodels)
  set.seed(123)
  
  data <- pbp_4th %>%
    filter(str_detect(play_type, "Rush|Pass|Sack|Completion|Incompletion|Interception|Punt|Field Goal")) %>%
    mutate(
      fg_distance = yards_to_goal + 17,  # realistic FG distance
      seconds_left = clock.minutes * 60 + clock.seconds,
      
      decision = case_when(
        # BAD FIELD GOAL CASES
        str_detect(play_type, "Field Goal") & fg_distance > 60 ~ "Go for it",
        str_detect(play_type, "Field Goal") & score_diff <= -4 & seconds_left < 80 & offense_timeouts == 0 ~ "Go for it",
        
        # BAD PUNT CASES
        str_detect(play_type, "Punt") & score_diff <= 0 & seconds_left < 40 & offense_timeouts == 0 ~ "Go for it",
        
        # NORMAL CASES
        str_detect(play_type, "Field Goal") ~ "Field Goal",
        str_detect(play_type, "Punt") ~ "Punt",
        TRUE ~ "Go for it"
      ),
      decision = factor(decision),
      # Convert goal_to_go and under_two to numeric (0/1) for SMOTE compatibility
      goal_to_go = as.integer(distance == yards_to_goal), # Changed from factor to integer
      under_two = as.integer(clock.minutes < 2),       # Changed from factor to integer
      period = as.integer(period)
    ) %>%
    select(
      decision, distance, yards_to_goal, clock.minutes, clock.seconds,
      period, score_diff, offense_timeouts, defense_timeouts,
      goal_to_go, under_two, fg_distance
    )
  
  split <- initial_split(data, prop = 0.8, strata = decision)
  train <- training(split)
  test <- testing(split)
  
  # Define the recipe
  recipe <- 
    recipe(decision ~ ., data = train) %>%
    # Apply step_dummy BEFORE step_smote for any other nominal predictors if they exist
    step_dummy(all_nominal_predictors(), one_hot = TRUE, keep_original_cols = TRUE) %>% 
    step_zv(all_predictors()) %>%
    step_impute_median(all_numeric_predictors()) %>%
    step_normalize(all_numeric_predictors()) %>%
    step_smote(decision) # Now all predictors should be numeric or dummy variables
  
  model <- boost_tree(
    mode = "classification",
    trees = 400,
    learn_rate = 0.1,
    tree_depth = 6
  ) %>% set_engine("xgboost")
  
  wf <- workflow() %>%
    add_recipe(recipe) %>%
    add_model(model)
  
  final_model <- fit(wf, data = train)
  
  list(
    model = final_model,
    train_data = train,
    test_data = test
  )
}

global_coach_model <- build_coach_decision_model(pbp_4th_all)

# preds <- predict(global_coach_model$model, new_data = global_coach_model$test_data) %>%
#   bind_cols(
#     predict(global_coach_model$model, new_data = global_coach_model$test_data, type = "prob"),
#     global_coach_model$test_data
#   )

# # Basic accuracy and confusion matrix
# metrics(preds, truth = decision, estimate = .pred_class)
# conf_mat(preds, truth = decision, estimate = .pred_class)

# Alabama
alabama_model <- build_coach_decision_model(alabama_df)
# Arkansas
arkansas_model <- build_coach_decision_model(arkansas_df)
# Auburn
auburn_model <- build_coach_decision_model(auburn_df)
# Florida
florida_model <- build_coach_decision_model(florida_df)
# Georgia
georgia_model <- build_coach_decision_model(georgia_df)
# Kentucky
kentucky_model <- build_coach_decision_model(kentucky_df)
# LSU
lsu_model <- build_coach_decision_model(lsu_df)
# Mississippi State
mississippi_state_model <- build_coach_decision_model(mississippi_state_df)
# Missouri
missouri_model <- build_coach_decision_model(missouri_df)
# Ole Miss
ole_miss_model <- build_coach_decision_model(ole_miss_df)
# Oklahoma
oklahoma_model <- build_coach_decision_model(oklahoma_df)
# South Carolina
south_carolina_model <- build_coach_decision_model(south_carolina_df)
# Tennessee
tennessee_model <- build_coach_decision_model(tennessee_df)
# Texas
texas_model <- build_coach_decision_model(texas_df)
# Texas A&M
texas_a_m_model <- build_coach_decision_model(texas_a_m_df)
# Vanderbilt
vanderbilt_model <- build_coach_decision_model(vanderbilt_df)

# # Alabama
# alabama_preds <- predict(alabama_model$model, new_data = alabama_model$test_data) %>%
#   bind_cols(alabama_model$test_data)
# metrics(alabama_preds, truth = decision, estimate = .pred_class)
# conf_mat(alabama_preds, truth = decision, estimate = .pred_class)

# # Arkansas
# arkansas_preds <- predict(arkansas_model$model, new_data = arkansas_model$test_data) %>%
#   bind_cols(arkansas_model$test_data)
# metrics(arkansas_preds, truth = decision, estimate = .pred_class)
# conf_mat(arkansas_preds, truth = decision, estimate = .pred_class)

# # Auburn
# auburn_preds <- predict(auburn_model$model, new_data = auburn_model$test_data) %>%
#   bind_cols(auburn_model$test_data)
# metrics(auburn_preds, truth = decision, estimate = .pred_class)
# conf_mat(auburn_preds, truth = decision, estimate = .pred_class)

# # Florida
# florida_preds <- predict(florida_model$model, new_data = florida_model$test_data) %>%
#   bind_cols(florida_model$test_data)
# metrics(florida_preds, truth = decision, estimate = .pred_class)
# conf_mat(florda_preds, truth = decision, estimate = .pred_class)

# # Georgia
# georgia_preds <- predict(georgia_model$model, new_data = georgia_model$test_data) %>%
#   bind_cols(georgia_model$test_data)
# metrics(georgia_preds, truth = decision, estimate = .pred_class)
# conf_mat(georgia_preds, truth = decision, estimate = .pred_class)

# # Kentucky
# kentucky_preds <- predict(kentucky_model$model, new_data = kentucky_model$test_data) %>%
#   bind_cols(kentucky_model$test_data)
# metrics(kentucky_preds, truth = decision, estimate = .pred_class)
# conf_mat(kentucky_preds, truth = decision, estimate = .pred_class)

# # LSU
# lsu_preds <- predict(lsu_model$model, new_data = lsu_model$test_data) %>%
#   bind_cols(lsu_model$test_data)
# metrics(lsu_preds, truth = decision, estimate = .pred_class)
# conf_mat(lsu_preds, truth = decision, estimate = .pred_class)

# # Mississippi State
# mississippi_state_preds <- predict(mississippi_state_model$model, new_data = mississippi_state_model$test_data) %>%
#   bind_cols(mississippi_state_model$test_data)
# metrics(mississippi_state_preds, truth = decision, estimate = .pred_class)
# conf_mat(mississippi_state_preds, truth = decision, estimate = .pred_class)

# # Missouri
# missouri_preds <- predict(missouri_model$model, new_data = missouri_model$test_data) %>%
#   bind_cols(missouri_model$test_data)
# metrics(missouri_preds, truth = decision, estimate = .pred_class)
# conf_mat(missouri_preds, truth = decision, estimate = .pred_class)

# # Ole Miss
# ole_miss_preds <- predict(ole_miss_model$model, new_data = ole_miss_model$test_data) %>%
#   bind_cols(ole_miss_model$test_data)
# metrics(ole_miss_preds, truth = decision, estimate = .pred_class)
# conf_mat(ole_miss_preds, truth = decision, estimate = .pred_class)

# # Oklahoma
# oklahoma_preds <- predict(oklahoma_model$model, new_data = oklahoma_model$test_data) %>%
#   bind_cols(oklahoma_model$test_data)
# metrics(oklahoma_preds, truth = decision, estimate = .pred_class)
# conf_mat(oklahoma_preds, truth = decision, estimate = .pred_class)

# # South Carolina
# south_carolina_preds <- predict(south_carolina_model$model, new_data = south_carolina_model$test_data) %>%
#   bind_cols(south_carolina_model$test_data)
# metrics(south_carolina_preds, truth = decision, estimate = .pred_class)
# conf_mat(south_carolina_preds, truth = decision, estimate = .pred_class)

# # Tennessee
# tennessee_preds <- predict(tennessee_model$model, new_data = tennessee_model$test_data) %>%
#   bind_cols(tennessee_model$test_data)
# metrics(tennessee_preds, truth = decision, estimate = .pred_class)
# conf_mat(tennessee_preds, truth = decision, estimate = .pred_class)

# # Texas
# texas_preds <- predict(texas_model$model, new_data = texas_model$test_data) %>%
#   bind_cols(texas_model$test_data)
# metrics(texas_preds, truth = decision, estimate = .pred_class)
# conf_mat(texas_preds, truth = decision, estimate = .pred_class)

# # Texas A&M
# texas_a_m_preds <- predict(texas_a_m_model$model, new_data = texas_a_m_model$test_data) %>%
#   bind_cols(texas_a_m_model$test_data)
# metrics(texas_a_m_preds, truth = decision, estimate = .pred_class)
# conf_mat(texas_a_m_preds, truth = decision, estimate = .pred_class)

# # Vanderbilt
# vanderbilt_preds <- predict(vanderbilt_model$model, new_data = vanderbilt_model$test_data) %>%
#   bind_cols(vanderbilt_model$test_data)
# metrics(vanderbilt_preds, truth = decision, estimate = .pred_class)
# conf_mat(vanderbilt_preds, truth = decision, estimate = .pred_class)



# Alabama
alabama_preds <- predict(alabama_model$model, new_data = alabama_model$test_data) %>%
  bind_cols(alabama_model$test_data)
metrics(alabama_preds, truth = decision, estimate = .pred_class)
conf_mat(alabama_preds, truth = decision, estimate = .pred_class)

# Arkansas
arkansas_preds <- predict(arkansas_model$model, new_data = arkansas_model$test_data) %>%
  bind_cols(arkansas_model$test_data)
metrics(arkansas_preds, truth = decision, estimate = .pred_class)
conf_mat(arkansas_preds, truth = decision, estimate = .pred_class)

# Auburn
auburn_preds <- predict(auburn_model$model, new_data = auburn_model$test_data) %>%
  bind_cols(auburn_model$test_data)
metrics(auburn_preds, truth = decision, estimate = .pred_class)
conf_mat(auburn_preds, truth = decision, estimate = .pred_class)

# Florida
florida_preds <- predict(florida_model$model, new_data = florida_model$test_data) %>%
  bind_cols(florida_model$test_data)
metrics(florida_preds, truth = decision, estimate = .pred_class)
conf_mat(florida_preds, truth = decision, estimate = .pred_class)

# Georgia
georgia_preds <- predict(georgia_model$model, new_data = georgia_model$test_data) %>%
  bind_cols(georgia_model$test_data)
metrics(georgia_preds, truth = decision, estimate = .pred_class)
conf_mat(georgia_preds, truth = decision, estimate = .pred_class)

# Kentucky
kentucky_preds <- predict(kentucky_model$model, new_data = kentucky_model$test_data) %>%
  bind_cols(kentucky_model$test_data)
metrics(kentucky_preds, truth = decision, estimate = .pred_class)
conf_mat(kentucky_preds, truth = decision, estimate = .pred_class)

# LSU
lsu_preds <- predict(lsu_model$model, new_data = lsu_model$test_data) %>%
  bind_cols(lsu_model$test_data)
metrics(lsu_preds, truth = decision, estimate = .pred_class)
conf_mat(lsu_preds, truth = decision, estimate = .pred_class)

# Mississippi State
mississippi_state_preds <- predict(mississippi_state_model$model, new_data = mississippi_state_model$test_data) %>%
  bind_cols(mississippi_state_model$test_data)
metrics(mississippi_state_preds, truth = decision, estimate = .pred_class)
conf_mat(mississippi_state_preds, truth = decision, estimate = .pred_class)

# Missouri
missouri_preds <- predict(missouri_model$model, new_data = missouri_model$test_data) %>%
  bind_cols(missouri_model$test_data)
metrics(missouri_preds, truth = decision, estimate = .pred_class)
conf_mat(missouri_preds, truth = decision, estimate = .pred_class)

# Ole Miss
ole_miss_preds <- predict(ole_miss_model$model, new_data = ole_miss_model$test_data) %>%
  bind_cols(ole_miss_model$test_data)
metrics(ole_miss_preds, truth = decision, estimate = .pred_class)
conf_mat(ole_miss_preds, truth = decision, estimate = .pred_class)

# Oklahoma
oklahoma_preds <- predict(oklahoma_model$model, new_data = oklahoma_model$test_data) %>%
  bind_cols(oklahoma_model$test_data)
metrics(oklahoma_preds, truth = decision, estimate = .pred_class)
conf_mat(oklahoma_preds, truth = decision, estimate = .pred_class)

# South Carolina
south_carolina_preds <- predict(south_carolina_model$model, new_data = south_carolina_model$test_data) %>%
  bind_cols(south_carolina_model$test_data)
metrics(south_carolina_preds, truth = decision, estimate = .pred_class)
conf_mat(south_carolina_preds, truth = decision, estimate = .pred_class)

# Tennessee
tennessee_preds <- predict(tennessee_model$model, new_data = tennessee_model$test_data) %>%
  bind_cols(tennessee_model$test_data)
metrics(tennessee_preds, truth = decision, estimate = .pred_class)
conf_mat(tennessee_preds, truth = decision, estimate = .pred_class)

# Texas
texas_preds <- predict(texas_model$model, new_data = texas_model$test_data) %>%
  bind_cols(texas_model$test_data)
metrics(texas_preds, truth = decision, estimate = .pred_class)
conf_mat(texas_preds, truth = decision, estimate = .pred_class)

# Texas A&M
texas_a_m_preds <- predict(texas_a_m_model$model, new_data = texas_a_m_model$test_data) %>%
  bind_cols(texas_a_m_model$test_data)
metrics(texas_a_m_preds, truth = decision, estimate = .pred_class)
conf_mat(texas_a_m_preds, truth = decision, estimate = .pred_class)

# Vanderbilt
vanderbilt_preds <- predict(vanderbilt_model$model, new_data = vanderbilt_model$test_data) %>%
  bind_cols(vanderbilt_model$test_data)
metrics(vanderbilt_preds, truth = decision, estimate = .pred_class)
conf_mat(vanderbilt_preds, truth = decision, estimate = .pred_class)

# -------------------------
# WP Model Builder
# -------------------------

build_wp_model <- function(pbp_4th_all) {
  library(tidymodels)
  set.seed(345)
  
  # Filter and prep data
  data <- pbp_4th_all %>%
    filter(
      !is.na(wp_before),
      !is.na(yards_to_goal),
      !is.na(distance),
      !is.na(clock.minutes),
      !is.na(clock.seconds),
      !is.na(score_diff)
    ) %>%
    mutate(
      period = as.integer(period),
      # Convert to integer for consistency and numeric usage
      goal_to_go = as.integer(distance == yards_to_goal), 
      under_two = as.integer(clock.minutes < 2)
    ) %>%
    select(
      wp_before, yards_to_goal, distance, clock.minutes, clock.seconds,
      period, score_diff, offense_timeouts, defense_timeouts,
      goal_to_go, under_two
    )
  
  # Recipe for regression (wp_before is the outcome)
  # Use 'data' directly since you're training on the full dataset here.
  recipe <- 
    recipe(wp_before ~ ., data = data) %>% # CHANGED: data = train to data = data
    step_dummy(all_nominal_predictors(), one_hot = TRUE, keep_original_cols = TRUE) %>% 
    step_zv(all_predictors()) %>%
    step_impute_median(all_numeric_predictors()) %>%
    step_normalize(all_numeric_predictors())
  
  # Model spec
  model <- rand_forest(mode = "regression", trees = 500) %>%
    set_engine("ranger")
  
  # Workflow
  wf <- workflow() %>%
    add_recipe(recipe) %>%
    add_model(model)
  
  # Fit model
  final_model <- fit(wf, data = data)
  
  list(
    model = final_model,
    data = data  # Keep raw for level checks if needed
  )
}

# -------------------------
# Train WP model on global 4th down plays
# -------------------------

# -------------------------
# Train WP model on global 4th‑down plays
# -------------------------
wp_model <- build_wp_model(pbp_4th_all)

# --- optional: compute RMSE before slimming ---
wp_preds  <- predict(wp_model$model, new_data = wp_model$data) |>
             bind_cols(wp_model$data)
rmse_val  <- metrics(wp_preds, truth = wp_before, estimate = .pred)
print(rmse_val)

simulate_wp_options <- function(situation, wp_model) {
  # Extract
  current_yards_to_goal <- situation$yards_to_goal
  current_distance <- situation$distance
  current_clock_minutes <- situation$clock.minutes
  current_clock_seconds <- situation$clock.seconds
  current_period <- situation$period
  current_score_diff <- situation$score_diff
  current_offense_timeouts <- situation$offense_timeouts
  current_defense_timeouts <- situation$defense_timeouts
  
  # Constants
  avg_play_time <- 5
  avg_punt_net <- 40
  kickoff_touchback <- 75  # opponent's yards to goal after touchback (own 25)
  
  # Clock helper
  calculate_new_clock <- function(minutes, seconds, play_time) {
    total_sec <- minutes * 60 + seconds - play_time
    new_minutes <- max(floor(total_sec / 60), 0)
    new_seconds <- max(total_sec %% 60, 0)
    list(minutes = new_minutes, seconds = new_seconds)
  }
  
  # --- GO FOR IT ---
  go_clock <- calculate_new_clock(current_clock_minutes, current_clock_seconds, avg_play_time)
  
  # Did we score a TD?
  converted_yards_to_goal <- current_yards_to_goal - current_distance
  made_TD <- converted_yards_to_goal <= 0
  
  if (made_TD) {
    # TD: add 7 points, kickoff to opponent
    go_convert_data <- tibble(
      yards_to_goal = kickoff_touchback,
      distance = 10,
      clock.minutes = go_clock$minutes,
      clock.seconds = go_clock$seconds,
      period = current_period,
      score_diff = current_score_diff + 7,
      offense_timeouts = current_defense_timeouts,
      defense_timeouts = current_offense_timeouts,
      goal_to_go = as.integer(FALSE), # Changed to as.integer
      under_two = as.integer(go_clock$minutes < 2) # Changed to as.integer
    )
  } else {
    # Converted, not TD: new 1st & 10
    new_ytg <- converted_yards_to_goal
    new_dist <- ifelse(new_ytg <= 10, new_ytg, 10)
    
    go_convert_data <- tibble(
      yards_to_goal = new_ytg,
      distance = new_dist,
      clock.minutes = go_clock$minutes,
      clock.seconds = go_clock$seconds,
      period = current_period,
      score_diff = current_score_diff,
      offense_timeouts = current_offense_timeouts,
      defense_timeouts = current_defense_timeouts,
      goal_to_go = as.integer(new_dist == new_ytg), # Changed to as.integer
      under_two = as.integer(go_clock$minutes < 2)   # Changed to as.integer
    )
  }
  
  # Fail: opponent takes over at line of scrimmage
  go_fail_data <- tibble(
    yards_to_goal = 100 - current_yards_to_goal,
    distance = 10,
    clock.minutes = go_clock$minutes,
    clock.seconds = go_clock$seconds,
    period = current_period,
    score_diff = current_score_diff,  # Keep same, they just get the ball
    offense_timeouts = current_defense_timeouts,
    defense_timeouts = current_offense_timeouts,
    goal_to_go = as.integer(FALSE), # Changed to as.integer
    under_two = as.integer(go_clock$minutes < 2) # Changed to as.integer
  )
  
  # --- FIELD GOAL ---
  fg_clock <- calculate_new_clock(current_clock_minutes, current_clock_seconds, avg_play_time)
  fg_make_possible <- current_yards_to_goal <= 42  # only if realistic
  
  if (fg_make_possible) {
    fg_make_data <- tibble(
      yards_to_goal = kickoff_touchback,
      distance = 10,
      clock.minutes = fg_clock$minutes,
      clock.seconds = fg_clock$seconds,
      period = current_period,
      score_diff = current_score_diff + 3,
      offense_timeouts = current_defense_timeouts,
      defense_timeouts = current_offense_timeouts,
      goal_to_go = as.integer(FALSE), # Changed to as.integer
      under_two = as.integer(fg_clock$minutes < 2) # Changed to as.integer
    )
    
    snap_distance <- 7  
    
    miss_spot <- max(current_yards_to_goal, current_yards_to_goal + snap_distance)
    
    fg_miss_data <- tibble(
      yards_to_goal = 100 - miss_spot,
      distance = 10,
      clock.minutes = fg_clock$minutes,
      clock.seconds = fg_clock$seconds,
      period = current_period,
      score_diff = current_score_diff,
      offense_timeouts = current_defense_timeouts,
      defense_timeouts = current_offense_timeouts,
      goal_to_go = as.integer(FALSE), # Changed to as.integer
      under_two = as.integer(fg_clock$minutes < 2) # Changed to as.integer
    )
  } else {
    fg_make_data <- NULL
    fg_miss_data <- NULL
  }
  
  # --- PUNT ---
  punt_clock <- calculate_new_clock(current_clock_minutes, current_clock_seconds, avg_play_time)
  punt_yard_line <- max(1, current_yards_to_goal - avg_punt_net)
  
  punt_data <- tibble(
    yards_to_goal = 100 - punt_yard_line,
    distance = 10,
    clock.minutes = punt_clock$minutes,
    clock.seconds = punt_clock$seconds,
    period = current_period,
    score_diff = current_score_diff,
    offense_timeouts = current_defense_timeouts,
    defense_timeouts = current_offense_timeouts,
    goal_to_go = as.integer(FALSE), # Changed to as.integer
    under_two = as.integer(punt_clock$minutes < 2) # Changed to as.integer
  )
  
  # --- Predict ---
  # Ensure the input 'situation' itself is also converted correctly for prediction
  situation_numeric <- situation %>%
    mutate(
      goal_to_go = as.integer(goal_to_go),
      under_two = as.integer(under_two)
    )
  
  wp_go_convert <- predict(wp_model$model, new_data = go_convert_data)$.pred
  wp_go_fail <- predict(wp_model$model, new_data = go_fail_data)$.pred
  
  wp_fg_make <- NA
  wp_fg_miss <- NA
  if (fg_make_possible) {
    wp_fg_make <- predict(wp_model$model, new_data = fg_make_data)$.pred
    wp_fg_miss <- predict(wp_model$model, new_data = fg_miss_data)$.pred
  }
  
  wp_punt <- predict(wp_model$model, new_data = punt_data)$.pred
  
  # Get current WP
  current_wp <- predict(wp_model$model, new_data = situation_numeric)$.pred # Use situation_numeric here
  
  # Define outcomes and their estimated WPs
  options <- c("Go for it (Convert)", "Go for it (Fail)", "Field Goal (Make)", "Field Goal (Miss)", "Punt")
  wps <- c(wp_go_convert, wp_go_fail, wp_fg_make, wp_fg_miss, wp_punt)
  wp_gain <- round(wps - current_wp, 3)
  
  # Best option = highest WP gain
  valid_wps <- ifelse(is.na(wp_gain), -99, wp_gain)
  best_idx <- which.max(valid_wps)
  best_flags <- seq_along(wp_gain) == best_idx
  
  # Return table
  # current_wp already calculated above using situation_numeric
  
  tibble(
    Option = options,
    Current_WP = round(current_wp, 3),
    Estimated_WP_Gain = round(wps - current_wp, 3),
    Best = best_flags
  )
}




# -------------------------
# Check WP model accuracy
# -------------------------

build_play_similarity <- function(team_df) {
  
  data <- team_df %>% 
    filter(!str_detect(play_type, "Penalty")) %>% 
    drop_na(distance, yards_to_goal, clock.minutes, clock.seconds,
            period, score_diff, offense_timeouts, defense_timeouts) %>% 
    mutate(
      goal_to_go = as.integer(distance == yards_to_goal),
      under_two  = as.integer(clock.minutes < 2),
      period     = as.integer(period)
    ) %>% 
    select(                            # <- include these 3 new columns
      year, week, pos_team, def_pos_team,    # NEW
      distance, yards_to_goal, clock.minutes, clock.seconds,
      period, score_diff, offense_timeouts, defense_timeouts,
      goal_to_go, under_two, play_type, EPA, wp_before, wp_after,
      play_text
    )
  
  matrix <- as.matrix(data %>% select(
    distance, yards_to_goal, clock.minutes, clock.seconds,
    period, score_diff, offense_timeouts, defense_timeouts,
    goal_to_go, under_two
  ))
  
  list(matrix = matrix, raw = data)
}


global_sim <- build_play_similarity(pbp_4th_all)
alabama_sim <- build_play_similarity(alabama_df)
arkansas_sim <- build_play_similarity(arkansas_df)
auburn_sim <- build_play_similarity(auburn_df)
florida_sim <- build_play_similarity(florida_df)
georgia_sim <- build_play_similarity(georgia_df)
kentucky_sim <- build_play_similarity(kentucky_df)
lsu_sim <- build_play_similarity(lsu_df)
mississippi_state_sim <- build_play_similarity(mississippi_state_df)
missouri_sim <- build_play_similarity(missouri_df)
ole_miss_sim <- build_play_similarity(ole_miss_df)
oklahoma_sim <- build_play_similarity(oklahoma_df)
south_carolina_sim <- build_play_similarity(south_carolina_df)
tennessee_sim <- build_play_similarity(tennessee_df)
texas_sim <- build_play_similarity(texas_df)
texas_a_m_sim <- build_play_similarity(texas_a_m_df)
vanderbilt_sim <- build_play_similarity(vanderbilt_df)

library(FNN)

get_similar_plays <- function(sim_data, input_situation) {
  # Create 1-row matrix from input
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
  
  # Get 5 nearest neighbors
  nn <- get.knnx(sim_data$matrix, query, k = 5)
  
  # Pull raw play info
  sim_data$raw[nn$nn.index[1, ], c("distance", "yards_to_goal", "clock.minutes", "clock.seconds", 
                                   "period", "score_diff", "play_type", "EPA", "wp_before", "wp_after", "play_text")]
}


# ----------------------------
# 1.  DATA FRAMES ------------
# ----------------------------
save_team_dataframes <- function(dir_path = "data") {
  dir.create(dir_path, showWarnings = FALSE)
  
  teams <- c("alabama","arkansas","auburn","florida","georgia",
             "kentucky","lsu","mississippi_state","missouri","ole_miss",
             "oklahoma","south_carolina","tennessee","texas","texas_a_m","vanderbilt")
  
  for (team in teams) {
    df_obj <- get(paste0(team, "_df"), inherits = TRUE)
    saveRDS(df_obj, file = file.path(dir_path, paste0(team, "_df.rds")))
  }
}

# ----------------------------
# 2.  TEAM COACH MODELS ------
# ----------------------------
save_team_coach_models <- function(dir_path = "models") {
  dir.create(dir_path, showWarnings = FALSE)
  
  teams <- c("alabama","arkansas","auburn","florida","georgia",
             "kentucky","lsu","mississippi_state","missouri","ole_miss",
             "oklahoma","south_carolina","tennessee","texas","texas_a_m","vanderbilt")
  
  for (team in teams) {
    model_obj <- get(paste0(team, "_model"), inherits = TRUE)
    saveRDS(model_obj, file = file.path(dir_path, paste0(team, "_model.rds")))
  }
}

# ----------------------------
# 3.  GLOBAL MODELS ----------
# ----------------------------
save_global_models <- function(dir_path = "models") {
  dir.create(dir_path, recursive = TRUE, showWarnings = FALSE)
  
  saveRDS(global_coach_model,
          file = file.path(dir_path, "global_coach_model.rds"))
  
  # slim and save WP model
  wp_model$model <- butcher::butcher(wp_model$model, add_class = TRUE)
  saveRDS(wp_model,
          file = file.path(dir_path, "wp_model.rds"),
          compress = "xz")
}


# ----------------------------
# 4.  MASTER WRAPPER ---------
# ----------------------------
save_all_sec_artifacts <- function() {
  save_team_dataframes()       # -> data/*.rds
  save_team_coach_models()     # -> models/*_model.rds
  save_global_models()         # -> models/global_*.rds
}

# --- Run once ---
save_all_sec_artifacts()

save_play_similarity_objects <- function(dir_path = "similarity") {
  dir.create(dir_path, showWarnings = FALSE)
  
  sims <- c("global","alabama","arkansas","auburn","florida","georgia",
            "kentucky","lsu","mississippi_state","missouri","ole_miss",
            "oklahoma","south_carolina","tennessee","texas","texas_a_m","vanderbilt")
  
  for (s in sims) {
    sim_obj <- get(paste0(s, "_sim"), inherits = TRUE)
    saveRDS(sim_obj, file = file.path(dir_path, paste0(s, "_sim.rds")))
  }
}

save_play_similarity_objects()


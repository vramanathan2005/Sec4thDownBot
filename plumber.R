library(plumber)
library(tidymodels)
library(memoise)

pretty_names <- c(
  alabama  = "Alabama", arkansas = "Arkansas", auburn = "Auburn",
  florida  = "Florida", georgia  = "Georgia",  kentucky = "Kentucky",
  lsu      = "LSU",     mississippi_state = "Mississippi State",
  missouri = "Missouri", ole_miss = "Ole Miss", oklahoma = "Oklahoma",
  south_carolina = "South Carolina", tennessee = "Tennessee",
  texas = "Texas", texas_a_m = "Texas A&M", vanderbilt = "Vanderbilt"
)

# -- 1. lazy-load and cache a model the first time it’s needed ----------
load_model <- memoise(function(team_slug) {
  path <- file.path("models", paste0(team_slug, "_model.rds"))
  readRDS(path)
})

# -- 2. helper: map nice name → slug ------------------------------------
slug_of <- function(team_name) {
  names(pretty_names)[pretty_names == team_name]
}

#* @apiTitle SEC Coach-Decision API

#* Predict coach decision for a given team
#* @post /predict_decision
#* @param team:string   (e.g. "Texas")
#* @param distance:numeric
#* @param yards_to_goal:numeric
#* @param clock_minutes:numeric
#* @param clock_seconds:numeric
#* @param period:int
#* @param score_diff:numeric
#* @param offense_timeouts:int
#* @param defense_timeouts:int
function(team, distance, yards_to_goal,
         clock_minutes, clock_seconds,
         period, score_diff,
         offense_timeouts, defense_timeouts) {
  
  # --- validate & map team ---------------------------------------------
  slug <- slug_of(team)
  if (is.na(slug)) {
    return(list(error = "Unknown team name"))
  }
  
  model_obj <- load_model(slug)      # memoised – loads once
  
  # --- build feature frame ---------------------------------------------
  newdata <- tibble::tibble(
    distance         = as.numeric(distance),
    yards_to_goal    = as.numeric(yards_to_goal),
    clock.minutes    = as.numeric(clock_minutes),
    clock.seconds    = as.numeric(clock_seconds),
    period           = as.integer(period),
    score_diff       = as.numeric(score_diff),
    offense_timeouts = as.integer(offense_timeouts),
    defense_timeouts = as.integer(defense_timeouts),
    fg_distance      = yards_to_goal + 17,
    goal_to_go       = as.integer(distance == yards_to_goal),
    under_two        = as.integer(clock_minutes < 2)
  )
  
  # --- predict ----------------------------------------------------------
  decision <- predict(model_obj$model, new_data = newdata)$.pred_class[1]
  
  list(team = team, coach_decision = decision)
}

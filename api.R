#* @apiTitle Win-probability endpoint
#* @apiDescription Returns WP for one 4th-down situation

library(plumber)
library(readr)        # for read_rds
library(tibble)
library(jsonlite)
library(xgboost)
library(glue)

# lazy-load model once per container
load_wp <- local({
  model <- NULL
  function() {
    if (is.null(model)) {
      url <- "https://storage.googleapis.com/sec-4th-down-bot/models/wp_model.rds"
      message("⇢ downloading model …")
      tmp <- tempfile(fileext = ".rds")
      download.file(url, tmp, mode = "wb", quiet = TRUE)
      model <<- readRDS(tmp)
    }
    model
  }
})

#* Compute win probability
#* @param distance
#* @param ytg           Distance to goal line
#* @param min           Clock minutes
#* @param sec           Clock seconds
#* @param q             Quarter (1-4)
#* @param diff          Score diff (offense − defense)
#* @param o_to          Offense timeouts
#* @param d_to          Defense timeouts
#* @post /wp
function(distance, ytg, min, sec, q, diff, o_to, d_to) {
  
  row <- tibble(
    distance         = as.numeric(distance),
    yards_to_goal    = as.numeric(ytg),
    clock.minutes    = as.numeric(min),
    clock.seconds    = as.numeric(sec),
    period           = as.integer(q),
    score_diff       = as.numeric(diff),
    offense_timeouts = as.integer(o_to),
    defense_timeouts = as.integer(d_to),
    goal_to_go       = as.integer(distance == ytg),
    under_two        = as.integer(min < 2)
  )
  
  mod <- load_wp()
  wp  <- predict(mod$model, new_data = row)[1]
  list(wp = round(wp, 4))
}
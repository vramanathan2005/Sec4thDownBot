library(plumber)

pr <- plumber::plumb("plumber.R")   #  <-- was "api.R"
pr$run(host = "0.0.0.0", port = 8080)

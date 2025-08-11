# Load plumber library
library(plumber)

# Start the API from the api.R file (contains both parking and prediction)
pr("api.R") %>% 
  pr_run(port = 8000)
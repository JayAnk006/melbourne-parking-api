# main.R - Heroku-compatible
library(plumber)

# Heroku provides PORT environment variable
port <- as.numeric(Sys.getenv("PORT", "8000"))

cat("Starting Melbourne Parking API on Heroku...\n")
cat("Port:", port, "\n")

# Start the API
pr("api.R") %>% 
  pr_run(host = "0.0.0.0", port = port)

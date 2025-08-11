FROM rstudio/plumber:latest

# Set working directory
WORKDIR /app

# Copy all files
COPY . .

# Install additional packages
RUN R -e "install.packages(c('httr', 'dplyr', 'jsonlite'), repos='https://cran.rstudio.com/')"

# Expose port
EXPOSE $PORT

# Start the application - simplified
CMD ["R", "-e", "library(plumber); pr('api.R') %>% pr_run(host='0.0.0.0', port=as.numeric(Sys.getenv('PORT', '8000')))"]

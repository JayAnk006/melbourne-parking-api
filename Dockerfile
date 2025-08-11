FROM rstudio/plumber:latest

# Set working directory
WORKDIR /app

# Copy all files
COPY . .

# Install additional packages
RUN R -e "install.packages(c('httr', 'dplyr', 'jsonlite'), repos='https://cran.rstudio.com/')"

# Expose port
EXPOSE $PORT

# Use our main.R file which is already configured for cloud deployment
CMD ["R", "-e", "source('main.R')"]

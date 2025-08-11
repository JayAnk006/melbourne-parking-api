FROM rstudio/plumber:latest

# Set working directory
WORKDIR /app

# Copy all files
COPY . .

# Install additional packages
RUN R -e "install.packages(c('httr', 'dplyr', 'jsonlite'), repos='https://cran.rstudio.com/')"

# Completely override the plumber image's behavior
ENTRYPOINT []
CMD []

# Expose port
EXPOSE $PORT

# Run our main.R file directly
CMD ["R", "--slave", "-f", "main.R"]

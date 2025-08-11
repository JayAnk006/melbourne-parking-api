FROM rstudio/plumber:latest

# Set working directory
WORKDIR /app

# Copy all files
COPY . .

# Install additional packages
RUN R -e "install.packages(c('httr', 'dplyr', 'jsonlite'), repos='https://cran.rstudio.com/')"

# Override the default plumber entrypoint
ENTRYPOINT []

# Expose port
EXPOSE $PORT

# Use our main.R file directly
CMD ["Rscript", "main.R"]

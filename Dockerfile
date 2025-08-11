FROM rocker/r-ver:4.2.0

# Install system dependencies
RUN apt-get update && apt-get install -y \
    libcurl4-openssl-dev \
    libssl-dev \
    libxml2-dev \
    libfontconfig1-dev \
    libfreetype6-dev \
    libpng-dev \
    libtiff5-dev \
    libjpeg-dev \
    && rm -rf /var/lib/apt/lists/*

# Set working directory
WORKDIR /app

# Copy files
COPY *.R ./
COPY *.csv ./

# Install R packages with explicit CRAN mirror
RUN R -e "options(repos = c(CRAN = 'https://cran.rstudio.com/')); install.packages(c('plumber', 'httr', 'dplyr', 'jsonlite'), dependencies = TRUE)"

# Expose port
EXPOSE $PORT

# Start the application
CMD ["R", "-e", "source('main.R')"]

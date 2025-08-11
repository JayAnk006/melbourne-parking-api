FROM rocker/r-ver:4.2.0

RUN apt-get update && apt-get install -y \
    libcurl4-openssl-dev \
    libssl-dev \
    libxml2-dev \
    && rm -rf /var/lib/apt/lists/*

WORKDIR /app

COPY *.R ./
COPY *.csv ./

RUN R -e "install.packages(c('plumber', 'httr', 'dplyr', 'jsonlite'), repos='https://cran.rstudio.com/')"

EXPOSE $PORT

RUN echo '#!/bin/bash\nR -e "library(plumber); pr(\"api.R\") %>% pr_run(host=\"0.0.0.0\", port=as.numeric(Sys.getenv(\"PORT\", \"8000\")))"' > start.sh
RUN chmod +x start.sh

CMD ["./start.sh"]
FROM rocker/shiny:4.3

WORKDIR /app

# System dependencies for common R packages
RUN apt-get update && apt-get install -y --no-install-recommends \
    libcurl4-openssl-dev \
    libssl-dev \
    libxml2-dev \
    && rm -rf /var/lib/apt/lists/*

# Install renv and restore packages
COPY renv.lock renv.lock
COPY .Rprofile .Rprofile
COPY renv/activate.R renv/activate.R
RUN R -e "renv::restore()"

# Copy app code
COPY . .

# Install the golem app as a package
RUN R -e "remotes::install_local('.', dependencies = FALSE)"

ENV PORT=8080
EXPOSE 8080

# Run via golem's run_app() — package name is read from DESCRIPTION
CMD ["R", "-e", "options(shiny.port = as.integer(Sys.getenv('PORT', 8080)), shiny.host = '0.0.0.0'); pkgload::load_all(); run_app()"]

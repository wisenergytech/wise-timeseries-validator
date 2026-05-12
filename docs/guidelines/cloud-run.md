# Google Cloud Run (R Shiny) — Conventions Wise

## Image

- Base image: `rocker/shiny:4.3`
- Install system dependencies + R packages via `renv::restore()`.
- Final image runs `R -e "shiny::runApp('app.R', host='0.0.0.0', port=8080)"`.

## Dockerfile Pattern

```dockerfile
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

ENV PORT=8080
EXPOSE 8080

CMD ["R", "-e", "shiny::runApp('app.R', host='0.0.0.0', port=as.integer(Sys.getenv('PORT', 8080)))"]
```

## Deployment

- Deploy via `gcloud run deploy` or Cloud Build.
- Region: `europe-west1` (default Wise).
- Allow unauthenticated invocations (auth is handled by Supabase at the app level).
- Set minimum instances to 0 (scale to zero for cost efficiency on POCs).

## Environment Variables

Inject the same variables as defined in `.env.example`:
- `SUPABASE_URL`, `SUPABASE_KEY`, `SUPABASE_SERVICE_ROLE_KEY`
- Project-specific API credentials (e.g., `WISE_API_CLIENT_ID`, `WISE_API_CLIENT_SECRET`)

Never bake secrets into the Docker image at build time.

## Health Check

Shiny responds on the root `/` route by default. Configure Cloud Run to use this endpoint for health checks.

## Logs

- R logs go to stdout → Cloud Run forwards to Cloud Logging automatically.
- Use `message()` for structured output with contextual prefixes (e.g., `[api]`, `[auth]`).

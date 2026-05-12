# Supabase Auth (R Shiny) — Conventions Wise

## Architecture

Authentication uses two layers, matching the Nuxt/Streamlit pattern:

1. **Client-side (browser)**: Login form rendered by Shiny UI. The user submits credentials, and Shiny calls Supabase Auth REST API via `httr2` to obtain a session token. The token is stored in `session$userData`.
2. **Server-side (Shiny process)**: All API calls and data access verify the JWT token using the Supabase **service role key** before processing requests. The service role key MUST NEVER be exposed to the browser.

## Dependencies

```
httr2
jsonlite
```

## Auth Module (`R/mod_auth.R`)

The auth module provides:
- `auth_login(email, password)` — calls Supabase Auth REST API
- `auth_verify(token)` — verifies JWT via Supabase `GET /auth/v1/user`
- `auth_guard(session)` — checks session for valid token, redirects to login if missing

### Login via Supabase REST API

```r
auth_login <- function(email, password) {
  url <- Sys.getenv("SUPABASE_URL")
  key <- Sys.getenv("SUPABASE_KEY")
  if (url == "" || key == "") return(NULL)

  resp <- httr2::request(paste0(url, "/auth/v1/token?grant_type=password")) |>
    httr2::req_headers(apikey = key, `Content-Type` = "application/json") |>
    httr2::req_body_json(list(email = email, password = password)) |>
    httr2::req_perform()

  if (httr2::resp_status(resp) == 200) {
    httr2::resp_body_json(resp)
  } else {
    NULL
  }
}
```

## Auth Guard

Every Shiny app MUST check authentication at startup:

```r
# In server function:
observe({
  if (is.null(session$userData$access_token)) {
    # Show login UI instead of app content
  }
})
```

## Login UI

- Rendered as a Shiny module with email/password inputs.
- On success, stores `access_token` and `user` in `session$userData`.
- No self-registration (users are created by admin in Supabase dashboard).

## Local Development

When `SUPABASE_URL` and `SUPABASE_SERVICE_ROLE_KEY` are not set, the auth guard skips authentication entirely. This allows local development without a Supabase project.

## Security Rules (same as Nuxt/Streamlit)

- `SERVICE_ROLE_KEY` MUST NEVER be exposed to the browser or stored in client-accessible reactive values.
- JWT tokens MUST NOT appear in log output, error messages, or version-controlled files.
- API secrets MUST be supplied via environment variables or a secrets manager.
- All external API calls requiring authentication MUST include the token server-side only.

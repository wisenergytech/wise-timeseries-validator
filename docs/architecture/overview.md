# Architecture Overview

<!--
  This file is maintained by Speckit. Update when a feature impacts the global architecture.
  Use Mermaid diagrams for visual representation.
-->

## System Context

```mermaid
graph TB
    User[User / Browser]
    App[Nuxt 3 App<br/>Cloud Run]
    Auth[Supabase Auth]
    API[External API]

    User -->|HTTPS| App
    App -->|JWT verify| Auth
    App -->|OAuth2 / REST| API
```

## Deployment

```mermaid
graph LR
    subgraph Google Cloud
        CR[Cloud Run<br/>Node.js 20 Alpine]
    end
    subgraph Supabase
        SB[Supabase Auth<br/>PostgreSQL]
    end
    subgraph External
        EXT[External API]
    end

    CR -->|SERVICE_ROLE_KEY| SB
    CR -->|Bearer token| EXT
```

## Key Components

| Component | Location | Responsibility |
|---|---|---|
| Client middleware | `middleware/auth.global.ts` | Redirect to login if unauthenticated |
| Server middleware | `server/middleware/auth.ts` | Verify JWT on API routes |
| API client | `server/utils/<api>-client.ts` | Centralized external API calls with auth + retry |
| Token manager | `server/utils/tokenManager.ts` | In-memory token cache with auto-refresh |

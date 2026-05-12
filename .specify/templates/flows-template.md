# Flow Diagrams: [FEATURE NAME]

**Feature Branch**: `[###-feature-name]`
**Created**: [DATE]

## Primary Flow

<!--
  Mermaid sequence diagram showing the main happy path for this feature.
  Include all actors: User, Browser (Vue/Nuxt client), Server (Nitro),
  external APIs, auth providers.
-->

```mermaid
sequenceDiagram
    actor User
    participant Browser
    participant Server as Nuxt Server
    participant Auth as [Auth Provider]
    participant API as [External API]

    User->>Browser: [action]
    Browser->>Server: [internal API call]
    Server->>Auth: [auth step if needed]
    Auth-->>Server: [token/response]
    Server->>API: [external call]
    API-->>Server: [response]
    Server-->>Browser: [processed data]
    Browser-->>User: [rendered result]
```

## Error / Retry Flow

<!--
  Show what happens on auth failure, API error, network timeout, etc.
  Only include if the feature has non-trivial error handling.
-->

```mermaid
sequenceDiagram
    [error flow diagram]
```

## State Diagram

<!--
  Optional: If the feature involves state transitions (e.g., connection states,
  processing stages), include a state diagram.
  Remove this section if not applicable.
-->

```mermaid
stateDiagram-v2
    [state diagram]
```

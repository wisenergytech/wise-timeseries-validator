# Business Logic: [FEATURE NAME]

**Feature Branch**: `[###-feature-name]`
**Created**: [DATE]

## Domain Rules

<!--
  List the business rules specific to this feature.
  Each rule should be independently verifiable.
  Use MUST/MUST NOT/SHOULD/MAY per RFC 2119.
-->

### BL-001: [Rule Name]

[Description of the business rule]

- **Input**: [What triggers this rule]
- **Expected behavior**: [What MUST happen]
- **Edge cases**: [Boundary conditions]

### BL-002: [Rule Name]

[Description]

## Data Constraints

<!--
  Validation rules, allowed values, formats, ranges.
-->

| Field | Type | Constraint | Example |
|---|---|---|---|
| [field] | [type] | [constraint] | [example] |

## Invariants

<!--
  Conditions that MUST always be true throughout the feature lifecycle.
-->

- [Invariant 1]
- [Invariant 2]

## External Dependencies

<!--
  Business rules imposed by external systems (APIs, regulations, contracts).
-->

- **[System/API]**: [Rule imposed by external dependency]

# Méthode de travail — Product Discovery

> Note de processus décrivant comment on passe de besoins métiers bruts à des features Speckit implémentables. Lecture recommandée pour toute personne qui rejoint le projet ou veut comprendre la logique des artefacts produits.
>
> Ce document décrit le pipeline **pré-Speckit** — les étapes qui précèdent `/speckit-specify`. Pour les features simples ou bien comprises, on peut sauter directement à Speckit. Ce workflow est recommandé quand les besoins métiers ne sont pas encore formalisés et que plusieurs alternatives techniques doivent être évaluées.

## Pourquoi cette approche — Product Discovery

Le pipeline en 6 étapes est une démarche de **Product Discovery** : un processus structuré pour **réduire l'incertitude** avant d'investir dans le développement. L'idée centrale est simple — on ne construit rien tant qu'on n'a pas compris ce qu'on construit, pourquoi, et comment ça s'insère dans l'existant.

### Trois sources d'incertitude typiques

1. **Incertitude métier** — les besoins ne sont pas formalisés. On sait qu'on veut "quelque chose", mais pas exactement quoi, pour qui, avec quel budget, quel périmètre MVP vs V1 vs V2.
2. **Incertitude technique** — l'infrastructure existante offre plusieurs points d'ancrage, mais choisir le bon nécessite de croiser avec les contraintes métier.
3. **Incertitude d'intégration** — l'application existante a ses propres conventions, son design system, ses patterns. Insérer une nouvelle feature sans d'abord comprendre comment elle s'y greffe risque de créer un silo technique.

### Pourquoi ne pas coder directement

Une approche "on commence à coder et on verra" produit du code spéculatif, découplé des vrais besoins, et coûteux à refactoriser. La Product Discovery élimine le travail spéculatif en fixant les décisions **avant** d'écrire du code.

### Ce que ça coûte et ce que ça rapporte

**Coût** : les étapes 1 à 5 prennent typiquement 1 à 3 sessions de travail. Aucun code n'est écrit pendant cette période.

**Bénéfice** : quand on entre en implémentation, chaque feature est :
- **Justifiée** par un besoin métier explicite
- **Dimensionnée** au juste nécessaire (pas de sur-ingénierie)
- **Branchée sur l'existant** de manière documentée
- **Découpée** en phases avec critères de succès
- **Traçable** — si dans 6 mois on se demande "pourquoi on a fait ça comme ça", la réponse est dans un ADR

## Quand utiliser ce workflow

| Situation | Approche recommandée |
|---|---|
| Nouvelle feature non triviale partant de besoins métiers vagues | **Ce workflow** (6 étapes) → puis Speckit |
| Feature bien comprise, scope clair, pas d'ambiguïté technique | **Directement Speckit** (`/speckit-specify` → `/speckit-plan` → `/speckit-tasks` → `/speckit-implement`) |
| Bug fix ou petite amélioration | **PR classique** (pas de Speckit, pas de Product Discovery) |
| Choix technique isolé (ex : quelle DB utiliser ?) | **Un ADR seul** (pas besoin du pipeline complet) |

## Principe directeur

**Top-down, pas tech-first.** Chaque choix technique est justifié par un besoin métier, pas par la nouveauté ou la mode. Le raisonnement va toujours dans le sens : *besoin → contrainte → choix technique*, jamais l'inverse.

Concrètement, on s'autorise à **remettre en question** un ADR existant si un besoin métier le rend disproportionné — ou inversement, à le renforcer si un besoin le rend nécessaire.

## Pipeline en 6 étapes

Le travail de transformation des besoins métiers en code passe par 6 étapes successives, avec des points de validation explicites.

### Étape 1 — Triage et extraction

Lecture du dump brut des besoins métiers. Production d'un **tableau de triage** :

| Énoncé brut | Type | Thème | Clarté |
|---|---|---|---|
| (verbatim) | objectif business / feature / contrainte non-fonct / question / hors scope | thème métier | 🟢 clair / 🟡 ambigu / 🔴 bloquant |

À ce stade, **aucune décision, aucune reformulation**. Juste de l'organisation. Le résultat est consigné dans [business-requirements.md](./business-requirements.md).

### Étape 2 — Questions de clarification (groupées)

Les ambiguïtés et bloquants identifiés au triage donnent lieu à un **batch de questions concis et priorisé** (≤10 questions, classées bloquantes / importantes / nice-to-have). Pour minimiser les allers-retours.

Si une question reste sans réponse, on propose une **hypothèse de travail** que le métier valide ou non.

### Étape 3 — Reformulation et regroupement

Chaque besoin brut est reformulé en énoncé clair, mesurable et techniquement actionnable. Les besoins reformulés sont ensuite **regroupés en candidats features**.

Certains besoins sont **transverses** (auth, observability) — ils ne deviennent pas des features mais des "cross-cutting requirements". D'autres sont **architecturaux** — ils deviennent ou modifient des ADR (Architecture Decision Records), pas des features Speckit.

### Étape 4 — Mapping besoins → tech + révision des ADR

Pour chaque feature candidate, on fait le travail de **branchement sur l'existant** :
- Quelles tables / services existants elle s'appuie
- Quels composants de l'application existante elle étend ou crée
- Quel code existant (POCs, prototypes) est réutilisable
- Quels ADR sont **impactés** : à confirmer, amender, rejeter, ou créer

Les ADR sont **vivantes** :
- Si une ADR existante tient toujours → on la confirme
- Si elle ne tient plus → on la **supersede** par un nouvel ADR (traçabilité)
- Si un nouveau choix architectural émerge → nouvel ADR

### Étape 5 — Écriture de `docs/roadmap.md`

Document central, business-driven, structuré pour deux publics (décideurs + développeurs) :

```
docs/roadmap.md
├── 0. TL;DR exécutif (5 min de lecture)
├── 1. Besoins métiers (reformulés, par thème)
├── 2. Architecture cible (avec liens vers ADR)
├── 3. Plan de phases (MVP → V1 → V2)
├── 4. Mapping features → composants tech
├── 5. Cross-cutting requirements
├── 6. Risques et hypothèses
├── 7. Séquence des Speckit specs
└── 8. Glossaire (si nécessaire)
```

La roadmap **ne contient pas le détail tech** : elle pointe. Les ADR contiennent les décisions, les specs Speckit contiennent les features.

### Étape 6 — Création des specs business-driven (pré-Speckit)

Pour chaque feature de **Phase 1 / MVP** dont le scope est clair, on crée un squelette de spec dans le dossier **`br_driven_specs/`** (business-requirements-driven specs) :

```
br_driven_specs/                            ← specs issues du Product Discovery (lecture seule, référence)
├── 001-feature-name/
│   └── spec.md                             ← rédigé par le Product Discovery
├── 002-other-feature/
│   └── spec.md
└── ...
```

Ces specs sont des **inputs de référence**, pas des artefacts Speckit. Elles ne contiennent que le `spec.md`.

Quand on est prêt à implémenter une feature, on lance `/speckit-specify` en lui indiquant de se baser sur la spec pré-rédigée :

```
/speckit-specify "Implement feature X as described in br_driven_specs/001-feature-name/spec.md — use it as the base, enrich it in Speckit format."
```

Speckit fait alors son flow normal :
1. Crée automatiquement la branche git
2. Crée le dossier `specs/NNN-feature-name/`
3. Rédige le `spec.md` Speckit enrichi en s'appuyant sur la spec business-driven

**Deux dossiers, deux rôles** :

| Dossier | Rôle | Géré par |
|---|---|---|
| `br_driven_specs/` | Specs issues du Product Discovery — source de vérité business, lecture seule | Le workflow en 6 étapes (humain + AI) |
| `specs/` | Specs Speckit — enrichies, avec plan/tasks/code | Speckit (`/speckit-specify`, `/speckit-plan`, etc.) |

## Points de validation

Le pipeline a **3 points de validation** explicites où le travail s'arrête en attendant une décision :

| # | Après l'étape | Ce qu'on valide |
|---|---|---|
| ✋ 1 | Étape 1-2 (triage + questions) | Le découpage thématique et les réponses aux questions |
| ✋ 2 | Étape 3-4 (reformulation + mapping) | Les énoncés reformulés, le regroupement en features, le diff des ADR |
| ✋ 3 | Étape 5-6 (roadmap + squelettes) | La roadmap finale et les `spec.md` avant commit |

Entre chaque point, **rien n'est figé**. Les propositions sont d'abord discutées. Une fois validé, on commit.

## Artefacts produits

| Artefact | Rôle | Localisation |
|---|---|---|
| **`docs/business-requirements.md`** | Capture des besoins bruts + triage + hypothèses | `docs/` |
| **`docs/roadmap.md`** | Point d'entrée business-driven, lien entre besoins et tech | `docs/` |
| **ADR** (`docs/adr/ADR-NNNN-*.md`) | Décisions architecturales tracées et justifiées | `docs/adr/` |
| **BR-driven specs** (`br_driven_specs/NNN-feature/spec.md`) | Specs issues du Product Discovery — source de vérité business, lecture seule | `br_driven_specs/` |
| **Speckit specs** (`specs/NNN-feature/`) | Specs Speckit enrichies + plan + tasks + code | `specs/` (géré par Speckit) |

## Du Product Discovery à Speckit — le pont

Le Product Discovery (ce workflow) **produit** les `br_driven_specs/`. Speckit les **consomme** comme input de référence et génère ses propres artefacts dans `specs/`.

```
Product Discovery (étapes 1–6)                    Speckit (par feature)
──────────────────────────────                    ──────────────────────────
Étapes 1–5 : besoins → roadmap → ADR
Étape 6 : br_driven_specs/NNN/spec.md             ← input de référence
                                                  /speckit-specify → lit br_driven_specs/, crée specs/NNN/spec.md enrichi
                                                  /speckit-plan → plan.md + flows.md
                                                  /speckit-tasks → tasks.md
                                                  /speckit-implement → code
```

Les deux sont complémentaires, pas concurrents. Le Product Discovery **réduit l'incertitude** et produit la source de vérité business. Speckit **transforme** cette source en code.

## Ce qui est hors du périmètre de cette méthode

- **Pas de méthodologie agile formelle** (pas de sprints, pas de story points)
- **Pas de process de relecture obligatoire** par plusieurs personnes
- **Pas de couverture de test minimum** imposée — décidée au cas par cas dans chaque spec
- **Pas de SLO/SLA** définis a priori — on les introduit si un besoin métier les justifie

Cette méthode peut elle-même évoluer si les besoins du projet le demandent. Elle est un **support**, pas un dogme.

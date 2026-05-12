# Business Requirements — [Project Name]

> Note de cadrage des besoins métiers. Ce document est l'**input** du pipeline Product Discovery décrit dans [workflow.md](./workflow.md).
> Rempli aux étapes 1 et 2 du pipeline, puis mis à jour au fil des clarifications.

| | |
|---|---|
| **Statut** | Draft |
| **Date** | [DATE] |
| **Source** | [Réunion avec ..., document reçu de ..., etc.] |
| **Validé par** | [Qui a validé le triage et les hypothèses] |
| **Étape suivante** | Étape 1 — Triage |

---

## 1. Énoncé original (verbatim)

<!--
  Coller ici les besoins bruts tels que reçus, sans reformulation.
  Conserver le texte original même s'il est approximatif ou incomplet.
  La reformulation vient à l'étape 3, pas ici.
-->

## 2. Tableau de triage

<!--
  Rempli à l'étape 1 du workflow.
  Chaque ligne reprend un énoncé du texte original ci-dessus.
-->

| # | Énoncé extrait du draft | Type | Thème | Clarté |
|---|---|---|---|---|
| 1 | | | | |

**Légende** : 🟢 clair · 🟡 ambigu (clarification utile) · 🔴 bloquant (clarification nécessaire)

**Types** : objectif business / feature / contrainte non-fonctionnelle / contrainte d'archi / décision d'archi proposée / question ouverte / backlog-hors scope

## 3. Observations majeures

<!--
  Rempli à l'étape 1. Observations qui émergent du triage et qui ont
  un impact sur l'architecture ou les ADR existants.
-->

## 4. Hypothèses de travail validées

<!--
  Rempli à l'étape 2.
  Chaque question de clarification est accompagnée d'une hypothèse de travail.
  Le porteur du projet valide ou corrige chaque hypothèse.
-->

| Question | Hypothèse de travail validée |
|---|---|
| **Q1 — [sujet]** | [réponse ou hypothèse acceptée] |

## 5. Prochaines étapes

Conformément au [workflow](./workflow.md), la suite du pipeline est :

- ⏳ **Étape 3** — Reformulation des besoins et regroupement en features candidates
- ⏳ **Étape 4** — Mapping tech + révision/création des ADR
- ⏳ **Étape 5** — Rédaction de `docs/roadmap.md`
- ⏳ **Étape 6** — Création des squelettes Speckit

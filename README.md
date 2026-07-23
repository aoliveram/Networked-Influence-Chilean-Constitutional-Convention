# Collaboration Networks in a Tabula Rasa Legislature

Network formation, behavior, and legislative success in Chile's Constitutional Convention (2021–2022).
A. Olivera M. & J. Fábrega — CICS, Universidad del Desarrollo.

> **Status: work in progress.** Findings below reflect the current state of the analysis and may change. Earlier versions of this README described preliminary results (e.g., "strategic gatekeeping") that were later traced to measurement artifacts and are superseded.

## The case

The 2021–2022 Chilean Constitutional Convention offers a rare natural experiment for studying how political collaboration organizes from scratch: a majority of political newcomers, a body that dissolved upon delivering its draft, and procedural rules written by its own members. Two of those rules structure the study: every constitutional initiative required 8–16 sponsors (signing = forming a visible, dated coalition), and every norm required a two-thirds floor majority (103 of 154).

## Data

Built from the complete documentary record of the Convention, curated in the companion repository **[constitutional-proposal-tracking](https://github.com/aoliveram/constitutional-proposal-tracking)** (CPT):

- **995 constitutional initiatives** from the official platform (947 usable with 2–16 person signers, 100% dated), with harmonized signer lists.
- **1,565 genesis articles** with known outcomes against the draft (20% survival), including dated amendment histories across the 7 thematic commissions.
- **4,707 roll-call votes** of the floor (basis for first-month W-NOMINATE ideal points and dynamic IRT revealed votes).
- Curated profiles of the 154 delegates (profession, prior institutional experience, education, age, gender, district/pueblo, electoral list, commission).

## Models

- **Formation (RQ1):** conditional logit over initiative "menus"; bipartite ERGMs per commission (person × document — the real unit of analysis) estimated by MPLE with initiative-bootstrap standard errors; relational hyperevent model (RHEM) over the dated event sequence.
- **Effects on people (RQ2):** individual fixed-effects panels for position change (with parametric-bootstrap measurement-error propagation); two-way fixed-effects models of voting defection with permutation benchmarks.
- **Effects on texts (RQ3):** article-level survival models (2/3-pivot geometry, coalition heterogeneity, internal density); spatial Durbin model of individual success with exact impact decomposition.

## Current headline findings

1. **The network was predictable from pre-Convention endowments**: electoral list, ideology (first-month W-NOMINATE), and — conditional on commission — territory. Credentials did not weave it: lawyers show no homophily under any of three designs.
2. **Positions did not move** (selection, not influence — a null defended on power, instrument, and measurement-error grounds), but **behavior did travel**: defection clusters along co-sponsorship lines, beyond block mechanics, carried by newcomer peers.
3. **Articles survived through 2/3-pivot geometry** (the heterogeneity premium decays toward the pivot) **and through coalitions with internal co-signing history** — not through human capital.

## Repository map

- `docs/networked-influence-study.md` / `.pdf` — the living report (pedagogical, full tables).
- `docs/playground.md` / `.pdf` — methods explainers (RHEM, bipartite ERGM estimation, implementation plans).
- `docs/revision-critica.md` / `.pdf` — critical-review log (pending items + closed responses).
- `code/` — reproducible pipeline (numbered scripts; data live as versioned snapshots, no runtime dependence on external repositories).
- `presentations/` — slide decks.
- `results/` — figures (PDF + PNG) and model tables (CSV).

## Data source

All documentary data come from **[constitutional-proposal-tracking](https://github.com/aoliveram/constitutional-proposal-tracking)**, which reconstructs and harmonizes the Convention's initiatives, committee reports, amendment histories, and signer records.

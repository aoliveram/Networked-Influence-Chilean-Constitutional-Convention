# Networked Influence in a Tabula Rasa Legislature

**[📄 Extended Abstract (5 pages)](docs/extended-abstract.pdf)** — Full mathematical specifications, results tables, and discussion.

## Overview

The 2021–2022 Chilean Constitutional Convention offers a rare natural experiment for studying political network formation and legislative effectiveness. Delegates were largely unfamiliar with each other, including both career politicians and civically recruited citizens, established their own procedural rules, and required co-sponsorships for constitutional proposals. This *tabula rasa* legislature—where networks were not predetermined by party structure—enables us to trace how alliances form and translate into political success.

We analyze 154 delegates across 5 thematic commissions over 91 temporal periods using a three-stage modeling pipeline:

1. **Model 1** — Co-sponsorship network formation via **Valued ERGM**
2. **Model 2** — Ideological dynamics via **panel regression** with individual fixed effects
3. **Model 3** — Legislative success via **Spatial Durbin Model** (SDM)

## Findings

### 1. Strategic Gatekeeping in Co-sponsorship Networks

While political homophily is the expected baseline, delegates with prior **institutional experience** and **lawyers** exhibit *negative* homophily—they co-sponsor *less* with peers sharing these attributes. Rather than clustering densely, these "privileged" actors disperse across coalitions, consistent with strategic gatekeeping behavior where they mentor novices and maintain leadership through representational diversity.

### 2. Selection, Not Influence

Despite strong positive correlation in pooled OLS ($\beta = +0.033$, $p < 0.001$), **fixed-effects models show a null causal effect** of network exposure on ideological dynamics ($\beta = +0.0004$, $p = 0.91$). A falsification test (future networks predicting past ideology) fails, confirming that apparent network effects reflect **endogenous selection**—delegates choose co-sponsors aligned with preexisting positions—not genuine influence.

### 3. Legislative Success is Collective

Lexical retention (TF-IDF similarity between genesis and final constitutional text) is not an individual achievement but a **collective network phenomenon**. The Spatial Durbin Model dominates OLS/SAR/SEM (AIC = -383.35, 44+ points better). Spatial autocorrelation $\hat{\rho} = 0.997$ indicates that delegate success depends overwhelmingly on co-author characteristics and collective success—success "spills over" through co-authorship ties. Ideological consistency and intra-coalitional ties predict retention; cross-coalitional bridges reduce it.

## Methods Summary

| Model | Method | Dependent Variable | Key Finding |
|-------|--------|---------------------|-------------|
| **1** | Valued ERGM (Poisson reference) | Co-sponsorship counts | Negative homophily in privileged groups |
| **2** | Panel FE regression | $\Delta\theta_{i,t}$ (ideological change) | Causal effect ≈ 0; only selection |
| **3** | Spatial Durbin Model | Mean TF-IDF retention | $\rho \approx 1$; success is collective |

Ideological positions are estimated via a **Dynamic Bayesian IRT** (dynIRT) model with Random Walk prior, anchored by Teresa Marinovic (right) and Jorge Baradit (left), producing a $154 \times 91$ matrix of delegate-period ideal points.

Lexical retention is measured via **TF-IDF cosine similarity** (primary) and **Sentence-BERT embeddings** (robustness) on 236 mapped articles (125 identical + 111 similar) from commissions C1, C3, and C5.

## Repository Structure

```
Networked-Influence-Chilean-Constitutional-Convention/
├── README.md                      # This file
├── docs/
│   ├── extended-abstract.pdf      # 5-page extended abstract (LaTeX source included)
│   ├── extended-abstract.tex
│   ├── research-proposal.pdf      # Original research proposal
│   └── research-proposal.tex
├── code/
│   ├── 00-build_dynamic_networks.py     # Build co-authorship networks
│   ├── 01-model-valued-ergm.R           # Model 1: Valued ERGM
│   ├── 02-extract-emirt-temporal.R      # Align emIRT to commission time
│   ├── 03-model-network-influence.R     # Model 2: Panel regression
│   ├── 04-build-article-mapping.py      # Map articles to final draft
│   ├── 05-nlp-text-similarity.py        # TF-IDF + BERT similarity
│   ├── 06-build-integrated-dataset.py   # Merge all sources
│   ├── 07-model-spatial-durbin.R        # Model 3: SDM
│   └── 08-robustness-checks.R           # Robustness across all models
├── data/
│   ├── raw/
│   │   ├── commissions/                 # Enriched commission JSONs (C1, C3, C5, C6, C7)
│   │   ├── conventional-profiles.json   # BCN-scraped delegate profiles
│   │   └── emirt/                       # Dynamic IRT model outputs
│   └── processed/                       # Pipeline outputs (networks, metrics, panels)
└── results/
    ├── figures/
    └── tables/
```

## Running the Pipeline

The pipeline is sequential: each script consumes outputs from earlier steps.

```bash
# Model 1: Network formation
python code/00-build_dynamic_networks.py
Rscript code/01-model-valued-ergm.R

# Model 2: Ideological dynamics
Rscript code/02-extract-emirt-temporal.R
Rscript code/03-model-network-influence.R

# Model 3: Legislative success
python code/04-build-article-mapping.py
python code/05-nlp-text-similarity.py
python code/06-build-integrated-dataset.py
Rscript code/07-model-spatial-durbin.R

# Robustness
Rscript code/08-robustness-checks.R
```

### Dependencies

**Python (≥3.9):** `numpy`, `scipy`, `scikit-learn`, `pandas`, `sentence-transformers`, `openpyxl`

**R (≥4.0):** `statnet`, `ergm.count`, `plm`, `lmtest`, `sandwich`, `spdep`, `spatialreg`, `jsonlite`, `emIRT`

## Data Sources

- **Commission proposals**: Manually enriched JSONs from the Convention's five thematic commissions, containing article-level text, authorship, and final-status metadata.
- **Delegate profiles**: Web-scraped from the Biblioteca del Congreso Nacional (BCN) biographical portal.
- **Legislative votes** (for emIRT): Roll-call data from 91 voting sessions (2021-07-13 to 2022-06-24).

Only derived and manually enriched data files are included in this repository; raw scraping inputs are excluded to keep the repository focused on analysis.

---

This is a working paper: we are expanding the dataset to include all seven commissions (C2, C4, C6, C7 are currently excluded from Models 2–3 pending completion of modification mapping).

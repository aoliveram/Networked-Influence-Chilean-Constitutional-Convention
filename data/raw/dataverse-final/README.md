# Snapshot dataverse-final (7 comisiones)

Copia versionada de los datos limpios del repositorio CPT
(`constitutional-proposal-tracking`), branch `paper-draft`, commit
**`5852519ea2bfa222deb498b4952b8e05d57e39ef`** (2026-07-07). Regla del proyecto
(P14): ningún script lee de CPT en runtime; para actualizar los datos se copia un
snapshot nuevo desde CPT y se corre el test de aceptación:

```bash
python3 code/0a-verify-dataverse-snapshot.py   # escribe QA-report.txt aquí
```

## Contenido

- `comision-{1..7}/C{k}_{GENESIS_master,BORRADOR_final,TRACK_articles,TRACK_full}.json`
- `coincidencias_comisiones.csv` — tabla maestra de trazabilidad de los 498 artículos
  del borrador final (UTF-8, separador `,`): 298 identical, 146 similar, 54 not_traced.
- `dataverse-codebook-draft.md` — codebook de CPT (semántica de `article_uid`,
  `final_status`, `timestamp` MM-DD con año implícito 2022 y sufijo `-{bloque}`).
- `QA-report.txt` — salida del último test de aceptación.

## Cifras del snapshot

TRACK_full = **2.019** registros: **1.809 artículos** + **210 indicaciones sueltas**
(top-level, con `action`; sin anclar a artículo) + 0 títulos. `article_uid` y
`final_status` al 100%. GENESIS = 1.892 iniciativas; BORRADOR = 498 artículos.

Cobertura de `authors` en artículos TRACK_full (base de la red de co-patrocinio):
C1 99/99, **C2 0/182 (P15 — pendiente en CPT)**, C3 222/222, C4 135/167,
C5 420/464, C6 418/447, C7 218/228. Indicaciones sueltas utilizables para ondas
(authors + timestamp): 205/210. Timestamps NA top-level: 66 (C1 4, C3 2, C5 5,
C6 17, C7 38).

Los strings de autor incluyen referencias a **iniciativas populares** (números
tipo `7-2`, "Iniciativa Popular Indígena 21-2"): `code/lib_names.py` las clasifica
como no-persona y las excluye de la red; toda mención de persona resuelve a los
154 nombres canónicos de `convention_members.json`.

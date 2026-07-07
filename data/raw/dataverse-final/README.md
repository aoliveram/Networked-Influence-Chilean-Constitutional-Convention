# Snapshot dataverse-final (7 comisiones)

Copia versionada de los datos limpios del repositorio CPT
(`constitutional-proposal-tracking`), branch `paper-draft`, commit
**`6fac4c4cc933dbb4702a1ad5c1d966dc10c7ef8c`** (2026-07-07, ronda 2: authors de C2/C4 poblados). Regla del proyecto
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

Cobertura de `authors` en artículos TRACK_full (base de la red de co-patrocinio),
tras la ronda 2 (C2 poblado vía `sources`→firmantes, C4 vía `icc_id`; reglas
validadas 1.415/1.416): C1 99/99, C2 149/182, C3 222/222, C4 150/167, C5 420/464,
C6 418/447, C7 218/228 → **1.676 artículos con ≥2 autores-persona**. Residuo
documentado en el codebook: 134/2.019 sin autores = 65 de iniciativas
populares/indígenas + 45 de ICC no recuperadas + 24 sin referencia de fuente.
Indicaciones sueltas utilizables para ondas (authors + timestamp): 205/210.
Timestamps sin fecha ("undated", no se imputan): 66 top-level (C1 4, C3 2, C5 5,
C6 17, C7 38) + 5 anidados en history[].

Los strings de autor incluyen referencias a **iniciativas populares** (números
tipo `7-2`, "Iniciativa Popular Indígena 21-2"): `code/lib_names.py` las clasifica
como no-persona y las excluye de la red; toda mención de persona resuelve a los
154 nombres canónicos de `convention_members.json`.

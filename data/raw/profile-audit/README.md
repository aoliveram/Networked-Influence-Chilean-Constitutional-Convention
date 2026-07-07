# Auditoría dual-fuente de perfiles e imputación (P5)

Generado por `code/0b-audit-profiles-dual-source.py` con `gemini-3.5-flash` (API Gemini,
search grounding). Para cada uno de los 154 convencionales se extraen las características
dos veces, de forma independiente: desde el texto BCN scrapeado
(`conventional-profiles-raw.json`) y desde su artículo de Wikipedia en español. Toda la
información está referida al período de la Convención (jul-2021 a jul-2022).

Hay dos corridas:

- **v1** (`gemini_batches/`, 2026-07-06): lotes de 5, grado académico en escala 0--2
  (0 sin postgrado, 1 magíster, 2 doctorado). Se conserva como referencia.
- **v2** (`profiles-batches/`, 2026-07-07, **vigente**): lotes de 6, grado académico en
  escala 0--3 (0 sin estudios universitarios terminados, 1 educación superior terminada,
  2 magíster/máster sin contar diplomados, 3 doctorado). Las tablas y la imputación se
  construyen desde esta corrida.

## Archivos

- `profile_audit_table.csv` — tabla 154 × 8 características con símbolos de concordancia:
  - `=` ambas fuentes informan y coinciden
  - `≠` ambas fuentes informan y **discrepan** (revisar a mano)
  - `B` solo BCN informa
  - `W` solo Wikipedia informa
  - `∅` ninguna fuente informa
- `profile_audit_values.csv` — valores crudos por fuente (`<campo>_bcn`, `<campo>_wiki`) + URL de Wikipedia.
- `discrepancias_pipeline.csv` — diferencias entre la auditoría v2 y la salida cruda del
  scraper de CPT (`conventional-profiles.json` de CPT).
- `runs_consistency.csv` — desacuerdos entre v1 y v2, por fuente y campo (el grado se
  compara mapeando la escala: v1 1→2, v1 2→3). Consistencia observada: 100% en género,
  fecha, distrito y experiencia; 99.6% abogado; 97.5% lista; 96.0% grado; 93.5% afiliación.
- `manual_validations.json` — validaciones humanas del usuario (2026-07-07). **Siempre
  prevalecen sobre la regla automática.** Para agregar nuevas validaciones, añadir objetos
  `{nombre, campo, valor}` y re-correr `--impute`. (Las entradas de grado hechas bajo la
  escala antigua y las de formato de distrito fueron retiradas; ver `descripcion` del archivo.)
- `imputation_report.csv` — cambios aplicados en la última pasada de imputación, con su
  origen (`bcn`, `wiki`, `base`, `manual*`).
- `gemini_batches/`, `profiles-batches/` — respuestas crudas por lote (provenance;
  `batch_999` es el lote de prueba).

## Imputación (regla del usuario, 2026-07-07)

`--impute` regenera `data/raw/conventional-profiles.json` (154 perfiles): **BCN es ground
truth; si BCN no informa, Wikipedia; si ninguna informa, se conserva el valor base**; las
validaciones de `manual_validations.json` prevalecen sobre todo lo anterior. Los escaños
reservados se codifican `distrito = "Escaño reservado: <pueblo>"` y su afiliación es la
partidaria real (o "Independiente"). Resultado vigente: 154 perfiles, 0 afiliaciones
"Desconocida", 0 edades faltantes; grado 0--3 = {0: 18, 1: 81, 2: 41, 3: 14}.

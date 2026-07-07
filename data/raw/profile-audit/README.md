# Auditoría dual-fuente de perfiles (P5)

Generado por `code/0b-audit-profiles-dual-source.py` con `gemini-3.5-flash` (API Gemini,
search grounding), lotes de 5 convencionales. Para cada uno de los 154 convencionales se
extraen las características dos veces, de forma independiente: desde el texto BCN scrapeado
(`conventional-profiles-raw.json`) y desde su artículo de Wikipedia en español. Toda la
información está referida al período de la Convención (jul-2021 a jul-2022).

## Archivos

- `profile_audit_table.csv` — tabla 154 × 8 características con símbolos de concordancia:
  - `=` ambas fuentes informan y coinciden
  - `≠` ambas fuentes informan y **discrepan** (revisar a mano)
  - `B` solo BCN informa
  - `W` solo Wikipedia informa
  - `∅` ninguna fuente informa
- `profile_audit_values.csv` — valores crudos por fuente (`<campo>_bcn`, `<campo>_wiki`) + URL de Wikipedia.
- `discrepancias_pipeline.csv` — filas donde la auditoría contradice `conventional-profiles.json`
  (el insumo actual de los modelos): nombre, campo, valor del pipeline, valor auditado y
  valores por fuente.
- `gemini_batches/batch_*.json` — respuestas crudas por lote (provenance; `batch_999` es el lote de prueba).

## Resumen de la corrida 2026-07-06

154/154 auditados; ~124 con artículo de Wikipedia (los ~30 restantes quedan con símbolo `B`).
Conflictos entre fuentes (`≠`): afiliación 12, fecha de nacimiento 8, grado académico 6,
abogado 4, experiencia 2, distrito 1, lista 1. Discrepancias contra el pipeline: **135**
(afiliación 62 — resuelve las 33 "Desconocida" y detecta falsos "Independiente" como
Zúñiga→UDI; grado 29; experiencia 24; distrito 17, incl. los 9 escaños reservados; género 1;
edad 1 — Garín: 35; abogado 1).

**Paso siguiente:** validación humana de `discrepancias_pipeline.csv`, con foco en los 12
conflictos `≠` de afiliación (p. ej. Chahin: BCN "Independiente" vs Wikipedia "Demócrata
Cristiano") antes de regenerar las covariables del pipeline.

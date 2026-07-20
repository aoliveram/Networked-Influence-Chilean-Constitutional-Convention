"""
27-extract-initiative-dates.py  (refresh script; v2 2026-07-20, decisión
"red desde la plataforma")
Materializa el snapshot local del pool COMPLETO de iniciativas de la
plataforma oficial (CPT/submitted_initiatives, 995 ICC tras eliminar la 519
duplicada de la 451): número, comisión, fecha_iso (100% fechado; 131
imputadas con nota), autor principal y LISTA DE FIRMANTES armonizada.
Como todos los refrescos: CPT se lee SOLO aquí; el pipeline consume el CSV.

Output: data/raw/platform_initiatives.csv
"""

import csv
import glob
import json
import os
import re
import sys

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
from paths import DATA_RAW  # noqa: E402

CPT_DIR = ("/Users/anibaloliveramorales/Desktop/Doctorado/-Projects-/"
           "B - constitutional-proposal-tracking/submitted_initiatives")

rows = []
for path in sorted(glob.glob(os.path.join(CPT_DIR, "api_extracted_*.json"))):
    for fname, rec in json.load(open(path, encoding="utf-8")).items():
        m = re.match(r"^(\d+)", fname)
        if not m:
            continue
        firmantes = rec.get("firmantes_matched") or []
        rows.append({
            "icc_num": int(m.group(1)),
            "commission": f"C{rec['comision_n']}" if rec.get("comision_n") else "",
            "fecha_iso": rec.get("fecha_iso") or "",
            "fecha_imputada": 1 if rec.get("fecha_iso_nota") else 0,
            "autor": rec.get("autor_matched") or "",
            "n_firmantes": len(firmantes),
            "firmantes": "; ".join(sorted(firmantes)),
        })
rows.sort(key=lambda r: r["icc_num"])

out = os.path.join(DATA_RAW, "platform_initiatives.csv")
with open(out, "w", newline="", encoding="utf-8") as fh:
    wr = csv.DictWriter(fh, fieldnames=["icc_num", "commission", "fecha_iso",
                                        "fecha_imputada", "autor", "n_firmantes",
                                        "firmantes"])
    wr.writeheader()
    wr.writerows(rows)

n_ok = sum(1 for r in rows if 2 <= r["n_firmantes"] <= 16)
print(f"ICC: {len(rows)} | con fecha_iso: {sum(1 for r in rows if r['fecha_iso'])} "
      f"(imputadas: {sum(r['fecha_imputada'] for r in rows)}) | "
      f"con comisión: {sum(1 for r in rows if r['commission'])}")
print(f"utilizables (2-16 firmantes): {n_ok} | >16 (excluidas D8): "
      f"{sum(1 for r in rows if r['n_firmantes'] > 16)} | <2: "
      f"{sum(1 for r in rows if r['n_firmantes'] < 2)}")
fechas = sorted(r["fecha_iso"] for r in rows if r["fecha_iso"])
print(f"rango de fechas: {fechas[0]} .. {fechas[-1]}")
print(f"-> {out}")

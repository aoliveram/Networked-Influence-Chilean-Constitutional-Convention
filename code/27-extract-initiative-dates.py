"""
27-extract-initiative-dates.py  (refresh script, IV.P2)
Extrae fecha de ingreso, autor principal y firmantes de las ICC desde
CPT/submitted_initiatives (11 JSON de la API) y las materializa como snapshot
local. Como todos los refrescos: CPT se lee SOLO aquí; el pipeline consume
el CSV local.

Output: data/raw/initiative_submission_dates.csv
        (initiative_id, commission, fecha_ingreso, autor, n_firmantes_matched)
+ reporte de cruce contra data/processed/initiative_registry.csv
"""

import csv
import glob
import json
import os
import re
import sys
import unicodedata
from collections import Counter

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
from paths import DATA_PROCESSED, DATA_RAW  # noqa: E402

CPT_DIR = ("/Users/anibaloliveramorales/Desktop/Doctorado/-Projects-/"
           "B - constitutional-proposal-tracking/submitted_initiatives")

MESES = {"enero": 1, "febrero": 2, "marzo": 3, "abril": 4, "mayo": 5,
         "junio": 6, "julio": 7, "agosto": 8, "septiembre": 9,
         "octubre": 10, "noviembre": 11, "diciembre": 12}
ABREV = {"ene": 1, "feb": 2, "mar": 3, "abr": 4, "may": 5, "jun": 6,
         "jul": 7, "ago": 8, "sep": 9, "oct": 10, "nov": 11, "dic": 12}


def parse_fecha(s):
    """Formatos: '04/enero/2022', '18/Ene/2022', '24 de enero de 2022',
    '24/enero/202' (año truncado). Devuelve (iso, corregida:bool) o None."""
    if not s:
        return None
    s = unicodedata.normalize("NFC", s.strip().lower())
    m = (re.match(r"^(\d{1,2})/([a-zñ]+)/(\d{3,4})$", s) or
         re.match(r"^(\d{1,2}) de ([a-zñ]+) de (\d{3,4})$", s))
    if not m:
        return None
    mes = MESES.get(m.group(2)) or ABREV.get(m.group(2)[:3])
    if mes is None:
        return None
    dia, anio = int(m.group(1)), int(m.group(3))
    corregida = False
    if anio == 202:                      # año truncado ('202' -> 2022)
        anio, corregida = 2022, True
    # typos de año: la ventana ICC es nov-2021..feb-2022
    if anio == 2021 and mes in (1, 2):   # 'enero/febrero 2021' -> 2022
        anio, corregida = 2022, True
    if anio == 2022 and mes == 12:       # 'diciembre 2022' -> 2021
        anio, corregida = 2021, True
    return f"{anio:04d}-{mes:02d}-{dia:02d}", corregida


rows, sin_fecha, sin_id = [], 0, 0
for path in sorted(glob.glob(os.path.join(CPT_DIR, "api_extracted_*.json"))):
    data = json.load(open(path, encoding="utf-8"))
    for fname, rec in data.items():
        # dos formatos de nombre: "99-3-Texto..." (num-comisión) y "1035-Texto..."
        # (solo num; la comisión viene en el registro como comision_n)
        m = re.match(r"^(\d+)(?:-(\d+))?(?=-[A-Za-z])", fname) or re.match(r"^(\d+)(?:-(\d+))?", fname)
        if not m:
            sin_id += 1
            continue
        num = int(m.group(1))
        suf = m.group(2)
        com_n = rec.get("comision_n") or (int(suf) if suf else None)
        parsed = parse_fecha(rec.get("fecha"))
        if parsed is None:
            sin_fecha += 1
        rows.append({
            "initiative_id": f"{num}-{suf}" if suf else str(num),
            "icc_num": num,
            "commission": f"C{com_n}" if com_n else "",
            "fecha_ingreso": parsed[0] if parsed else "",
            "fecha_corregida": int(parsed[1]) if parsed else "",
            "fecha_raw": rec.get("fecha") or "",
            "autor": rec.get("autor_matched") or "",
            "n_firmantes_matched": rec.get("n_firmantes_matched") or "",
        })

# dedupe por initiative_id (si un id aparece en varios archivos, conservar el que tenga fecha)
by_id = {}
for r in rows:
    prev = by_id.get(r["initiative_id"])
    if prev is None or (not prev["fecha_ingreso"] and r["fecha_ingreso"]):
        by_id[r["initiative_id"]] = r
rows = sorted(by_id.values(), key=lambda r: (r["icc_num"], r["initiative_id"]))

out = os.path.join(DATA_RAW, "initiative_submission_dates.csv")
with open(out, "w", newline="", encoding="utf-8") as fh:
    wr = csv.DictWriter(fh, fieldnames=["initiative_id", "icc_num", "commission",
                                        "fecha_ingreso", "fecha_corregida", "fecha_raw",
                                        "autor", "n_firmantes_matched"])
    wr.writeheader()
    wr.writerows(rows)

con_fecha = sum(1 for r in rows if r["fecha_ingreso"])
print(f"ICC extraídas: {len(rows)} | con fecha: {con_fecha} | sin fecha: {len(rows) - con_fecha} "
      f"| archivos sin id parseable: {sin_id}")
fechas = sorted(r["fecha_ingreso"] for r in rows if r["fecha_ingreso"])
print(f"rango de fechas: {fechas[0]} .. {fechas[-1]}")

# anomalías de fecha (la ventana real de ICC es ~nov-2021 a feb-2022)
anom = [r for r in rows if r["fecha_ingreso"] and not
        ("2021-10-01" <= r["fecha_ingreso"] <= "2022-03-01")]
print(f"fechas fuera de la ventana oct-2021..mar-2022: {len(anom)} "
      f"({sorted(set(a['fecha_ingreso'] for a in anom))[:8]}...)")

# ---------------- cruce contra el registro de análisis (<=16, D8) ----------------
reg = [r for r in csv.DictReader(open(os.path.join(DATA_PROCESSED, "initiative_registry.csv"),
                                      encoding="utf-8"))
       if 2 <= int(r["n_firmantes"]) <= 16]
by_full = {r["initiative_id"]: r for r in rows}
by_numcom = {}
for r in rows:
    by_numcom.setdefault((r["icc_num"], r["commission"]), []).append(r)
by_num = {}
for r in rows:
    by_num.setdefault(r["icc_num"], []).append(r)


def lookup(rid, commission):
    hit = by_full.get(rid)
    if hit:
        return hit, "id"
    m = re.match(r"^(\d+)", rid)
    if not m:
        return None, None
    num = int(m.group(1))
    cands = by_numcom.get((num, commission), [])
    if len(cands) == 1:
        return cands[0], "num+com"
    cands = by_num.get(num, [])
    if len(cands) == 1:
        return cands[0], "num"
    return None, None


tally = Counter()
no_match, matched_dates = [], []
for r in reg:
    hit, how = lookup(r["initiative_id"], r["commission"])
    if hit is None:
        no_match.append(f"{r['commission']}:{r['initiative_id']}")
        tally["sin_match"] += 1
    elif not hit["fecha_ingreso"]:
        tally[f"match_{how}_sin_fecha"] += 1
    else:
        tally[f"match_{how}"] += 1
        matched_dates.append(hit["fecha_ingreso"])

print(f"\nCruce con las {len(reg)} iniciativas de análisis (<=16):")
for k, v in tally.most_common():
    print(f"  {k}: {v}")
if no_match:
    print(f"  sin match: {'; '.join(no_match[:15])}{' ...' if len(no_match) > 15 else ''}")

cnt = Counter(matched_dates)
print(f"\nCon fecha: {len(matched_dates)}/{len(reg)} | días distintos: {len(cnt)} | "
      f"top 5 días: {cnt.most_common(5)}")
print(f"-> {out}")

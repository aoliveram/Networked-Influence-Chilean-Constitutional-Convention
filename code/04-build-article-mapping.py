"""
04-build-article-mapping.py  (v2)
Lector del mapeo génesis -> borrador final desde coincidencias_comisiones.csv
(fuente de verdad, decisión 2026-07-06) + clasificación de desenlaces por artículo.

- El texto final de cada artículo viene de C{k}_BORRADOR_final.json; la fila de
  coincidencias se une por POSICIÓN (final_draft_order es contiguo por comisión
  y calza 1:1 con el orden del archivo; se valida contra el prefijo "N.-").
- Pares génesis-final: uno por (final_order, source_uid) usando las fuentes
  primaria/secundaria/terciaria. Artículos TRACK con final_status Idéntico/
  Similar que no aparezcan en coincidencias se rescatan parseando el número de
  orden desde final_status ("Idéntico a 142.- ..."), marcados como fallback.
- track_article_outcomes.csv clasifica TODOS los artículos TRACK:
  identico / similar / fallido / eliminado (los dos últimos = fracaso, sim = 0
  en la DV principal de M3).

Outputs (data/processed/): article_mapping_unified.csv, track_article_outcomes.csv
"""

import csv
import json
import os
import re
import unicodedata
from collections import defaultdict

from lib_names import clean_authors
from paths import COMMISSIONS, COINCIDENCIAS, DATA_PROCESSED, borrador_path, track_full_path


def norm(s):
    return unicodedata.normalize("NFKD", s or "").encode("ascii", "ignore").decode().lower().strip()


def status_class(fs):
    f = norm(fs)
    if f.startswith("identico") or f.startswith("idntico"):
        return "identico"
    if f.startswith("similar"):
        return "similar"
    if "fallido" in f:
        return "fallido"
    if "eliminado" in f:
        return "eliminado"
    return "otro"


def parse_target_order(fs):
    m = re.search(r"\ba\s+(\d+)\s*\.-", fs or "")
    return int(m.group(1)) if m else None


# --- textos del borrador final, por orden global 1..498 ---
final_text = {}
for k in COMMISSIONS:
    for entry in json.load(open(borrador_path(k), encoding="utf-8")):
        m = re.match(r"\s*(\d+)\s*\.-", entry.get("article") or "")
        if m:
            final_text[int(m.group(1))] = entry["text"]
assert len(final_text) == 498, f"borradores: {len(final_text)} órdenes con prefijo N.-"

# --- registros TRACK por uid ---
track_by_uid = {}
articles = []
for k in COMMISSIONS:
    for r in json.load(open(track_full_path(k), encoding="utf-8")):
        if r.get("article_uid"):
            track_by_uid[r["article_uid"]] = (k, r)
        if "action" not in r and "titleuid" not in r:
            articles.append((k, r))

# --- filas de coincidencias -> pares ---
rows = list(csv.DictReader(open(COINCIDENCIAS, encoding="utf-8")))
pairs = []
orders_by_uid = defaultdict(set)
missing_uid, not_traced = [], 0
for row in rows:
    order = int(row["final_draft_order"])
    comm = row["source_commission"]
    for tier in ("primary", "secondary", "tertiary"):
        uid = (row.get(f"source_article_uid_{tier}") or "").strip()
        status = (row.get(f"traceability_status_{tier}") or "").strip()
        if not uid or uid.upper() in ("NA", "N/A"):
            continue
        if status == "not_traced":
            not_traced += 1
            continue
        if uid not in track_by_uid:
            missing_uid.append(uid)
            continue
        _, rec = track_by_uid[uid]
        pairs.append({
            "final_order": order, "commission": comm, "source_uid": uid,
            "status": status, "origin": "coincidencias",
            "genesis_text": rec.get("text") or rec.get("content_snapshot") or rec.get("content") or "",
            "final_text": final_text[order],
            "authors": clean_authors(rec.get("authors") or []),
        })
        orders_by_uid[uid].add(order)

n_coinc = len(pairs)

# --- fallback: artículos Idéntico/Similar sin par vía coincidencias ---
fallback = 0
for k, r in articles:
    cls = status_class(r.get("final_status"))
    uid = r["article_uid"]
    if cls in ("identico", "similar") and uid not in orders_by_uid:
        order = parse_target_order(r.get("final_status"))
        if order and order in final_text:
            pairs.append({
                "final_order": order, "commission": f"C{k}", "source_uid": uid,
                "status": "identical" if cls == "identico" else "similar",
                "origin": "final_status_parse",
                "genesis_text": r.get("text") or "",
                "final_text": final_text[order],
                "authors": clean_authors(r.get("authors") or []),
            })
            orders_by_uid[uid].add(order)
            fallback += 1

print(f"Pares génesis-final: {len(pairs)} ({n_coinc} vía coincidencias + {fallback} rescatados "
      f"desde final_status); fuentes not_traced: {not_traced}; uids no encontrados en TRACK: {len(missing_uid)}")
if missing_uid:
    print(f"  no encontrados: {sorted(set(missing_uid))}")

os.makedirs(DATA_PROCESSED, exist_ok=True)
with open(os.path.join(DATA_PROCESSED, "article_mapping_unified.csv"), "w", newline="", encoding="utf-8") as fh:
    wr = csv.writer(fh)
    wr.writerow(["final_order", "commission", "source_uid", "status", "origin",
                 "genesis_text", "final_text", "authors"])
    for p in pairs:
        wr.writerow([p["final_order"], p["commission"], p["source_uid"], p["status"],
                     p["origin"], p["genesis_text"], p["final_text"], "; ".join(p["authors"])])

# --- desenlaces por artículo TRACK ---
counts = defaultdict(int)
with open(os.path.join(DATA_PROCESSED, "track_article_outcomes.csv"), "w", newline="", encoding="utf-8") as fh:
    wr = csv.writer(fh)
    wr.writerow(["commission", "article_uid", "outcome_class", "final_orders", "n_authors", "authors"])
    for k, r in articles:
        cls = status_class(r.get("final_status"))
        au = clean_authors(r.get("authors") or [])
        counts[cls] += 1
        wr.writerow([f"C{k}", r["article_uid"], cls,
                     ";".join(str(o) for o in sorted(orders_by_uid.get(r["article_uid"], set()))),
                     len(au), "; ".join(au)])
print(f"Desenlaces de artículos TRACK: {dict(counts)}")
traced_no_pair = sum(1 for k, r in articles
                     if status_class(r.get("final_status")) in ("identico", "similar")
                     and r["article_uid"] not in orders_by_uid)
print(f"Artículos Idéntico/Similar SIN par de texto (quedan fuera de la similitud): {traced_no_pair}")
print("--- Done ---")

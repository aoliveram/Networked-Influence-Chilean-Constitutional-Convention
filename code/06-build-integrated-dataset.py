"""
06-build-integrated-dataset.py  (v2)
Dataset integrado transversal: exactamente una fila por cada uno de los 154
convencionales del roster canónico (el merge es roster-driven; P4 muere por
construcción). Une éxito (05), métricas de red (01, red iniciativa), ideología
(02) y perfiles curados (P5), y deriva ego_heterophily / cross_coalition_ties
sobre la red génesis de iniciativas.
"""

import csv
import json
import os
from collections import defaultdict

from paths import DATA_PROCESSED, MEMBERS, PROFILES

csv.field_size_limit(10_000_000)


def load_csv(name, key="nombre_armonizado"):
    with open(os.path.join(DATA_PROCESSED, name), encoding="utf-8") as fh:
        return {r[key]: r for r in csv.DictReader(fh)}


def num(v):
    if v in (None, "", "None", "NA"):
        return None
    return float(v)


members = sorted(json.load(open(MEMBERS, encoding="utf-8")))
success = load_csv("author_success_scores.csv")
metrics = load_csv("network_metrics.csv")
ideology = load_csv("emirt_summary_metrics.csv")
profiles = {p["nombre_armonizado"]: p for p in json.load(open(PROFILES, encoding="utf-8"))}
print(f"roster={len(members)} success={len(success)} metrics={len(metrics)} "
      f"ideology={len(ideology)} profiles={len(profiles)}")

# vecindarios sobre la red génesis (iniciativa)
neighbors = defaultdict(list)
with open(os.path.join(DATA_PROCESSED, "genesis_network_initiative.csv"), encoding="utf-8") as fh:
    for r in csv.DictReader(fh):
        w = int(r["weight"])
        neighbors[r["source"]].append((r["target"], w))
        neighbors[r["target"]].append((r["source"], w))

def coalition(name):
    return profiles.get(name, {}).get("afiliacion_agrupada", "Desconocida")

rows = []
for m in members:
    own = coalition(m)
    nbrs = neighbors.get(m, [])
    n_cross = sum(1 for nb, _ in nbrs if coalition(nb) != own)
    s, mt, idl, p = success.get(m, {}), metrics.get(m, {}), ideology.get(m, {}), profiles.get(m, {})
    rows.append({
        "nombre_armonizado": m,
        # éxito (DV principal y robustez)
        "n_articles_authored": int(s.get("n_articles") or 0),
        "n_traced": int(s.get("n_traced") or 0),
        "survival_rate": num(s.get("survival_rate")),
        "retention_all": num(s.get("retention_all")),           # y' principal
        "retention_traced": num(s.get("retention_traced")),     # robustez (DV antigua)
        "retention_all_sbert": num(s.get("retention_all_sbert")),
        "retention_traced_sbert": num(s.get("retention_traced_sbert")),
        # red (iniciativa)
        "degree": int(float(mt.get("degree") or 0)),
        "weighted_degree": num(mt.get("weighted_degree")) or 0.0,
        "betweenness": num(mt.get("betweenness")) or 0.0,
        "eigenvector": num(mt.get("eigenvector")) or 0.0,
        # ideología
        "theta_mean": num(idl.get("theta_mean")),
        "theta_sd": num(idl.get("theta_sd")),
        "theta_range": num(idl.get("theta_range")),
        "theta_shift": num(idl.get("theta_shift")),
        # perfiles curados
        "es_mujer": p.get("es_mujer"),
        "es_abogado": p.get("es_abogado"),
        "edad_al_asumir": p.get("edad_al_asumir"),
        "experiencia_previa_institucional": p.get("experiencia_previa_institucional"),
        "grado_academico_nivel": p.get("grado_academico_nivel"),
        "afiliacion_agrupada": p.get("afiliacion_agrupada", "Desconocida"),
        "distrito": p.get("distrito"),
        # derivadas
        "ego_heterophily": (n_cross / len(nbrs)) if nbrs else 0.0,
        "cross_coalition_ties": n_cross,
    })

assert len(rows) == 154, f"esperaba 154 filas, hay {len(rows)}"
csv_path = os.path.join(DATA_PROCESSED, "integrated_dataset.csv")
with open(csv_path, "w", newline="", encoding="utf-8") as fh:
    wr = csv.DictWriter(fh, fieldnames=list(rows[0].keys()))
    wr.writeheader()
    wr.writerows(rows)
json.dump(rows, open(os.path.join(DATA_PROCESSED, "integrated_dataset.json"), "w",
                     encoding="utf-8"), ensure_ascii=False, indent=2)

n_y = sum(1 for r in rows if r["retention_all"] is not None)
n_th = sum(1 for r in rows if r["theta_mean"] is not None)
complete = sum(1 for r in rows if r["retention_all"] is not None and r["theta_mean"] is not None
               and r["degree"] > 0 and r["es_mujer"] is not None)
print(f"integrated_dataset.csv: {len(rows)} filas | con y': {n_y} | con theta: {n_th} | "
      f"casos completos: {complete}")
print("--- Done ---")

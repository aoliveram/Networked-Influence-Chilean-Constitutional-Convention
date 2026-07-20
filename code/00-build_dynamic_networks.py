"""
00-build_dynamic_networks.py  (v2, actualización 7 comisiones)
Construye las redes de co-patrocinio desde el snapshot dataverse-final.

Unidad de co-firma (decisión 2026-07-07, "a revisar" en el reporte):
  - PRINCIPAL: INICIATIVA — cada iniciativa convencional (id de `sources`, o
    `icc_id` en C4) cuenta UNA vez, aunque se divida en varios artículos.
  - ROBUSTEZ: ARTÍCULO — cada artículo de TRACK_full con >=2 autores cuenta +1
    (compatible con la versión de abril 2026).

Ondas temporales (M2), las 7 comisiones: T0 = red génesis (iniciativas) de la
comisión; T1..Tn acumulan la co-firma de INDICACIONES (entradas de history[] +
indicaciones sueltas) agrupadas por fecha MM-DD (sufijo -bloque colapsado al día).
Registros sin timestamp o con <2 autores-persona se excluyen y se reportan.

Outputs (data/processed/):
  genesis_network_initiative.csv   red pooled, unidad iniciativa  [PRINCIPAL]
  genesis_network_article.csv      red pooled, unidad artículo    [robustez]
  initiative_registry.csv          registro de iniciativas y firmantes
  C{k}_dynamic_networks.json       ondas acumuladas por comisión (x7)
  commission_waves.csv             fechas de onda por comisión (insumo de 02)
"""

import csv
import hashlib
import itertools
import json
import os
import re
from collections import Counter, defaultdict

from lib_names import clean_authors
from paths import (COMMISSIONS, DATA_PROCESSED, DATA_RAW, genesis_path, track_full_path)

TS_RE = re.compile(r"^(\d{2}-\d{2})(-\d+)?$")

# Decisión 2026-07-11 (revisión D8): los eventos de co-firma con >16
# firmantes-persona son imposibles bajo la regla ICC (art. 83) y están en
# auditoría como duplicaciones transversales (p. ej. la 954 con 83 firmantes
# en C4+C5+C6). SE EXCLUYEN de toda red/menú de análisis; el registro CSV
# se escribe COMPLETO (sigue documentando la anomalía; F9 la muestra).
MAX_SIGNERS = 16


def classify(record):
    if "titleuid" in record or "tite" in record:
        return "titulo"
    return "suelta" if "action" in record else "articulo"


def ts_day(ts):
    """'04-01-2' -> '04-01'; None/'NA'/mal formado -> None."""
    m = TS_RE.match(str(ts or ""))
    return m.group(1) if m else None


def event_hash(authors, ts, content):
    key = "|".join(sorted(authors)) + "|" + str(ts) + "|" + str(content or "")[:400]
    return hashlib.md5(key.encode()).hexdigest()


def add_clique(edges, authors, w=1):
    for a, b in itertools.combinations(sorted(authors), 2):
        edges[(a, b)] += w


def save_edges(edges, path):
    with open(path, "w", newline="", encoding="utf-8") as fh:
        wr = csv.writer(fh)
        wr.writerow(["source", "target", "weight"])
        for (s, t), w in sorted(edges.items()):
            wr.writerow([s, t, w])


def net_stats(edges):
    nodes = {n for e in edges for n in e}
    return len(nodes), len(edges), sum(edges.values())


# =============================================================================
# 1. Registro de iniciativas (unidad principal de co-firma)
# =============================================================================

def build_initiative_registry():
    """(comisión, iniciativa_id) -> set de autores-persona canónicos.

    C1-C3, C5-C7: id = cada elemento de `sources` (GENESIS y artículos TRACK).
    C4: id = `icc_id` del GENESIS (los artículos TRACK joinean vía article_uid).
    Registros mono-fuente asignan primero (autores = firmantes de esa iniciativa,
    regla de proveniencia del dataset); los multi-fuente solo rellenan iniciativas
    aún no vistas (sus autores son la unión de firmantes) y se cuentan aparte.
    """
    registry = {}
    multi_assigned = 0
    conflicts = 0

    for k in COMMISSIONS:
        track = json.load(open(track_full_path(k), encoding="utf-8"))
        genesis = json.load(open(genesis_path(k), encoding="utf-8"))
        arts = [r for r in track if classify(r) == "articulo"]

        if k == 4:
            uid2icc = {g.get("article_uid"): str(g.get("icc_id")).strip()
                       for g in genesis if g.get("icc_id")}
            records = [(uid2icc.get(r.get("article_uid")), clean_authors(r.get("authors") or []))
                       for r in arts]
            records += [(str(g["icc_id"]).strip(), clean_authors(g.get("authors") or []))
                        for g in genesis if g.get("icc_id")]
            singles = [([iid], au) for iid, au in records if iid]
            multis = []
        else:
            pool = [(r.get("sources") or [], clean_authors(r.get("authors") or []))
                    for r in arts + genesis]
            singles = [(src, au) for src, au in pool if len(src) == 1]
            multis = [(src, au) for src, au in pool if len(src) > 1]

        for src, au in singles:
            if not au:
                continue
            key = (k, str(src[0]).strip())
            if key in registry:
                if registry[key] != frozenset(au):
                    registry[key] = registry[key] | frozenset(au)
                    conflicts += 1
            else:
                registry[key] = frozenset(au)
        for src, au in multis:
            if not au:
                continue
            for s in src:
                key = (k, str(s).strip())
                if key not in registry:
                    registry[key] = frozenset(au)
                    multi_assigned += 1
    return registry, multi_assigned, conflicts


# =============================================================================
# 2. Eventos de indicación (para las ondas de M2)
# =============================================================================

def indication_events(track):
    """Eventos de indicación de history[] + sueltas.

    Devuelve (days, edge_events, contadores): `days` son TODOS los días MM-DD
    observados en timestamps válidos de indicaciones (definen las ondas aunque
    la indicación sea unipersonal — la fecha del informe es real); `edge_events`
    son los eventos con >=2 autores-persona (los únicos que agregan lazos),
    deduplicados por hash de autores+día+contenido (una indicación repetida en
    el history de varios artículos cuenta una sola vez como acto de co-firma).
    """
    seen = set()
    days = set()
    edge_events, dropped_dup, dropped_na, solo_events = [], 0, 0, 0
    for r in track:
        cls = classify(r)
        pool = []
        if cls == "articulo":
            pool = [(h.get("timestamp"), h.get("authors"), h.get("content"))
                    for h in r.get("history", [])]
        elif cls == "suelta":
            pool = [(r.get("timestamp"), r.get("authors"), r.get("content"))]
        for ts, authors, content in pool:
            day = ts_day(ts)
            if day is None:
                dropped_na += 1
                continue
            days.add(day)
            au = clean_authors(authors or [])
            if len(au) < 2:
                solo_events += 1
                continue
            # NOTA D8: aquí NO se aplica el tope de 16 — la regla 8-16 del
            # art. 83 rige para INICIATIVAS; una indicación puede llevar
            # legítimamente >16 firmas (p. ej. renovaciones, art. 95).
            h = event_hash(au, day, content)
            if h in seen:
                dropped_dup += 1
                continue
            seen.add(h)
            edge_events.append((day, au))
    return sorted(days), edge_events, dropped_dup, dropped_na, solo_events


# =============================================================================
# Main
# =============================================================================

if __name__ == "__main__":
    os.makedirs(DATA_PROCESSED, exist_ok=True)

    # ------------------------------------------------------------------
    # Registro de iniciativas: DESDE LA PLATAFORMA (decisión 2026-07-20).
    # data/raw/platform_initiatives.csv (refresco code/27) trae las 995 ICC
    # presentadas, con firmantes armonizados y fecha_iso al 100%. El registro
    # TRACK anterior (solo ICC que entraron a informes de comisión, ~528)
    # queda superado; TRACK sigue siendo la fuente de artículos e indicaciones.
    # ------------------------------------------------------------------
    plat_rows = list(csv.DictReader(open(os.path.join(DATA_RAW, "platform_initiatives.csv"),
                                         encoding="utf-8")))
    registry = {}
    for r in plat_rows:
        au = [a for a in r["firmantes"].split("; ") if a] if r["firmantes"] else []
        registry[(r["commission"], r["icc_num"], r["fecha_iso"], r["fecha_imputada"])] = au
    usable = {key: au for key, au in registry.items() if 2 <= len(au) <= MAX_SIGNERS}
    n_over = sum(1 for au in registry.values() if len(au) > MAX_SIGNERS)
    print("=== Registro de iniciativas (plataforma, 995 ICC) ===")
    for k in COMMISSIONS:
        n_u = sum(1 for (c, *_), au in usable.items() if c == f"C{k}")
        print(f"  C{k}: {n_u} utilizables")
    n_sincom = sum(1 for (c, *_), au in usable.items() if not c)
    print(f"  sin comisión asignada: {n_sincom}")
    print(f"  Total: {len(registry)} ICC ({len(usable)} utilizables 2..{MAX_SIGNERS}; "
          f"{n_over} excluidas por >{MAX_SIGNERS}, decisión D8)")

    with open(os.path.join(DATA_PROCESSED, "initiative_registry.csv"), "w", newline="", encoding="utf-8") as fh:
        wr = csv.writer(fh)
        wr.writerow(["commission", "initiative_id", "n_firmantes", "firmantes",
                     "fecha", "fecha_imputada"])
        for (c, iid, fecha, imp), au in sorted(registry.items(), key=lambda x: int(x[0][1])):
            wr.writerow([c, iid, len(au), "; ".join(sorted(au)), fecha, imp])

    # --- red génesis pooled: unidad INICIATIVA (principal) ---
    pooled_init = defaultdict(int)
    genesis_by_comm = {k: defaultdict(int) for k in COMMISSIONS}
    for (c, *_), au in usable.items():
        add_clique(pooled_init, au)
        if c:
            k = int(c[1:])
            add_clique(genesis_by_comm[k], au)
    n, e, w = net_stats(pooled_init)
    print(f"\n=== Red génesis (iniciativa) === nodos={n}, aristas={e}, peso total={w}, "
          f"peso máx={max(pooled_init.values())}")
    save_edges(pooled_init, os.path.join(DATA_PROCESSED, "genesis_network_initiative.csv"))

    # --- red génesis pooled: unidad ARTÍCULO (robustez) ---
    pooled_art = defaultdict(int)
    n_art_events = 0
    for k in COMMISSIONS:
        track = json.load(open(track_full_path(k), encoding="utf-8"))
        for r in track:
            if classify(r) == "articulo":
                au = clean_authors(r.get("authors") or [])
                if 2 <= len(au) <= MAX_SIGNERS:
                    add_clique(pooled_art, au)
                    n_art_events += 1
    n, e, w = net_stats(pooled_art)
    print(f"=== Red génesis (artículo, robustez) === eventos={n_art_events}, nodos={n}, "
          f"aristas={e}, peso total={w}")
    save_edges(pooled_art, os.path.join(DATA_PROCESSED, "genesis_network_article.csv"))

    # --- ondas por comisión ---
    print("\n=== Ondas por comisión (T0 génesis-iniciativa + indicaciones) ===")
    waves_rows = []
    for k in COMMISSIONS:
        track = json.load(open(track_full_path(k), encoding="utf-8"))
        days, edge_events, d_dup, d_na, n_solo = indication_events(track)
        cumulative = dict(genesis_by_comm[k])
        waves = {"T0_Genesis": dict(cumulative)}
        by_day = defaultdict(list)
        for d, au in edge_events:
            by_day[d].append(au)
        for step, d in enumerate(days, start=1):
            cum = defaultdict(int, cumulative)
            for au in by_day[d]:
                add_clique(cum, au)
            cumulative = dict(cum)
            waves[f"T{step}_{d}"] = dict(cumulative)
            waves_rows.append([f"C{k}", step, d])
        out = {label: [{"source": s, "target": t, "weight": w} for (s, t), w in ed.items()]
               for label, ed in waves.items()}
        path = os.path.join(DATA_PROCESSED, f"C{k}_dynamic_networks.json")
        json.dump(out, open(path, "w", encoding="utf-8"), ensure_ascii=False)
        n_fin, e_fin, w_fin = net_stats(cumulative)
        print(f"  C{k}: {len(days)} ondas ({len(edge_events)} eventos con lazos, {n_solo} unipersonales; "
              f"descartados: {d_dup} duplicados, {d_na} sin fecha); "
              f"onda final: {n_fin} nodos, {e_fin} aristas, peso {w_fin}")

    with open(os.path.join(DATA_PROCESSED, "commission_waves.csv"), "w", newline="", encoding="utf-8") as fh:
        wr = csv.writer(fh)
        wr.writerow(["commission", "step", "date_mmdd"])
        wr.writerows(waves_rows)

    print("\n--- Done ---")

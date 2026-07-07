"""
0a-verify-dataverse-snapshot.py — Test de aceptación del snapshot dataverse-final.

Se corre cada vez que se refresca data/raw/dataverse-final/ desde CPT. Verifica:
  1. presencia de los 4 JSON por comisión + coincidencias_comisiones.csv,
  2. conteos por archivo contra el registro esperado,
  3. clasificación TRACK_full (artículo / indicación suelta / título),
  4. cobertura de article_uid y final_status (100%),
  5. cobertura de authors por comisión (génesis e history),
  6. timestamps: formato MM-DD(-bloque) o NA, con conteo de NA,
  7. validación de TODOS los strings de autor contra los 154 canónicos
     (vía lib_names; los no resueltos se listan y hacen fallar el QA).

Escribe data/raw/dataverse-final/QA-report.txt y termina con exit code != 0
si alguna verificación dura falla.
"""

import json
import re
import sys
from collections import Counter

from paths import (COMMISSIONS, COINCIDENCIAS, genesis_path, track_full_path,
                   track_articles_path, borrador_path, DATAVERSE_DIR)
from lib_names import clean_authors

# conteos esperados del snapshot vigente (CPT paper-draft @ 5852519)
EXPECTED = {
    1: {"genesis": 96,  "borrador": 100, "track_articles": 131, "track_full": 202},
    2: {"genesis": 312, "borrador": 41,  "track_articles": 24,  "track_full": 182},
    3: {"genesis": 222, "borrador": 96,  "track_articles": 72,  "track_full": 234},
    4: {"genesis": 167, "borrador": 58,  "track_articles": 58,  "track_full": 175},
    5: {"genesis": 464, "borrador": 43,  "track_articles": 36,  "track_full": 484},
    6: {"genesis": 440, "borrador": 119, "track_articles": 117, "track_full": 491},
    7: {"genesis": 191, "borrador": 41,  "track_articles": 40,  "track_full": 251},
}
TS_RE = re.compile(r"^\d{2}-\d{2}(-\d+)?$")

lines, failures = [], []


def log(msg):
    print(msg)
    lines.append(msg)


def fail(msg):
    failures.append(msg)
    log(f"  [FALLA] {msg}")


def classify(record):
    if "titleuid" in record or "tite" in record:
        return "titulo"
    return "indicacion_suelta" if "action" in record else "articulo"


def main():
    unmatched = {}
    tot = Counter()

    for k in COMMISSIONS:
        paths = {"genesis": genesis_path(k), "borrador": borrador_path(k),
                 "track_articles": track_articles_path(k), "track_full": track_full_path(k)}
        data = {name: json.load(open(p, encoding="utf-8")) for name, p in paths.items()}

        log(f"C{k}:")
        for name, d in data.items():
            n, exp = len(d), EXPECTED[k][name]
            if n != exp:
                fail(f"C{k} {name}: {n} registros, esperaba {exp}")
        log(f"  conteos ok ({', '.join(f'{name}={len(d)}' for name, d in data.items())})")

        cls = Counter(classify(r) for r in data["track_full"])
        if cls["titulo"]:
            fail(f"C{k}: {cls['titulo']} registros de título residuales en TRACK_full")
        arts = [r for r in data["track_full"] if classify(r) == "articulo"]
        inds = [r for r in data["track_full"] if classify(r) == "indicacion_suelta"]
        tot["articulos"] += len(arts)
        tot["sueltas"] += len(inds)

        sin_uid = sum(1 for r in data["track_full"] if not r.get("article_uid"))
        sin_fs = sum(1 for r in arts + inds if not r.get("final_status"))
        if sin_uid:
            fail(f"C{k}: {sin_uid} registros sin article_uid")
        if sin_fs:
            fail(f"C{k}: {sin_fs} registros sin final_status")

        art_auth = sum(1 for r in arts if r.get("authors"))
        ind_ok = sum(1 for r in inds if r.get("authors")
                     and TS_RE.match(str(r.get("timestamp") or "")))
        hist = [h for r in data["track_full"] for h in r.get("history", [])]
        hist_ok = sum(1 for h in hist if h.get("authors")
                      and TS_RE.match(str(h.get("timestamp") or "")))
        log(f"  clasificación: {len(arts)} artículos, {len(inds)} ind. sueltas | "
            f"authors: artículos {art_auth}/{len(arts)}, "
            f"sueltas utilizables {ind_ok}/{len(inds)}, history utilizables {hist_ok}/{len(hist)}")
        if art_auth == 0:
            log(f"  [AVISO] C{k}: NINGÚN artículo con authors — no aporta a la red de co-patrocinio (P15)")

        ts_bad = [str(r.get("timestamp")) for r in data["track_full"]
                  if r.get("timestamp") not in (None, "", "NA")
                  and not TS_RE.match(str(r.get("timestamp")))]
        if ts_bad:
            fail(f"C{k}: timestamps no parseables: {sorted(set(ts_bad))[:5]}")
        ts_na = sum(1 for r in data["track_full"] if r.get("timestamp") in (None, "", "NA"))
        if ts_na:
            log(f"  timestamps NA (top-level): {ts_na}")

        # validación de nombres en todos los niveles y archivos
        for d in data.values():
            for r in d:
                pools = [r.get("authors")] + [h.get("authors") for h in r.get("history", [])]
                for pool in pools:
                    if pool:
                        tot["autores"] += len(pool) if isinstance(pool, list) else 1
                        clean_authors(pool, collector=unmatched)

    rows = sum(1 for _ in open(COINCIDENCIAS, encoding="utf-8")) - 1
    log(f"coincidencias_comisiones.csv: {rows} filas" + ("" if rows == 498 else " [FALLA: esperaba 498]"))
    if rows != 498:
        failures.append("coincidencias != 498")

    log(f"\nTotales: {tot['articulos']} artículos, {tot['sueltas']} indicaciones sueltas, "
        f"{tot['autores']} menciones de autor")
    if unmatched:
        fail(f"{len(unmatched)} strings de autor NO resueltos (revisar/añadir a NAME_CORRECTIONS):")
        for name, n in sorted(unmatched.items(), key=lambda x: -x[1]):
            log(f"    {n:4d}  {name!r}")
    else:
        log("Validación de nombres: todos los autores-persona resuelven a los 154 canónicos.")

    log("\nRESULTADO: " + ("OK" if not failures else f"{len(failures)} FALLAS"))
    with open(f"{DATAVERSE_DIR}/QA-report.txt", "w", encoding="utf-8") as fh:
        fh.write("\n".join(lines) + "\n")
    sys.exit(1 if failures else 0)


if __name__ == "__main__":
    main()

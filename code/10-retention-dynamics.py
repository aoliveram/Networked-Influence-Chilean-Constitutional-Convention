"""
10-retention-dynamics.py — Dinámica de la retención sobre las ondas de comisión
(generalización del piloto C3, las 7 comisiones, con LOCF).

Para cada artículo trazado al borrador, su ESTADO en la onda w de su comisión es
el último `content_snapshot` con fecha <= la onda (génesis si aún no tiene
indicaciones). Si un artículo deja de modificarse antes de la última onda, su
valor SE PROPAGA hacia la derecha hasta la última onda de la comisión (decisión
del usuario, 2026-07-08). La similitud de cada estado se mide contra el texto
del artículo en el borrador final (coseno TF-IDF, vocabulario global).

Outputs: data/processed/retention_dynamics_locf.csv
         results/figures/retention_dynamics_all_commissions.{pdf,png}
"""

import csv
import json
import os
import re
import sys
import unicodedata
from collections import defaultdict

import numpy as np
from sklearn.feature_extraction.text import TfidfVectorizer
from sklearn.metrics.pairwise import cosine_similarity

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
from paths import (COMMISSIONS, DATA_PROCESSED, RESULTS_FIGURES,
                   borrador_path, track_full_path)  # noqa: E402

csv.field_size_limit(10_000_000)
TS = re.compile(r"^(\d{2})-(\d{2})(?:-(\d+))?$")


def normalize_text(text):
    text = (text or "").lower()
    text = unicodedata.normalize("NFC", text)
    text = re.sub(r"art[ií]culo\s+[\dx]+[\w]*\.?\s*-?\s*", "", text)
    text = re.sub(r"°", "", text)
    text = re.sub(r"[^\w\sáéíóúüñ-]", " ", text)
    return re.sub(r"\s+", " ", text).strip()


def ts_key(ts):
    m = TS.match(str(ts or ""))
    return (int(m.group(1)), int(m.group(2)), int(m.group(3) or 1)) if m else None


def day_key(mmdd):
    return (int(mmdd[:2]), int(mmdd[3:5]))


# --- ondas por comisión ---
waves = defaultdict(list)
for r in csv.DictReader(open(os.path.join(DATA_PROCESSED, "commission_waves.csv"), encoding="utf-8")):
    waves[int(r["commission"][1])].append(r["date_mmdd"])
for k in waves:
    waves[k] = sorted(waves[k], key=day_key)

# --- texto final por orden global ---
final_text = {}
for k in COMMISSIONS:
    for entry in json.load(open(borrador_path(k), encoding="utf-8")):
        m = re.match(r"\s*(\d+)\s*\.-", entry.get("article") or "")
        if m:
            final_text[int(m.group(1))] = entry["text"]

# --- artículos trazados con su orden final ---
orders = {r["article_uid"]: int(r["final_orders"].split(";")[0])
          for r in csv.DictReader(open(f"{DATA_PROCESSED}/track_article_outcomes.csv", encoding="utf-8"))
          if r["outcome_class"] in ("identico", "similar") and r["final_orders"]}

# --- trayectorias por (comisión, artículo): estado por onda con LOCF ---
records = []   # (k, uid, [state_idx por onda 0..n_k]) con índices al corpus
corpus = []


def add_text(t):
    corpus.append(normalize_text(t))
    return len(corpus) - 1


for k in COMMISSIONS:
    wdays = waves[k]
    for r in json.load(open(track_full_path(k), encoding="utf-8")):
        uid = r.get("article_uid")
        if uid not in orders or "action" in r or "titleuid" in r:
            continue
        snaps = sorted((h for h in r.get("history", [])
                        if h.get("content_snapshot") and ts_key(h.get("timestamp"))),
                       key=lambda h: ts_key(h["timestamp"]))
        gen_idx = add_text(r.get("text") or "")
        snap_items = [(ts_key(h["timestamp"])[:2], add_text(h["content_snapshot"])) for h in snaps]
        fin_idx = add_text(final_text[orders[uid]])
        # estado en cada onda (0 = génesis; onda i = último snapshot con día <= onda_i)
        states = [gen_idx]
        for d in wdays:
            dk = day_key(d)
            latest = gen_idx
            for sday, sidx in snap_items:
                if sday <= dk:
                    latest = sidx
                else:
                    break
            states.append(latest)
        records.append((k, uid, states, fin_idx))

print(f"Artículos trazados con trayectoria: {len(records)} "
      f"({sum(len(w) for w in waves.values())} ondas en total en 7 comisiones)")

# --- similitudes (una por estado distinto) ---
vec = TfidfVectorizer(ngram_range=(1, 2), sublinear_tf=True, max_features=20000, min_df=2)
M = vec.fit_transform(corpus)
sim_cache = {}


def sim(i, j):
    if (i, j) not in sim_cache:
        sim_cache[(i, j)] = float(cosine_similarity(M[i], M[j])[0, 0])
    return sim_cache[(i, j)]


rows = []
traj = defaultdict(list)  # (k, uid) -> [sim por onda]
for k, uid, states, fin_idx in records:
    for w, sidx in enumerate(states):
        s = sim(sidx, fin_idx)
        date = "genesis" if w == 0 else waves[k][w - 1]
        rows.append({"commission": f"C{k}", "article_uid": uid, "wave": w,
                     "date_mmdd": date, "sim_tfidf": round(s, 6)})
        traj[(k, uid)].append(s)

with open(os.path.join(DATA_PROCESSED, "retention_dynamics_locf.csv"), "w",
          newline="", encoding="utf-8") as fh:
    wr = csv.DictWriter(fh, fieldnames=["commission", "article_uid", "wave", "date_mmdd", "sim_tfidf"])
    wr.writeheader()
    wr.writerows(rows)

for k in COMMISSIONS:
    tk = [v for (kk, _), v in traj.items() if kk == k]
    if tk:
        print(f"  C{k}: n={len(tk)}, génesis media {np.mean([t[0] for t in tk]):.3f} "
              f"-> última onda {np.mean([t[-1] for t in tk]):.3f}")

# --- figura: todas las comisiones, líneas débiles + 7 medias gruesas ---
import matplotlib
matplotlib.use("Agg")
import matplotlib.pyplot as plt

CAT = {1: "#2a78d6", 2: "#1baf7a", 3: "#eda100", 4: "#008300",
       5: "#4a3aa7", 6: "#e34948", 7: "#e87ba4"}
INK, INK2, GRID, BASE, SURF = "#0b0b0b", "#52514e", "#e1e0d9", "#c3c2b7", "#fcfcfb"
plt.rcParams.update({"font.family": "sans-serif",
                     "font.sans-serif": ["Helvetica Neue", "Helvetica", "Arial", "DejaVu Sans"],
                     "figure.facecolor": SURF, "axes.facecolor": SURF, "savefig.facecolor": SURF})

fig, ax = plt.subplots(figsize=(7.6, 4.2))
for (k, uid), t in traj.items():
    ax.plot(range(len(t)), t, color=CAT[k], lw=0.5, alpha=0.13, zorder=2, rasterized=True)
n_by_k = {}
for k in COMMISSIONS:
    tk = [v for (kk, _), v in traj.items() if kk == k]
    n_by_k[k] = len(tk)
    if not tk:
        continue
    n_w = len(tk[0])
    mean_line = [np.mean([t[w] for t in tk]) for w in range(n_w)]
    ax.plot(range(n_w), mean_line, color=CAT[k], lw=2.4, zorder=4,
            solid_capstyle="round",
            label=f"C{k} (n = {len(tk)})")
ax.set_xticks(range(0, 9))
ax.set_xticklabels(["Genesis"] + [str(i) for i in range(1, 9)], fontsize=8.5)
ax.set_xlabel("Commission indication wave (last observed state carried forward)",
              fontsize=9, color=INK2)
ax.set_ylabel("TF-IDF cosine similarity to final draft", fontsize=9, color=INK2)
ax.set_ylim(0, 1.02)
ax.grid(axis="y", color=GRID, lw=0.6)
ax.set_axisbelow(True)
for side in ("top", "right"):
    ax.spines[side].set_visible(False)
ax.spines["left"].set_color(BASE)
ax.spines["bottom"].set_color(BASE)
ax.legend(frameon=False, fontsize=8, loc="lower right", ncol=2)
ax.set_title("Amendment trajectories toward the final text — all commissions\n"
             f"(traced articles, n = {len(records)}; thin lines = articles, thick lines = commission means)",
             fontsize=10.3, color=INK, loc="left", pad=8)
os.makedirs(RESULTS_FIGURES, exist_ok=True)
for ext in ("pdf", "png"):
    fig.savefig(os.path.join(RESULTS_FIGURES, f"retention_dynamics_all_commissions.{ext}"),
                dpi=300, bbox_inches="tight")
print("figura: retention_dynamics_all_commissions.pdf/.png")
print("--- Done ---")

"""
10-pilot-retention-dynamics.py — PILOTO (comisión C3): dinámica de la retención.

Para cada artículo de C3 trazado al borrador final, reconstruye la trayectoria
textual [génesis, snapshot tras cada indicación (content_snapshot, ordenado por
fecha), ...] y mide la similitud (TF-IDF y SBERT) de CADA estado contra el texto
final del borrador. Responde: ¿cómo se acerca (o aleja) un artículo de su forma
final a medida que se le agregan correcciones?

C3 se eligió por cobertura: 100% de sus 206 entradas de history en artículos
trazados tienen content_snapshot + timestamp (QA 2026-07-08).

Outputs: data/processed/pilot_retention_dynamics_C3.csv
         results/figures/pilot_retention_dynamics_C3.{pdf,png}
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
from paths import DATA_PROCESSED, RESULTS_FIGURES, track_full_path, borrador_path, COMMISSIONS  # noqa: E402

csv.field_size_limit(10_000_000)
K = 3  # comisión piloto
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


# --- texto final por orden global ---
final_text = {}
for kk in COMMISSIONS:
    for entry in json.load(open(borrador_path(kk), encoding="utf-8")):
        m = re.match(r"\s*(\d+)\s*\.-", entry.get("article") or "")
        if m:
            final_text[int(m.group(1))] = entry["text"]

# --- artículos trazados de C3 con su(s) orden(es) final(es) ---
orders = {r["article_uid"]: [int(o) for o in r["final_orders"].split(";")]
          for r in csv.DictReader(open(f"{DATA_PROCESSED}/track_article_outcomes.csv", encoding="utf-8"))
          if r["commission"] == f"C{K}" and r["outcome_class"] in ("identico", "similar")
          and r["final_orders"]}

trajectories = []  # (uid, [texto_0=génesis, snap_1, ...], [None, ts_1, ...], final)
for r in json.load(open(track_full_path(K), encoding="utf-8")):
    uid = r.get("article_uid")
    if uid not in orders or "action" in r:
        continue
    steps = sorted((h for h in r.get("history", [])
                    if h.get("content_snapshot") and ts_key(h.get("timestamp"))),
                   key=lambda h: ts_key(h["timestamp"]))
    texts = [r.get("text") or ""] + [h["content_snapshot"] for h in steps]
    tss = [None] + [h["timestamp"] for h in steps]
    fin = final_text[orders[uid][0]]
    trajectories.append((uid, texts, tss, fin))

print(f"C{K}: {len(trajectories)} artículos trazados; "
      f"{sum(len(t[1]) - 1 for t in trajectories)} pasos de indicación con snapshot")

# --- similitudes ---
corpus = [normalize_text(x) for _, texts, _, fin in trajectories for x in texts + [fin]]
vec = TfidfVectorizer(ngram_range=(1, 2), sublinear_tf=True, max_features=20000, min_df=2)
M = vec.fit_transform(corpus)

try:
    from sentence_transformers import SentenceTransformer
    sb_model = SentenceTransformer("paraphrase-multilingual-MiniLM-L12-v2")
    E = sb_model.encode(corpus, show_progress_bar=False, batch_size=32)
    has_sbert = True
except Exception as e:
    print(f"SBERT no disponible ({e})")
    E, has_sbert = None, False

rows = []
i = 0
for uid, texts, tss, fin in trajectories:
    idxs = list(range(i, i + len(texts)))
    fin_idx = i + len(texts)
    for step, (j, ts) in enumerate(zip(idxs, tss)):
        s_tfidf = float(cosine_similarity(M[j], M[fin_idx])[0, 0])
        s_sbert = (float(cosine_similarity(E[j].reshape(1, -1), E[fin_idx].reshape(1, -1))[0, 0])
                   if has_sbert else None)
        rows.append({"article_uid": uid, "step": step,
                     "stage": "genesis" if step == 0 else f"post-ind {step}",
                     "timestamp": ts or "", "n_steps": len(texts) - 1,
                     "sim_tfidf": round(s_tfidf, 6),
                     "sim_sbert": round(s_sbert, 6) if s_sbert is not None else ""})
    i += len(texts) + 1

out_csv = os.path.join(DATA_PROCESSED, f"pilot_retention_dynamics_C{K}.csv")
with open(out_csv, "w", newline="", encoding="utf-8") as fh:
    wr = csv.DictWriter(fh, fieldnames=list(rows[0].keys()))
    wr.writeheader()
    wr.writerows(rows)

# --- resumen ---
by_art = defaultdict(list)
for r in rows:
    by_art[r["article_uid"]].append(r)
first = [a[0]["sim_tfidf"] for a in by_art.values()]
last = [a[-1]["sim_tfidf"] for a in by_art.values()]
multi = [a for a in by_art.values() if len(a) > 1]
mono = sum(1 for a in multi
           if all(a[j + 1]["sim_tfidf"] >= a[j]["sim_tfidf"] - 1e-9 for j in range(len(a) - 1)))
print(f"similitud TF-IDF vs texto final — génesis: media {np.mean(first):.3f} | "
      f"último estado: media {np.mean(last):.3f}")
print(f"artículos con >=1 indicación: {len(multi)}; trayectoria monótonamente creciente: "
      f"{mono} ({100 * mono / len(multi):.0f}%)")
gain = [a[-1]["sim_tfidf"] - a[0]["sim_tfidf"] for a in multi]
print(f"ganancia media génesis->último (solo con indicaciones): {np.mean(gain):+.3f} "
      f"(mín {min(gain):+.3f}, máx {max(gain):+.3f})")

# --- figura (spaghetti + media por paso) ---
import matplotlib
matplotlib.use("Agg")
import matplotlib.pyplot as plt

INK, INK2, GRID, BASE, SURF = "#0b0b0b", "#52514e", "#e1e0d9", "#c3c2b7", "#fcfcfb"
BLUE, LIGHT = "#2a78d6", "#9ec5f4"
plt.rcParams.update({"font.family": "sans-serif",
                     "font.sans-serif": ["Helvetica Neue", "Helvetica", "Arial", "DejaVu Sans"],
                     "figure.facecolor": SURF, "axes.facecolor": SURF, "savefig.facecolor": SURF})
fig, ax = plt.subplots(figsize=(6.6, 3.6))
CAP = 8  # más allá quedan n<5 artículos: se trunca la vista y se anota
max_step = max(r["step"] for r in rows)
for a in by_art.values():
    pts = [(r["step"], r["sim_tfidf"]) for r in a if r["step"] <= CAP]
    ax.plot([x for x, _ in pts], [y for _, y in pts],
            color=LIGHT, lw=0.7, alpha=0.5, zorder=2)
mean_by_step = [(s, np.mean([r["sim_tfidf"] for r in rows if r["step"] == s]),
                 sum(1 for r in rows if r["step"] == s)) for s in range(CAP + 1)]
ax.plot([s for s, _, _ in mean_by_step], [m for _, m, _ in mean_by_step],
        color=BLUE, lw=2.2, zorder=3, label="Mean across articles")
for s, m, n in mean_by_step:
    ax.annotate(f"n={n}", (s, 0.02), ha="center", fontsize=7, color="#898781")
ax.annotate(f"view truncated at step {CAP}\n(max observed: {max_step}; n < 5 beyond)",
            (CAP, 0.30), ha="right", fontsize=7.5, color="#898781")
ax.set_xticks(range(CAP + 1))
ax.set_xticklabels(["Genesis"] + [str(s) for s in range(1, CAP + 1)], fontsize=8.5)
ax.set_xlabel("Amendment step (cumulative indications applied)", fontsize=9, color=INK2)
ax.set_ylim(0, 1.04)
ax.set_ylabel("TF-IDF cosine similarity to final draft", fontsize=9, color=INK2)
ax.grid(axis="y", color=GRID, lw=0.6)
ax.set_axisbelow(True)
for side in ("top", "right"):
    ax.spines[side].set_visible(False)
ax.spines["left"].set_color(BASE); ax.spines["bottom"].set_color(BASE)
ax.legend(frameon=False, fontsize=8.5, loc="lower right")
ax.set_title(f"How amendments move articles toward their final text — pilot, Commission {K}\n"
             f"({len(by_art)} traced articles; each line = one article)",
             fontsize=10, color=INK, loc="left", pad=8)
os.makedirs(RESULTS_FIGURES, exist_ok=True)
for ext in ("pdf", "png"):
    fig.savefig(os.path.join(RESULTS_FIGURES, f"pilot_retention_dynamics_C{K}.{ext}"),
                dpi=300, bbox_inches="tight")
print(f"figura: pilot_retention_dynamics_C{K}.pdf/.png")
print("--- Done ---")

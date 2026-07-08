"""
09-figures.py — Figuras del estudio (PDF + PNG 300 dpi, en inglés).

F1  initiatives_per_commission   barra corta y ancha: iniciativas por comisión
F2a bipartite_initiative         red bipartita documento(arriba)-convencional(abajo), unidad iniciativa
F2b bipartite_article            ídem, unidad artículo
F3  m2_waves_summary             construcción de ondas M2: 2x2, comisiones en eje x

Paleta: instancia de referencia del skill dataviz (orden categórico fijo 1-7,
validada; regla de alivio -> etiquetas directas). Convencionales ordenados por
posición ideológica (theta medio); documentos agrupados por comisión y ordenados
por la posición media de sus firmantes.
"""

import csv
import importlib.util
import os
import sys

import matplotlib
matplotlib.use("Agg")
import matplotlib.pyplot as plt
from matplotlib.collections import LineCollection
from matplotlib.patches import Patch

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
from paths import DATA_PROCESSED, RESULTS_FIGURES, COMMISSIONS  # noqa: E402

# --- paleta (referencia dataviz, modo claro) ---
CAT = {1: "#2a78d6", 2: "#1baf7a", 3: "#eda100", 4: "#008300",
       5: "#4a3aa7", 6: "#e34948", 7: "#e87ba4"}
INK, INK2, MUTED = "#0b0b0b", "#52514e", "#898781"
GRID, BASE, SURFACE = "#e1e0d9", "#c3c2b7", "#fcfcfb"
COMM_LABELS = {k: f"C{k}" for k in COMMISSIONS}
COMM_NAMES = {1: "Political System", 2: "Constitutional Principles", 3: "Form of the State",
              4: "Fundamental Rights", 5: "Environment", 6: "Justice Systems",
              7: "Knowledge Systems"}

plt.rcParams.update({
    "font.family": "sans-serif",
    "font.sans-serif": ["Helvetica Neue", "Helvetica", "Arial", "DejaVu Sans"],
    "text.color": INK, "axes.edgecolor": BASE, "axes.labelcolor": INK2,
    "xtick.color": INK2, "ytick.color": INK2, "axes.grid": False,
    "figure.facecolor": SURFACE, "axes.facecolor": SURFACE, "savefig.facecolor": SURFACE,
})


def save(fig, name):
    os.makedirs(RESULTS_FIGURES, exist_ok=True)
    for ext in ("pdf", "png"):
        fig.savefig(os.path.join(RESULTS_FIGURES, f"{name}.{ext}"), dpi=300,
                    bbox_inches="tight")
    plt.close(fig)
    print(f"  {name}.pdf/.png")


def despine(ax, keep_y=True):
    for side in ("top", "right"):
        ax.spines[side].set_visible(False)
    if not keep_y:
        ax.spines["left"].set_visible(False)


# --- datos ---
registry = list(csv.DictReader(open(os.path.join(DATA_PROCESSED, "initiative_registry.csv"),
                                    encoding="utf-8")))
outcomes = list(csv.DictReader(open(os.path.join(DATA_PROCESSED, "track_article_outcomes.csv"),
                                    encoding="utf-8")))
theta = {r["nombre_armonizado"]: float(r["theta_mean"])
         for r in csv.DictReader(open(os.path.join(DATA_PROCESSED, "emirt_summary_metrics.csv"),
                                      encoding="utf-8"))}

# =============================================================================
# F1 — iniciativas por comisión (barra corta y ancha)
# =============================================================================
counts = {k: sum(1 for r in registry if r["commission"] == f"C{k}" and int(r["n_firmantes"]) >= 2)
          for k in COMMISSIONS}
fig, ax = plt.subplots(figsize=(6.8, 1.9))
xs = list(COMMISSIONS)
ax.bar([COMM_LABELS[k] for k in xs], [counts[k] for k in xs],
       color=[CAT[k] for k in xs], width=0.62, zorder=3)
for i, k in enumerate(xs):
    ax.text(i, counts[k] + 3, str(counts[k]), ha="center", va="bottom",
            fontsize=8.5, color=INK)
ax.set_ylim(0, max(counts.values()) * 1.22)
ax.set_yticks([])
despine(ax, keep_y=False)
ax.spines["bottom"].set_color(BASE)
ax.tick_params(axis="x", length=0, labelsize=9)
ax.set_title("Constitutional initiatives per commission (≥ 2 delegate signers)",
             fontsize=10, color=INK, loc="left", pad=8)
save(fig, "initiatives_per_commission")

# =============================================================================
# F2 — bipartitas documento-convencional (dos niveles)
# =============================================================================
def bipartite(doc_rows, title, name):
    """doc_rows: lista de (commission_int, [firmantes])."""
    # convencionales ordenados por ideología (izquierda -> derecha)
    delegates = sorted({a for _, aus in doc_rows for a in aus})
    delegates.sort(key=lambda d: theta.get(d, 0.0))
    dx = {d: i / (len(delegates) - 1) for i, d in enumerate(delegates)}

    # documentos agrupados por comisión, ordenados por posición media de firmantes
    def doc_key(row):
        k, aus = row
        pos = [theta.get(a, 0.0) for a in aus]
        return (k, sum(pos) / len(pos))
    docs = sorted(doc_rows, key=doc_key)
    n_docs = len(docs)

    fig, ax = plt.subplots(figsize=(11, 4.6))
    segs, colors = [], []
    for j, (k, aus) in enumerate(docs):
        x_doc = j / (n_docs - 1)
        for a in aus:
            segs.append([(x_doc, 1.0), (dx[a], 0.0)])
            colors.append(CAT[k])
    lc = LineCollection(segs, colors=colors, linewidths=0.25, alpha=0.06, zorder=1)
    lc.set_rasterized(True)
    ax.add_collection(lc)

    for j, (k, _) in enumerate(docs):
        ax.plot(j / (n_docs - 1), 1.0, "o", ms=2.4, color=CAT[k], mec="none", zorder=3)
    ax.scatter([dx[d] for d in delegates], [0.0] * len(delegates), s=13,
               c=[theta.get(d, 0.0) for d in delegates], cmap="coolwarm",
               vmin=-2.2, vmax=2.2, edgecolors=SURFACE, linewidths=0.4, zorder=3)

    ax.text(-0.015, 1.0, f"Documents (n = {n_docs:,})", ha="right", va="center",
            fontsize=9, color=INK2)
    ax.text(-0.015, 0.0, "Delegates (n = 154)\nsorted by ideal point", ha="right",
            va="center", fontsize=9, color=INK2)
    handles = [Patch(facecolor=CAT[k], label=f"C{k} {COMM_NAMES[k]}") for k in COMMISSIONS]
    ax.legend(handles=handles, loc="upper center", bbox_to_anchor=(0.5, -0.06),
              ncol=4, frameon=False, fontsize=8, handlelength=1.1, handleheight=0.9)
    ax.set_xlim(-0.22, 1.01)
    ax.set_ylim(-0.12, 1.10)
    ax.axis("off")
    ax.set_title(title, fontsize=11, color=INK, loc="left", pad=6)
    save(fig, name)


init_rows = [(int(r["commission"][1]), r["firmantes"].split("; "))
             for r in registry if int(r["n_firmantes"]) >= 2]
bipartite(init_rows,
          "Genesis co-sponsorship as a bipartite network — initiative unit "
          "(each document = one constitutional initiative)",
          "bipartite_initiative")

art_rows = [(int(r["commission"][1]), r["authors"].split("; "))
            for r in outcomes if int(r["n_authors"]) >= 2]
bipartite(art_rows,
          "Genesis co-sponsorship as a bipartite network — article unit "
          "(each document = one genesis article)",
          "bipartite_article")

# =============================================================================
# F3 — construcción de ondas M2 (2x2)
# =============================================================================
# recalcular los conteos desde el snapshot con las funciones del script 00
spec = importlib.util.spec_from_file_location(
    "build00", os.path.join(os.path.dirname(os.path.abspath(__file__)),
                            "00-build_dynamic_networks.py"))
b00 = importlib.util.module_from_spec(spec)
spec.loader.exec_module(b00)

import json  # noqa: E402
from paths import track_full_path  # noqa: E402

waves_stats = {}
for k in COMMISSIONS:
    track = json.load(open(track_full_path(k), encoding="utf-8"))
    days, edge_events, d_dup, d_na, n_solo = b00.indication_events(track)
    waves_stats[k] = {"waves": len(days), "edges": len(edge_events),
                      "solo": n_solo, "dups": d_dup}

panels = [("waves", "Indication waves (report dates)"),
          ("edges", "Multi-author indication events (add ties)"),
          ("solo", "Single-author indications (no ties)"),
          ("dups", "Cross-article duplicates collapsed")]
fig, axes = plt.subplots(2, 2, figsize=(8.4, 5.2))
for ax, (key, subtitle) in zip(axes.flat, panels):
    vals = [waves_stats[k][key] for k in COMMISSIONS]
    ax.bar([COMM_LABELS[k] for k in COMMISSIONS], vals,
           color=[CAT[k] for k in COMMISSIONS], width=0.62, zorder=3)
    for i, v in enumerate(vals):
        ax.text(i, v + max(vals) * 0.03, str(v), ha="center", va="bottom",
                fontsize=7.5, color=INK)
    ax.set_ylim(0, max(vals) * 1.22 if max(vals) else 1)
    ax.set_yticks([])
    despine(ax, keep_y=False)
    ax.spines["bottom"].set_color(BASE)
    ax.tick_params(axis="x", length=0, labelsize=8.5)
    ax.set_title(subtitle, fontsize=9.5, color=INK, loc="left", pad=5)
fig.suptitle("Wave construction for Model 2, by commission", fontsize=11.5,
             color=INK, x=0.02, ha="left")
fig.tight_layout(rect=(0, 0, 1, 0.95))
save(fig, "m2_waves_summary")

print("--- Done ---")

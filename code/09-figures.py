"""
09-figures.py — Figuras del estudio (PDF + PNG 300 dpi, en inglés).

F1  initiatives_per_commission   iniciativas (sólido) sobre artículos (alpha 0.5)
F2a bipartite_initiative         bipartita documento-convencional, unidad iniciativa
F2b bipartite_article            ídem, unidad artículo
F3  C#_waves_summary             construcción de ondas M2: 2x2 con separador de columnas

Convenciones: paleta categórica de referencia (orden fijo C1-C7); gradiente
ideológico ROJO = izquierda (theta negativo; Baradit -1.38) a AZUL = derecha
(theta positivo; Marinovic +4.34), con blanco neutro en theta = 0 y colorbar.
"""

import csv
import importlib.util
import json
import os
import sys

import matplotlib
matplotlib.use("Agg")
import matplotlib.pyplot as plt
from matplotlib.cm import ScalarMappable
from matplotlib.collections import LineCollection
from matplotlib.colors import TwoSlopeNorm
from matplotlib.lines import Line2D
from matplotlib.patches import Patch

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
from paths import DATA_PROCESSED, RESULTS_FIGURES, COMMISSIONS, track_full_path  # noqa: E402

# --- paleta (referencia dataviz, modo claro) ---
CAT = {1: "#2a78d6", 2: "#1baf7a", 3: "#eda100", 4: "#008300",
       5: "#4a3aa7", 6: "#e34948", 7: "#e87ba4",
       0: "#898781"}  # 0 = iniciativas de la plataforma sin comisión asignada
INK, INK2, MUTED = "#0b0b0b", "#52514e", "#898781"
GRID, BASE, SURFACE = "#e1e0d9", "#c3c2b7", "#fcfcfb"
COMM_LABELS = {k: f"C{k}" for k in COMMISSIONS}
COMM_NAMES = {1: "Political System", 2: "Constitutional Principles", 3: "Form of the State",
              4: "Fundamental Rights", 5: "Environment", 6: "Justice Systems",
              7: "Knowledge Systems"}
# rojo = izquierda (theta bajo), azul = derecha (theta alto), neutro en 0
IDEO_CMAP = plt.get_cmap("coolwarm_r")
IDEO_NORM = TwoSlopeNorm(vcenter=0.0, vmin=-3.7, vmax=5.1)

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
# F1 — iniciativas (sólido) sobre artículos (translúcido), por comisión
# =============================================================================
init_counts = {k: sum(1 for r in registry if r["commission"] == f"C{k}" and int(r["n_firmantes"]) >= 2)
               for k in COMMISSIONS}
art_counts = {k: sum(1 for r in outcomes if r["commission"] == f"C{k}" and int(r["n_authors"]) >= 2)
              for k in COMMISSIONS}

fig, ax = plt.subplots(figsize=(6.8, 2.3))
xs = list(range(len(COMMISSIONS)))
top = max(art_counts.values())
ax.bar(xs, [art_counts[k] for k in COMMISSIONS], color=[CAT[k] for k in COMMISSIONS],
       width=0.62, alpha=0.5, zorder=2)
ax.bar(xs, [init_counts[k] for k in COMMISSIONS], color=[CAT[k] for k in COMMISSIONS],
       width=0.62, zorder=3)
for i, k in enumerate(COMMISSIONS):
    ax.text(i, art_counts[k] + top * 0.03, str(art_counts[k]), ha="center", va="bottom",
            fontsize=7.5, color=MUTED)
    ax.text(i, init_counts[k] + top * 0.03, str(init_counts[k]), ha="center", va="bottom",
            fontsize=8.5, color=INK, fontweight="bold")
ax.set_xticks(xs)
ax.set_xticklabels([COMM_LABELS[k] for k in COMMISSIONS], fontsize=9)
ax.set_ylim(0, top * 1.18)
ax.set_yticks([])
despine(ax, keep_y=False)
ax.spines["bottom"].set_color(BASE)
ax.tick_params(axis="x", length=0)
ax.set_title("Constitutional initiatives (solid, bold) and genesis articles (faded) per commission "
             "— $\\geq$ 2 delegate signers", fontsize=9.8, color=INK, loc="left", pad=8)
save(fig, "initiatives_per_commission")

# =============================================================================
# F2 — bipartitas documento-convencional (dos niveles)
# =============================================================================
def bipartite(doc_rows, title, name):
    delegates = sorted({a for _, aus in doc_rows for a in aus})
    delegates.sort(key=lambda d: theta.get(d, 0.0))  # izquierda política a la izquierda
    dx = {d: i / (len(delegates) - 1) for i, d in enumerate(delegates)}

    def doc_key(row):
        k, aus = row
        pos = [theta.get(a, 0.0) for a in aus]
        return (k, sum(pos) / len(pos))
    docs = sorted(doc_rows, key=doc_key)
    n_docs = len(docs)

    fig, ax = plt.subplots(figsize=(11, 4.8))
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
               c=[theta.get(d, 0.0) for d in delegates], cmap=IDEO_CMAP, norm=IDEO_NORM,
               edgecolors=SURFACE, linewidths=0.4, zorder=3)

    ax.text(-0.015, 1.0, f"Documents (n = {n_docs:,})", ha="right", va="center",
            fontsize=9, color=INK2)
    ax.text(-0.015, 0.0, "Delegates (n = 154)\nsorted by ideal point", ha="right",
            va="center", fontsize=9, color=INK2)
    ks_present = sorted({k for k, _ in docs if k}) + ([0] if any(k == 0 for k, _ in docs) else [])
    handles = [Patch(facecolor=CAT[k],
                     label=f"C{k} {COMM_NAMES[k]}" if k else "No commission assigned")
               for k in ks_present]
    ax.legend(handles=handles, loc="upper left", bbox_to_anchor=(0.02, -0.05),
              ncol=4, frameon=False, fontsize=8, handlelength=1.1, handleheight=0.9)
    # leyenda del gradiente ideológico
    cax = fig.add_axes([0.735, 0.085, 0.145, 0.022])
    cb = fig.colorbar(ScalarMappable(norm=IDEO_NORM, cmap=IDEO_CMAP), cax=cax,
                      orientation="horizontal", ticks=[-3.7, 0, 5.1])
    cb.ax.set_xticklabels(["Left", "0", "Right"], fontsize=7.5, color=INK2)
    cb.outline.set_edgecolor(BASE)
    cb.ax.set_title("Delegate ideal point ($\\theta$)", fontsize=8, color=INK2, pad=3)
    ax.set_xlim(-0.22, 1.01)
    ax.set_ylim(-0.12, 1.10)
    ax.axis("off")
    ax.set_title(title, fontsize=11, color=INK, loc="left", pad=6)
    save(fig, name)


init_rows = [(int(r["commission"][1]) if r["commission"] else 0, r["firmantes"].split("; "))
             for r in registry if int(r["n_firmantes"]) >= 2]
bipartite(init_rows, "Genesis co-sponsorship as a bipartite network — initiative unit",
          "bipartite_initiative")

art_rows = [(int(r["commission"][1]), r["authors"].split("; "))
            for r in outcomes if int(r["n_authors"]) >= 2]
bipartite(art_rows, "Genesis co-sponsorship as a bipartite network — article unit",
          "bipartite_article")

# =============================================================================
# F3 — construcción de ondas M2 (2x2, separador entre columnas)
# =============================================================================
spec = importlib.util.spec_from_file_location(
    "build00", os.path.join(os.path.dirname(os.path.abspath(__file__)),
                            "00-build_dynamic_networks.py"))
b00 = importlib.util.module_from_spec(spec)
spec.loader.exec_module(b00)

waves_stats = {}
for k in COMMISSIONS:
    track = json.load(open(track_full_path(k), encoding="utf-8"))
    days, edge_events, d_dup, d_na, n_solo = b00.indication_events(track)
    waves_stats[k] = {"waves": len(days), "edges": len(edge_events),
                      "solo": n_solo, "dups": d_dup}

panels = [("waves", "Indication waves (report dates)"),
          ("edges", "Multi-author indication events ($\\geq$ 2 delegate signers; add ties)"),
          ("solo", "Single-author indications (no ties)"),
          ("dups", "Cross-article duplicates collapsed")]
fig, axes = plt.subplots(2, 2, figsize=(8.6, 5.2))
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
    ax.set_title(subtitle, fontsize=9.3, color=INK, loc="left", pad=5)
fig.suptitle("Wave construction for Model 2, by commission", fontsize=11.5,
             color=INK, x=0.02, ha="left")
fig.tight_layout(rect=(0, 0, 1, 0.95))
fig.subplots_adjust(wspace=0.30)
save(fig, "C#_waves_summary")

# =============================================================================
# F4 — timeline de eventos multi-autor por comisión (niveles apilados, fechas 2022)
# =============================================================================
from datetime import date as _date

def to_date(mmdd):
    return _date(2022, int(mmdd[:2]), int(mmdd[3:5]))

events_by_day = {}
for k in COMMISSIONS:
    track = json.load(open(track_full_path(k), encoding="utf-8"))
    days, edge_events, _, _, _ = b00.indication_events(track)
    per_day = {d: 0 for d in days}
    for d, _au in edge_events:
        per_day[d] += 1
    events_by_day[k] = {to_date(d): n for d, n in sorted(per_day.items())}

VMAX = max(max(v.values()) for v in events_by_day.values() if v)  # 145
fig, ax = plt.subplots(figsize=(8.6, 5.4))
all_dates = sorted({d for v in events_by_day.values() for d in v})
x0, x1 = min(all_dates), max(all_dates)
span = (x1 - x0).days
LEVEL_H = 1.0
for row, k in enumerate(COMMISSIONS):           # C1 arriba
    base = (7 - row) * LEVEL_H
    ax.hlines(base, x0, x1, color=GRID, lw=0.8, zorder=1)
    ax.text(x0 - __import__("datetime").timedelta(days=9), base + 0.30, f"C{k}",
            ha="center", va="center", fontsize=9.5,
            color=CAT[k], fontweight="bold", transform=ax.transData)
    for d, n in events_by_day[k].items():
        if n == 0:
            ax.plot(d, base, "o", ms=2.6, color=MUTED, mec="none", zorder=3)
        else:
            h = 0.88 * n / VMAX
            ax.vlines(d, base, base + h, color=CAT[k], lw=3.4, zorder=3)
            ax.text(d, base + h + 0.05, str(n), ha="center", va="bottom",
                    fontsize=7, color=INK2)
import matplotlib.dates as mdates
ax.xaxis.set_major_locator(mdates.MonthLocator())
ax.xaxis.set_major_formatter(mdates.DateFormatter("%b 2022"))
ax.set_xlim(x0 - __import__("datetime").timedelta(days=13),
            x1 + __import__("datetime").timedelta(days=4))
ax.set_ylim(0.6, 8.2)
ax.set_yticks([])
for side in ("top", "right", "left"):
    ax.spines[side].set_visible(False)
ax.spines["bottom"].set_color(BASE)
ax.tick_params(axis="x", labelsize=8.5)
ax.set_title("Multi-author indication events per report date, by commission\n"
             f"(bar height $\\propto$ events, shared scale 0–{VMAX}; dots = reports with zero multi-author events)",
             fontsize=10.3, color=INK, loc="left", pad=8)
save(fig, "indication_events_timeline")

print("--- Done ---")

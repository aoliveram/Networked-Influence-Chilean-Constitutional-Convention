"""
47-success-by-ideology-figure.py  (comentario del autor 2026-07-21, punto 2)
Distribución del éxito (retención léxica media) de cada convencional, ordenada
por su posición política del primer mes (theta1 W-NOMINATE), coloreada por
conglomerado. Acompaña la sección 5.2 del reporte.

Output: results/figures/success_by_ideology.{pdf,png}
"""

import csv
import os
import sys

import matplotlib
matplotlib.use("Agg")
import matplotlib.pyplot as plt
import numpy as np

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
from paths import DATA_PROCESSED, DATA_RAW, RESULTS_FIGURES  # noqa: E402

INK, INK2, MUTED = "#0b0b0b", "#52514e", "#898781"
GRID, BASE, SURF = "#e1e0d9", "#c3c2b7", "#fcfcfb"
plt.rcParams.update({"font.family": "sans-serif",
                     "font.sans-serif": ["Helvetica Neue", "Helvetica", "Arial", "DejaVu Sans"],
                     "figure.facecolor": SURF, "axes.facecolor": SURF, "savefig.facecolor": SURF,
                     "text.color": INK, "axes.labelcolor": INK2,
                     "xtick.color": INK2, "ytick.color": INK2})

CONG_COLOR = {
    "Vamos por Chile": "#2a78d6",
    "Lista del Apruebo": "#1baf7a",
    "Apruebo Dignidad": "#e34948",
    "Lista del Pueblo": "#a8323e",
    "Escaños Reservados PPOO": "#4a3aa7",
    "Independientes No Neutrales": "#eda100",
    "Otras listas locales": "#898781",
}

theta = {r["nombre_armonizado"]: float(r["theta1_fm"])
         for r in csv.DictReader(open(os.path.join(DATA_PROCESSED, "ideal_points_2d_firstmonth.csv"),
                                      encoding="utf-8"))}
PIVOT = sorted(theta.values())[102]
cong = {}
for r in csv.DictReader(open(os.path.join(DATA_RAW, "electoral_lists.csv"), encoding="utf-8")):
    c = r["conglomerado"]
    if c == "REVISAR (lista local sin conglomerado)":
        c = "Otras listas locales"
    cong[r["nombre_armonizado"]] = c

pts = []
for r in csv.DictReader(open(os.path.join(DATA_PROCESSED, "integrated_dataset.csv"), encoding="utf-8")):
    if r["retention_all"] and r["nombre_armonizado"] in theta:
        pts.append((theta[r["nombre_armonizado"]], float(r["retention_all"]),
                    cong.get(r["nombre_armonizado"], "Otras listas locales")))

fig, ax = plt.subplots(figsize=(8.4, 3.7))
for c, col in CONG_COLOR.items():
    xs = [p[0] for p in pts if p[2] == c]
    ys = [p[1] for p in pts if p[2] == c]
    ax.scatter(xs, ys, s=26, color=col, alpha=0.8, edgecolors=SURF, linewidths=0.5,
               label=c, zorder=3)
# tendencia: media movil sobre bins de igual tamaño
pts.sort()
xv = np.array([p[0] for p in pts]); yv = np.array([p[1] for p in pts])
NB = 12
qs = np.quantile(xv, np.linspace(0, 1, NB + 1))
cx = [xv[(xv >= qs[b]) & (xv <= qs[b + 1])].mean() for b in range(NB)]
cy = [yv[(xv >= qs[b]) & (xv <= qs[b + 1])].mean() for b in range(NB)]
ax.plot(cx, cy, "-", color=INK, lw=1.4, alpha=0.65, zorder=4)
ax.axvline(PIVOT, color="#e34948", lw=1.3, ls="--", zorder=2)
ax.text(PIVOT + 0.06, 0.03, f"2/3 pivot ({PIVOT:.2f})", fontsize=8, color="#e34948")
ax.set_xlabel("First-month ideal point ($\\theta_1$, W-NOMINATE)", fontsize=9.5)
ax.set_ylabel("Success $y_i$ (mean lexical retention)", fontsize=9.5)
ax.grid(color=GRID, lw=0.5)
for side in ("top", "right"):
    ax.spines[side].set_visible(False)
ax.legend(fontsize=6.8, frameon=False, ncol=2, loc="upper right", handletextpad=0.2,
          columnspacing=0.8)
ax.set_title(f"Individual success by first-month position (n = {len(pts)} delegates; "
             "line = equal-count bin means)", fontsize=9.8, color=INK, loc="left", pad=8)
os.makedirs(RESULTS_FIGURES, exist_ok=True)
for ext in ("pdf", "png"):
    fig.savefig(os.path.join(RESULTS_FIGURES, f"success_by_ideology.{ext}"), dpi=300,
                bbox_inches="tight")
print("figura: success_by_ideology.pdf/.png")

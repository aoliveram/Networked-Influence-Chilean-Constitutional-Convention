"""
46-survival-by-position-figure.py  (comentario del autor 2026-07-21, punto 8.2)
Tasa de supervivencia de los artículos según la POSICIÓN MEDIA de su coalición
firmante: bins de igual tamaño sobre mean(theta1), con el pívot de 2/3 y los
cortes de cuartil de la Tabla 12. Acompaña a la Tabla 12 del reporte (F12).

Output: results/figures/survival_by_position.{pdf,png}
"""

import csv
import os
import sys

import matplotlib
matplotlib.use("Agg")
import matplotlib.pyplot as plt
import numpy as np

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
from paths import DATA_PROCESSED, RESULTS_FIGURES  # noqa: E402

INK, INK2, MUTED = "#0b0b0b", "#52514e", "#898781"
GRID, BASE, SURF = "#e1e0d9", "#c3c2b7", "#fcfcfb"
BLUE, RED, AMBER = "#2a78d6", "#e34948", "#c98a00"
plt.rcParams.update({"font.family": "sans-serif",
                     "font.sans-serif": ["Helvetica Neue", "Helvetica", "Arial", "DejaVu Sans"],
                     "figure.facecolor": SURF, "axes.facecolor": SURF, "savefig.facecolor": SURF,
                     "text.color": INK, "axes.labelcolor": INK2,
                     "xtick.color": INK2, "ytick.color": INK2})

theta = {r["nombre_armonizado"]: float(r["theta1_fm"])
         for r in csv.DictReader(open(os.path.join(DATA_PROCESSED, "ideal_points_2d_firstmonth.csv"),
                                      encoding="utf-8"))}
PIVOT = sorted(theta.values())[102]          # theta_(103)

arts = []
for r in csv.DictReader(open(os.path.join(DATA_PROCESSED, "m4_articles.csv"), encoding="utf-8")):
    S = r["coalicion"].split("|")
    vals = [theta[a] for a in S if a in theta]
    arts.append((float(np.mean(vals)), int(r["survive"])))
arts.sort()
mt = np.array([a[0] for a in arts])
sv = np.array([a[1] for a in arts])
n = len(arts)

# bins de igual tamaño (16) sobre la posición media
NB = 16
edges = np.quantile(mt, np.linspace(0, 1, NB + 1))
centers, rates, sizes = [], [], []
for b in range(NB):
    m = (mt >= edges[b]) & (mt <= edges[b + 1] if b == NB - 1 else mt < edges[b + 1])
    centers.append(mt[m].mean())
    rates.append(sv[m].mean())
    sizes.append(m.sum())

fig, ax = plt.subplots(figsize=(8.4, 4.1))
# bandas = terciles de los 154 CONVENCIONALES (no de las coaliciones): como el
# pivote es theta_(103) y 103 = 2/3 de 154, el corte T2/T3 ES el pivote
# (punto 1 del autor, 2026-07-22)
thv = np.array(sorted(theta.values()))
t = [mt.min(), thv[51], thv[102], mt.max()]
for i, lab in enumerate(["T1 (delegates 1-51)", "T2 (52-103)", "T3 (104-154)"]):
    m = (mt >= t[i]) & (mt <= t[i + 1])
    ax.axvspan(t[i], t[i + 1], color=GRID, alpha=0.35 if i % 2 == 0 else 0.12, zorder=0)
    if m.sum():
        ax.text((max(t[i], -0.9) + min(t[i + 1], 0.85)) / 2, 0.415,
                f"{lab}\n{100 * sv[m].mean():.0f}%", ha="center", va="top",
                fontsize=8, color=INK2)
# barandillas ARRIBA del plano del plot (coordenadas de ejes >1), en ingles
import matplotlib.transforms as mtransforms
trans = mtransforms.blended_transform_factory(ax.transData, ax.transAxes)
def bracket_row(y, cuts, label):
    tick = 0.022
    for i in range(len(cuts) - 1):
        x0, x1 = cuts[i] + 0.004, cuts[i + 1] - 0.004
        ax.plot([x0, x1], [y, y], color=RED, lw=1.0, alpha=0.45, zorder=5,
                transform=trans, clip_on=False)
        for xx in (x0, x1):
            ax.plot([xx, xx], [y - tick, y + tick], color=RED, lw=1.0, alpha=0.45,
                    zorder=5, transform=trans, clip_on=False)
    ax.text(cuts[0] - 0.02, y, label, ha="right", va="center", fontsize=7.2,
            color=RED, alpha=0.8, transform=trans)
bracket_row(1.12, list(np.quantile(mt, np.linspace(0, 1, 6))), "quintile cuts")
bracket_row(1.05, list(np.quantile(mt, np.linspace(0, 1, 5))), "quartile cuts")
ax.plot(centers, rates, "-", color=BLUE, lw=1.6, zorder=2)
ax.scatter(centers, rates, s=np.array(sizes) * 0.55, color=BLUE, edgecolors=SURF,
           linewidths=0.6, zorder=3)
ax.axvline(PIVOT, color=RED, lw=1.4, ls="--", zorder=4)
ax.text(PIVOT + 0.02, 0.36, f"2/3 pivot ($\\theta_{{(103)}}$ = {PIVOT:.2f})",
        fontsize=8.5, color=RED, va="top")
ax.axhline(sv.mean(), color=MUTED, lw=1.0, ls=":", zorder=1)
ax.text(mt.min(), sv.mean() + 0.008, f"overall rate = {sv.mean():.2f}",
        fontsize=7.5, color=MUTED, va="bottom")
ax.set_xlabel("Mean ideal point of the signing coalition ($\\bar\\theta_1$)", fontsize=9.5)
ax.set_ylabel("Article survival rate", fontsize=9.5)
ax.set_ylim(0, 0.5)
ax.grid(axis="y", color=GRID, lw=0.6)
for side in ("top", "right"):
    ax.spines[side].set_visible(False)
ax.set_title("Article survival by coalition position", fontsize=10.5, color=INK,
             loc="left", y=1.20)
os.makedirs(RESULTS_FIGURES, exist_ok=True)
for ext in ("pdf", "png"):
    fig.savefig(os.path.join(RESULTS_FIGURES, f"survival_by_position.{ext}"), dpi=300,
                bbox_inches="tight")
print("figura: survival_by_position.pdf/.png")

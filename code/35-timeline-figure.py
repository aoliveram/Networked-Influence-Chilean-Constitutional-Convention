"""
35-timeline-figure.py  (comentario del autor 2026-07-18, punto 1)
Línea de tiempo de la Convención Constitucional con eje temporal real:
hitos verificados (revisión IV.D4 contra el Diario Oficial) + las ventanas
de datos del estudio (estimación de theta, ingreso de iniciativas,
indicaciones en comisiones, votaciones de normas bajo 2/3).

Output: results/figures/cc_timeline.{pdf,png}
"""

import os
import sys
from datetime import date

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
from paths import RESULTS_FIGURES  # noqa: E402

import matplotlib
matplotlib.use("Agg")
import matplotlib.pyplot as plt
import matplotlib.dates as mdates

INK, INK2 = "#0b0b0b", "#52514e"
GRID, BASE, SURF = "#e1e0d9", "#c3c2b7", "#fcfcfb"
BLUE, RED, TEAL, PURPLE = "#2a78d6", "#e34948", "#1f8a70", "#7a5fc0"
plt.rcParams.update({"font.family": "sans-serif",
                     "font.sans-serif": ["Helvetica Neue", "Helvetica", "Arial", "DejaVu Sans"],
                     "figure.facecolor": SURF, "axes.facecolor": SURF, "savefig.facecolor": SURF,
                     "text.color": INK, "axes.labelcolor": INK2,
                     "xtick.color": INK2, "ytick.color": INK2})

# hitos (fecha, etiqueta, nivel de la etiqueta para evitar solapes)
MILESTONES = [
    (date(2021, 7, 4), "Installation\n(Loncón elected)", 1),
    (date(2021, 7, 14), "Provisional rules", 3.1),
    (date(2021, 10, 13), "General Rules published\n(incl. 8-16 sponsor rule)", 1),
    (date(2021, 10, 19), "Thematic commissions\ninstalled", 2.4),
    (date(2022, 2, 1), "Initiative deadline\n(156 filed that day)", 2.4),
    (date(2022, 2, 15), "First floor votes\nunder 2/3 rule", 1),
    (date(2022, 5, 14), "Draft constitution\ndelivered", 1.9),
    (date(2022, 7, 4), "Final text;\nConvention dissolves", 1),
]
# ventanas (inicio, fin, etiqueta, color, fila)
WINDOWS = [
    (date(2021, 7, 13), date(2021, 8, 12), "Ideology window\n($\\theta$: first-month votes,\nmajority rule)", PURPLE, 0),
    (date(2021, 11, 3), date(2022, 2, 1), "Initiatives filed\n(448 dated events)", BLUE, 1),
    (date(2022, 2, 8), date(2022, 5, 8), "Amendments in\ncommissions (M2 waves)", TEAL, 2),
    (date(2022, 2, 15), date(2022, 5, 14), "Constitutional norms\nvoted (2/3 = 103 votes)", RED, 3),
]
LABEL_RIGHT = {"Amendments in\ncommissions (M2 waves)",
               "Constitutional norms\nvoted (2/3 = 103 votes)"}

fig, ax = plt.subplots(figsize=(9.8, 3.4))
y_axis = 0.0
ax.axhline(y_axis, color=INK, lw=1.2, zorder=2)

for start, end, lab, color, row in WINDOWS:
    y0 = -0.55 - row * 0.52
    ax.barh(y0, (end - start).days, left=start, height=0.34, color=color, alpha=0.55,
            edgecolor="none", zorder=3)
    if lab in LABEL_RIGHT:
        ax.annotate(lab, (end, y0), fontsize=7.0, color=INK2, va="center", ha="left",
                    xytext=(6, 0), textcoords="offset points")
    else:
        ax.annotate(lab, (start, y0 - 0.30), fontsize=7.0, color=INK2, va="top", ha="left")

for d, lab, lvl in MILESTONES:
    ax.plot([d, d], [y_axis, 0.28 * lvl], color=BASE, lw=0.9, zorder=2)
    ax.plot(d, y_axis, "o", ms=5, color=INK, zorder=4)
    ax.annotate(lab, (d, 0.28 * lvl + 0.05), fontsize=7.0, color=INK, va="bottom",
                ha="center")

ax.set_ylim(-2.9, 1.35)
ax.set_xlim(date(2021, 6, 20), date(2022, 7, 20))
ax.xaxis.set_major_locator(mdates.MonthLocator())
ax.xaxis.set_major_formatter(mdates.DateFormatter("%b\n%Y"))
ax.set_yticks([])
for side in ("top", "right", "left"):
    ax.spines[side].set_visible(False)
ax.spines["bottom"].set_color(BASE)
ax.tick_params(labelsize=7.5)
ax.set_title("The Constitutional Convention, Jul 2021 – Jul 2022: milestones and the study's data windows",
             fontsize=10, color=INK, loc="left", pad=10)

fig.tight_layout()
os.makedirs(RESULTS_FIGURES, exist_ok=True)
for ext in ("pdf", "png"):
    fig.savefig(os.path.join(RESULTS_FIGURES, f"cc_timeline.{ext}"),
                dpi=300, bbox_inches="tight")
print("figura: cc_timeline.pdf/.png")

"""
40-data-infographic.py  (v2, comentarios del autor 6.1-6.15)
Infografia de datos: timeline sin titulos, barras neutras con tacheo rojo para
las votaciones (leve = reglas 1/2, denso = normas 2/3) + leyenda; comisiones
como puntos start/end; pipeline con cajas chicas sin color, iconos dibujados
encima de cada caja, sin franja de objetos derivados.

Output: results/figures/data_infographic.{pdf,png}
"""

import os
import sys
from datetime import date

import matplotlib
matplotlib.use("Agg")
import matplotlib.pyplot as plt
from matplotlib.patches import FancyBboxPatch, FancyArrowPatch, Rectangle, Circle

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
from paths import RESULTS_FIGURES  # noqa: E402

INK, INK2, MUTED = "#0b0b0b", "#52514e", "#898781"
GRID, BASE, SURF = "#e1e0d9", "#c3c2b7", "#fcfcfb"
RED = "#e34948"
plt.rcParams.update({"font.family": "sans-serif",
                     "font.sans-serif": ["Helvetica Neue", "Helvetica", "Arial", "DejaVu Sans"],
                     "figure.facecolor": SURF, "axes.facecolor": SURF, "savefig.facecolor": SURF,
                     "text.color": INK, "axes.labelcolor": INK2,
                     "hatch.linewidth": 1.0})

fig, ax = plt.subplots(figsize=(12.6, 7.6))
ax.set_xlim(0, 100)
ax.set_ylim(0, 100)
ax.axis("off")

ax.text(4.0, 96.5, "Chile's Constitutional Convention", fontsize=15, fontweight="bold", color=INK)
ax.plot([4.0, 36.0], [93.8, 93.8], color=RED, lw=1.4)

# ============================== TIMELINE (arriba) ==============================
T0, T1 = date(2021, 7, 1), date(2022, 7, 31)
X0, X1 = 6.0, 97.0
def tx(d):
    return X0 + (X1 - X0) * (d - T0).days / (T1 - T0).days

AXY = 58.0
ax.plot([X0 - 1, X1 + 1], [AXY, AXY], color=BASE, lw=1.4, zorder=1)
months = [date(2021, m, 1) for m in (7, 9, 11)] + [date(2022, m, 1) for m in (1, 3, 5, 7)]
labels = ["Jul '21", "Sep", "Nov", "Jan '22", "Mar", "May", "Jul '22"]
for d, lab in zip(months, labels):
    x = tx(d)
    ax.plot([x, x], [AXY - 0.9, AXY + 0.9], color=BASE, lw=1.2, zorder=1)
    ax.text(x, AXY - 3.6, lab, ha="center", va="top", fontsize=9.5, color=MUTED)

def span(d0, d1, y, label, hatch=None, above=True):
    x0, x1 = tx(d0), tx(d1)
    ax.add_patch(FancyBboxPatch((x0, y - 1.3), x1 - x0, 2.6,
                                boxstyle="round,pad=0.02,rounding_size=1.0",
                                fc=GRID, ec="none", zorder=3))
    if hatch:
        ax.add_patch(Rectangle((x0 + 0.3, y - 1.15), x1 - x0 - 0.6, 2.3,
                               fc="none", ec=RED, lw=0, hatch=hatch, zorder=4))
    ty = y + 2.6 if above else y - 2.7
    va = "bottom" if above else "top"
    ax.text((x0 + x1) / 2, ty, label, ha="center", va=va, fontsize=10, color=INK, zorder=5)

span(date(2021, 7, 5), date(2021, 8, 4), 67.0, "votes on rules (1/2 rule)", hatch="//")
span(date(2021, 11, 15), date(2022, 2, 1), 75.5, "constitutional initiatives")
span(date(2022, 1, 15), date(2022, 5, 14), 84.5, "amendment reports")
span(date(2022, 2, 15), date(2022, 5, 14), 67.0, "votes on norms (2/3 rule)", hatch="/////", above=False)

# hitos: comisiones como puntos start/end + entrega del borrador
for d, lab in ((date(2021, 10, 1), "start\ncommissions"), (date(2022, 5, 14), "end\ncommissions"),
               (date(2022, 7, 4), "draft\ndelivered")):
    x = tx(d)
    ax.plot([x], [AXY], marker="o", ms=7, mfc=INK, mec=SURF, mew=1.2, zorder=5)
    ax.text(x, AXY - 7.0, lab, ha="center", va="top", fontsize=9, color=INK2, linespacing=1.1)

# leyenda de tacheos (espacio vacio entre May y Jul '22, sobre el eje)
LX, LY = 82.0, 76.5
ax.add_patch(Rectangle((LX, LY - 1.0), 4.2, 2.0, fc=GRID, ec="none"))
ax.add_patch(Rectangle((LX, LY - 1.0), 4.2, 2.0, fc="none", ec=RED, lw=0, hatch="//"))
ax.text(LX + 5.2, LY, "roll-call votes, 1/2 rule", ha="left", va="center", fontsize=8.5, color=INK2)
ax.add_patch(Rectangle((LX, LY - 5.4), 4.2, 2.0, fc=GRID, ec="none"))
ax.add_patch(Rectangle((LX, LY - 5.4), 4.2, 2.0, fc="none", ec=RED, lw=0, hatch="/////"))
ax.text(LX + 5.2, LY - 4.4, "roll-call votes, 2/3 rule", ha="left", va="center", fontsize=8.5, color=INK2)

# ============================== PIPELINE (abajo) ==============================
ax.text(4.0, 41.5, "The pipeline", fontsize=13, fontweight="bold", color=INK)
ax.plot([4.0, 17.5], [39.2, 39.2], color=RED, lw=1.4)

BY = 20.0          # centro vertical de las cajas
BH = 11.0
BW = 11.5
boxes = [
    ("154", "convention\nmembers"),
    ("947", "initiatives"),
    ("1,809", "genesis\narticles"),
    ("4,707", "roll-call\nvotes"),
    ("498", "articles in\nthe draft"),
]
gaps = (100 - 2 * 5.0 - len(boxes) * BW) / (len(boxes) - 1)
xs = [5.0 + i * (BW + gaps) for i in range(len(boxes))]
arrow_labels = ["9,706\nsignatures", "articles\nproposed", "committee &\nplenary stages",
                "2/3 filter\n(27.5% survive)"]

for i, (x, (num, lab)) in enumerate(zip(xs, boxes)):
    ax.add_patch(FancyBboxPatch((x, BY - BH / 2), BW, BH,
                                boxstyle="round,pad=0.02,rounding_size=1.2",
                                fc=SURF, ec=BASE, lw=1.3, zorder=3))
    ax.text(x + BW / 2, BY + 2.2, num, ha="center", va="center", fontsize=17,
            fontweight="bold", color=INK, zorder=4)
    ax.text(x + BW / 2, BY - 2.6, lab, ha="center", va="center", fontsize=8.6,
            color=INK2, zorder=4, linespacing=1.05)
    if i < len(boxes) - 1:
        x0, x1 = x + BW + 0.6, xs[i + 1] - 0.6
        ax.add_patch(FancyArrowPatch((x0, BY), (x1, BY), arrowstyle="-|>",
                                     mutation_scale=12, color=INK2, lw=1.2, zorder=2))
        ax.text((x0 + x1) / 2, BY + 2.2, arrow_labels[i], ha="center", va="bottom",
                fontsize=8.6, color=MUTED, zorder=4, linespacing=1.1)

# ------------------------------ iconos (encima de cada caja) ------------------
IY = BY + BH / 2 + 3.2      # linea base de los iconos
IC = INK2

def icon_people(cx):
    for dx in (-2.2, 0.0, 2.2):
        ax.add_patch(Circle((cx + dx, IY + 3.0), 0.8, fc="none", ec=IC, lw=1.2, zorder=4))
        ax.add_patch(FancyBboxPatch((cx + dx - 1.05, IY), 2.1, 1.9,
                                    boxstyle="round,pad=0.02,rounding_size=0.9",
                                    fc="none", ec=IC, lw=1.2, zorder=4))

def icon_doc(cx):
    ax.add_patch(Rectangle((cx - 1.7, IY), 3.4, 4.4, fc="none", ec=IC, lw=1.2, zorder=4))
    for j in range(4):
        ax.plot([cx - 1.1, cx + 1.1], [IY + 3.5 - j * 0.9] * 2, color=IC, lw=0.9, zorder=4)

def icon_arts(cx):
    ax.add_patch(Rectangle((cx - 1.7, IY), 3.4, 4.4, fc="none", ec=IC, lw=1.2, zorder=4))
    for yy in (IY + 3.3, IY + 1.9):
        ax.text(cx - 1.05, yy, "ART", ha="left", va="center", fontsize=5.6,
                color=IC, fontweight="bold", zorder=4)
        ax.plot([cx + 0.25, cx + 1.15], [yy] * 2, color=IC, lw=0.9, zorder=4)
    ax.plot([cx - 1.05, cx + 1.15], [IY + 0.7] * 2, color=IC, lw=0.9, zorder=4)

def icon_vote(cx):
    # urna con ranura y papeleta entrando
    ax.add_patch(Rectangle((cx - 2.0, IY), 4.0, 2.5, fc="none", ec=IC, lw=1.2, zorder=4))
    ax.plot([cx - 0.9, cx + 0.9], [IY + 2.5] * 2, color=IC, lw=2.2, zorder=5)
    ax.add_patch(Rectangle((cx - 0.6, IY + 2.7), 1.2, 1.6, fc=SURF, ec=IC, lw=1.0, zorder=4))
    ax.plot([cx - 0.35, cx - 0.08, cx + 0.38], [IY + 3.45, IY + 3.1, IY + 3.95],
            color=IC, lw=1.0, zorder=5)

def icon_draft(cx):
    ax.add_patch(Rectangle((cx - 1.0, IY + 0.5), 3.0, 3.9, fc=SURF, ec=IC, lw=1.0, zorder=3))
    ax.add_patch(Rectangle((cx - 1.7, IY), 3.0, 3.9, fc=SURF, ec=IC, lw=1.2, zorder=4))
    ax.plot([cx - 1.1, cx - 0.6, cx + 0.5], [IY + 1.7, IY + 1.0, IY + 2.8],
            color=IC, lw=1.5, zorder=5)

icon_people(xs[0] + BW / 2)
icon_doc(xs[1] + BW / 2)
icon_arts(xs[2] + BW / 2)
icon_vote(xs[3] + BW / 2)
icon_draft(xs[4] + BW / 2)

for ext in ("pdf", "png"):
    fig.savefig(os.path.join(RESULTS_FIGURES, f"data_infographic.{ext}"),
                dpi=300, bbox_inches="tight")
print("data_infographic.{pdf,png} listos")

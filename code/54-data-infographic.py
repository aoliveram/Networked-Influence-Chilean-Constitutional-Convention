"""
54-data-infographic.py  (Manchester / Political Networks 2026)
Infografia de datos en una lamina (ingles): arriba la linea de tiempo del
proceso (jul-2021 a jul-2022) con las ventanas que usa cada pieza del estudio;
abajo el pipeline persona -> firma -> iniciativa -> articulo -> voto -> borrador
con los conteos, y la franja de objetos derivados.

Output: results/figures/data_infographic.{pdf,png}
"""

import os
import sys
from datetime import date

import matplotlib
matplotlib.use("Agg")
import matplotlib.pyplot as plt
from matplotlib.patches import FancyBboxPatch, FancyArrowPatch

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
from paths import RESULTS_FIGURES  # noqa: E402

INK, INK2, MUTED = "#0b0b0b", "#52514e", "#898781"
GRID, BASE, SURF = "#e1e0d9", "#c3c2b7", "#fcfcfb"
RED = "#e34948"
REDBG = "#fbeaea"
plt.rcParams.update({"font.family": "sans-serif",
                     "font.sans-serif": ["Helvetica Neue", "Helvetica", "Arial", "DejaVu Sans"],
                     "figure.facecolor": SURF, "axes.facecolor": SURF, "savefig.facecolor": SURF,
                     "text.color": INK, "axes.labelcolor": INK2,
                     "xtick.color": INK2, "ytick.color": INK2})

fig, ax = plt.subplots(figsize=(12.6, 7.0))
ax.set_xlim(0, 100)
ax.set_ylim(0, 100)
ax.axis("off")

# ============================== TIMELINE (arriba) ==============================
T0, T1 = date(2021, 7, 1), date(2022, 7, 31)
X0, X1 = 6.0, 97.0
def tx(d):
    return X0 + (X1 - X0) * (d - T0).days / (T1 - T0).days

AXY = 62.0
ax.plot([X0 - 1, X1 + 1], [AXY, AXY], color=BASE, lw=1.4, zorder=1)
months = [date(2021, m, 1) for m in (7, 9, 11)] + [date(2022, m, 1) for m in (1, 3, 5, 7)]
labels = ["Jul '21", "Sep", "Nov", "Jan '22", "Mar", "May", "Jul '22"]
for d, lab in zip(months, labels):
    x = tx(d)
    ax.plot([x, x], [AXY - 0.9, AXY + 0.9], color=BASE, lw=1.2, zorder=1)
    ax.text(x, AXY - 3.6, lab, ha="center", va="top", fontsize=9, color=MUTED)

def span(d0, d1, y, label, color, txtcolor, bold=False, above=True, sub=None):
    x0, x1 = tx(d0), tx(d1)
    ax.add_patch(FancyBboxPatch((x0, y - 1.1), x1 - x0, 2.2,
                                boxstyle="round,pad=0.02,rounding_size=0.9",
                                fc=color, ec="none", zorder=3))
    ty = y + 2.1 if above else y - 2.2
    va = "bottom" if above else "top"
    ax.text((x0 + x1) / 2, ty, label, ha="center", va=va, fontsize=9.2,
            color=txtcolor, fontweight="bold" if bold else "normal", zorder=4)
    if sub:
        ax.text((x0 + x1) / 2, ty + (3.4 if above else -3.4), sub, ha="center",
                va=va, fontsize=8.2, color=MUTED, zorder=4)

# hitos puntuales
for d, lab in ((date(2021, 7, 4), "Convention\nconvenes"), (date(2022, 7, 4), "Draft\ndelivered")):
    x = tx(d)
    ax.plot([x], [AXY], marker="o", ms=7, mfc=INK, mec=SURF, mew=1.2, zorder=5)
    ax.text(x, AXY - 7.2, lab, ha="center", va="top", fontsize=8.6, color=INK2)

# ventanas (alturas escalonadas sobre el eje)
span(date(2021, 7, 5), date(2021, 8, 4), 70.5,
     "First month: 147 roll calls", REDBG, INK, bold=True, above=True)
ax.text(tx(date(2021, 7, 20)), 76.4, "pre-network ideology\n(2-D W-NOMINATE)",
        ha="center", va="bottom", fontsize=8.2, color=MUTED, linespacing=1.1)
span(date(2021, 10, 1), date(2022, 5, 14), 66.0,
     "7 thematic commissions at work", GRID, INK2, above=False)
span(date(2021, 11, 15), date(2022, 2, 1), 78.5,
     "947 constitutional initiatives", GRID, INK2, above=True)
span(date(2022, 1, 15), date(2022, 5, 14), 87.0,
     "amendment reports = network waves", GRID, INK2, above=True)
span(date(2022, 2, 15), date(2022, 5, 14), 70.5,
     "two-thirds era: plenary votes on norms", RED, RED, bold=True, above=False)

ax.text(X0 - 1, 95.5, "One year, fully dated", fontsize=13, fontweight="bold", color=INK)
ax.text(X0 - 1, 91.8, "Chilean Constitutional Convention, Jul 2021 – Jul 2022",
        fontsize=9.5, color=INK2)

# ============================== PIPELINE (abajo) ==============================
BY = 30.0          # centro vertical de las cajas
BH = 15.5          # alto de caja
boxes = [
    ("154", "convention\nmembers", None),
    ("947", "initiatives", "8–16 sponsors each\n100% dated"),
    ("1,809", "genesis\narticles", None),
    ("4,707", "roll-call\nvotes", "two-thirds rule:\n103 of 154"),
    ("498", "articles in\nthe draft", "27.5% survive"),
]
BW = 15.0
gaps = (100 - 2 * 4.0 - len(boxes) * BW) / (len(boxes) - 1)
xs = [4.0 + i * (BW + gaps) for i in range(len(boxes))]

arrow_labels = ["9,706\nsignatures", "articles\nproposed", "committee &\nplenary stages", "⅔ filter"]

for i, (x, (num, lab, sub)) in enumerate(zip(xs, boxes)):
    hot = num in ("947", "498")
    ax.add_patch(FancyBboxPatch((x, BY - BH / 2), BW, BH,
                                boxstyle="round,pad=0.02,rounding_size=1.4",
                                fc=REDBG if hot else "#f1f0ea",
                                ec=RED if hot else BASE, lw=1.2, zorder=3))
    ax.text(x + BW / 2, BY + 3.4, num, ha="center", va="center", fontsize=21,
            fontweight="bold", color=RED if hot else INK, zorder=4)
    ax.text(x + BW / 2, BY - 2.6, lab, ha="center", va="center", fontsize=9.0,
            color=INK2, zorder=4, linespacing=1.05)
    if sub:
        ax.text(x + BW / 2, BY - BH / 2 - 1.6, sub, ha="center", va="top",
                fontsize=8.0, color=MUTED, zorder=4, linespacing=1.1)
    if i < len(boxes) - 1:
        x0, x1 = x + BW + 0.5, xs[i + 1] - 0.5
        ax.add_patch(FancyArrowPatch((x0, BY), (x1, BY), arrowstyle="-|>",
                                     mutation_scale=13, color=INK2, lw=1.3, zorder=2))
        ax.text((x0 + x1) / 2, BY + 2.0, arrow_labels[i], ha="center", va="bottom",
                fontsize=7.8, color=MUTED, zorder=4, linespacing=1.05)

ax.text(4.0, 44.5, "The pipeline: every signature, article and vote, linked",
        fontsize=13, fontweight="bold", color=INK)

# franja de objetos derivados
dy = 9.5
ax.plot([4.0, 96.0], [dy + 4.6, dy + 4.6], color=GRID, lw=1.0)
derived = [
    "7 commission co-sponsorship\nnetworks, wave by wave",
    "dynamic ideal points (dynIRT),\n91 periods",
    "article-level traceability:\ngenesis text $\\rightarrow$ draft text",
]
from matplotlib.patches import Rectangle
for x, t in zip((4.0, 38.5, 71.0), derived):
    ax.add_patch(Rectangle((x, dy + 0.4), 1.1, 1.6, fc=RED, ec="none", zorder=4))
    ax.text(x + 2.2, dy + 2.0, t, fontsize=8.8, color=INK2, va="top", linespacing=1.15)
ax.text(4.0, dy + 6.4, "Derived objects", fontsize=9.0, color=MUTED, va="bottom",
        fontweight="bold")

for ext in ("pdf", "png"):
    fig.savefig(os.path.join(RESULTS_FIGURES, f"data_infographic.{ext}"),
                dpi=300, bbox_inches="tight")
print("data_infographic.{pdf,png} listos")

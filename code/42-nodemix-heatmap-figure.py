"""
42-nodemix-heatmap-figure.py  (E4 -> figura para Manchester)
Heatmap de la matriz de mezcla por bloque politico (b2twostar sobre espec S3,
code/41-bipartite-nodemix-boot.R): 7 small multiples (uno por comision), triangulo inferior 5x5,
color = z del bootstrap (divergente azul-neutro-rojo, recortado en |z|<=4),
estrellas = significancia, celdas vacias (par nunca observado) en gris.

Output: results/figures/nodemix_heatmap.{pdf,png}
"""

import csv
import os
import sys

import matplotlib
matplotlib.use("Agg")
import matplotlib.pyplot as plt
import numpy as np
from matplotlib.colors import LinearSegmentedColormap, Normalize
from matplotlib.cm import ScalarMappable

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
from paths import RESULTS_TABLES, RESULTS_FIGURES  # noqa: E402

INK, INK2, MUTED = "#0b0b0b", "#52514e", "#898781"
GRID, BASE, SURF = "#e1e0d9", "#c3c2b7", "#fcfcfb"
BLUE, RED = "#2a78d6", "#e34948"
plt.rcParams.update({"font.family": "sans-serif",
                     "font.sans-serif": ["Helvetica Neue", "Helvetica", "Arial", "DejaVu Sans"],
                     "figure.facecolor": SURF, "axes.facecolor": SURF, "savefig.facecolor": SURF,
                     "text.color": INK, "axes.labelcolor": INK2})

# orden politico: espectro primero, los dos grupos fuera-del-eje al final
ORDER = ["Izquierda", "CentroIzq", "Derecha", "PPOO", "Otras"]
SHORT = {"Izquierda": "Left", "CentroIzq": "Ctr-left", "Derecha": "Right",
         "PPOO": "Res. seats", "Otras": "Others"}
COMM = {1: "Political system", 2: "Const. principles", 3: "Form of the state",
        4: "Fundamental rights", 5: "Environment", 6: "Justice systems",
        7: "Knowledge systems"}

rows = list(csv.DictReader(open(os.path.join(RESULTS_TABLES, "M1_bipartite_nodemix_boot.csv"),
                                encoding="utf-8")))
mix = [r for r in rows if r["term"].startswith("b2twostar")]

def cell(t):
    p = t.replace("b2twostar.bloque.modo2.bloque.", "").split(".")
    return p[0], p[1]

cmap = LinearSegmentedColormap.from_list("div", [BLUE, "#f2f1ec", RED])
norm = Normalize(vmin=-4, vmax=4)

fig, axes = plt.subplots(2, 4, figsize=(13.2, 7.2))
for ax in axes.flat:
    ax.set_axis_off()

for k in range(1, 8):
    ax = axes.flat[k - 1]
    ax.set_axis_on()
    ax.set_xlim(0, 5); ax.set_ylim(0, 5)
    ax.set_aspect("equal")
    ax.invert_yaxis()
    ax.set_xticks([]); ax.set_yticks([])
    for s in ax.spines.values():
        s.set_visible(False)
    sub = {cell(r["term"]): r for r in mix if r["commission"] == f"C{k}"}
    for i, gi in enumerate(ORDER):          # fila
        for j, gj in enumerate(ORDER[:i + 1]):   # columna (triangulo inferior)
            r = sub.get((gi, gj)) or sub.get((gj, gi))
            x, y = j, i
            if r is None or r["estimate"] in ("", "NA"):
                ax.add_patch(plt.Rectangle((x + .04, y + .04), .92, .92, fc=GRID,
                                           ec=SURF, lw=1.5))
                ax.text(x + .5, y + .5, "—", ha="center", va="center",
                        fontsize=8, color=MUTED)
                continue
            est, se = float(r["estimate"]), float(r["se_boot"])
            z = est / se if se > 0 else 0.0
            p = float(r["p_boot"])
            ax.add_patch(plt.Rectangle((x + .04, y + .04), .92, .92,
                                       fc=cmap(norm(np.clip(z, -4, 4))),
                                       ec=SURF, lw=1.5))
            star = "***" if p < .001 else "**" if p < .01 else "*" if p < .05 else ""
            dark = abs(np.clip(z, -4, 4)) > 2.6
            ax.text(x + .5, y + .40, f"{est:+.2f}", ha="center", va="center",
                    fontsize=7.6, color=SURF if dark else INK)
            if star:
                ax.text(x + .5, y + .74, star, ha="center", va="center",
                        fontsize=7.0, color=SURF if dark else INK2)
    for i, g in enumerate(ORDER):
        ax.text(-0.18, i + .5, SHORT[g], ha="right", va="center", fontsize=6.8, color=INK2)
        ax.text(i + .55, 5.22, SHORT[g], ha="right", va="top",
                fontsize=6.8, color=INK2, rotation=45)
    ax.set_title(f"C{k} — {COMM[k]}", fontsize=9.5, color=INK, pad=6)

# panel 8: colorbar + lectura
ax8 = axes.flat[7]
ax8.set_axis_off()
cax = fig.add_axes([0.775, 0.16, 0.16, 0.035])
cb = fig.colorbar(ScalarMappable(norm=norm, cmap=cmap), cax=cax, orientation="horizontal")
cb.set_label("z (initiative bootstrap, B = 500)", fontsize=8, color=INK2)
cb.ax.tick_params(labelsize=7, colors=INK2)
cb.outline.set_visible(False)
ax8.text(0.02, 0.92, "How to read", fontsize=9.5, fontweight="bold", color=INK,
         transform=ax8.transAxes)
ax8.text(0.02, 0.84,
         "Each cell: log-odds of joining an initiative\n"
         "per current signer of that bloc pair,\n"
         "net of ideology (2-D, continuous), district,\n"
         "profile and structural terms.\n"
         "Red = pair over-represented; blue = avoided;\n"
         "gray = pair never observed.",
         fontsize=8, color=INK2, va="top", transform=ax8.transAxes, linespacing=1.35)

fig.suptitle("Who co-signs with whom, net of everything else — political-bloc mixing "
             "(bipartite ERGM, MPLE + bootstrap)", fontsize=12, fontweight="bold",
             color=INK, y=0.985)
fig.subplots_adjust(left=0.05, right=0.985, top=0.87, bottom=0.06,
                    hspace=0.42, wspace=0.30)

for ext in ("pdf", "png"):
    fig.savefig(os.path.join(RESULTS_FIGURES, f"nodemix_heatmap.{ext}"),
                dpi=300, bbox_inches="tight")
print("nodemix_heatmap.{pdf,png} listos")

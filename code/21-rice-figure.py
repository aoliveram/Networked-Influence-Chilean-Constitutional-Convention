"""
21-rice-figure.py  (respuesta a revisión: D2.2)
F10: serie mensual del índice de Rice por conglomerado de lista, jul-2021 a
jun-2022. Un partido sostiene la cohesión en el tiempo; una etiqueta electoral
sin organización la ve decaer. Línea vertical: inicio de las votaciones de
normas constitucionales bajo 2/3 (15-feb-2022).

Input:  results/tables/rice_monthly.csv  (de 17-listas-rice.R)
Output: results/figures/rice_cohesion_monthly.{pdf,png}
"""

import csv
import os
import sys
from collections import defaultdict

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
from paths import RESULTS_TABLES, RESULTS_FIGURES  # noqa: E402

import matplotlib
matplotlib.use("Agg")
import matplotlib.pyplot as plt

INK, INK2 = "#0b0b0b", "#52514e"
GRID, BASE, SURF = "#e1e0d9", "#c3c2b7", "#fcfcfb"
plt.rcParams.update({"font.family": "sans-serif",
                     "font.sans-serif": ["Helvetica Neue", "Helvetica", "Arial", "DejaVu Sans"],
                     "figure.facecolor": SURF, "axes.facecolor": SURF, "savefig.facecolor": SURF,
                     "text.color": INK, "axes.labelcolor": INK2,
                     "xtick.color": INK2, "ytick.color": INK2})

# colores por identidad ideológica del proyecto (azul=derecha, rojo=izquierda);
# paleta validada CVD (deutan/protan dE76 >= 14 en todos los pares)
STYLE = {
    "Vamos por Chile": ("#2a78d6", "Vamos por Chile"),
    "Lista del Apruebo": ("#7a5fc0", "Lista del Apruebo"),
    "Apruebo Dignidad": ("#e34948", "Apruebo Dignidad"),
    "Lista del Pueblo": ("#a05a2c", "Lista del Pueblo"),
    "Escaños Reservados PPOO": ("#1f8a70", "Reserved seats (PPOO)"),
}

series = defaultdict(dict)
months = set()
with open(os.path.join(RESULTS_TABLES, "rice_monthly.csv"), encoding="utf-8") as f:
    for r in csv.DictReader(f):
        series[r["conglomerado"]][r["month"]] = float(r["rice"])
        months.add(r["month"])
months = sorted(months)
# posición calendario real (meses desde jul-2021): nov-2021 queda como hueco
# visible (mes con <10 roll-calls, filtrado en 17)
xi = {m: (int(m[:4]) - 2021) * 12 + int(m[5:]) - 7 for m in months}

fig, ax = plt.subplots(figsize=(8.6, 3.9))
for congl, (color, label) in STYLE.items():
    pts = sorted((xi[m], v) for m, v in series[congl].items())
    xs, ys = zip(*pts)
    ax.plot(xs, ys, color=color, lw=2, zorder=3, solid_capstyle="round")
    ax.annotate(label, (xs[-1], ys[-1]), xytext=(6, 0), textcoords="offset points",
                fontsize=7.8, color=INK2, va="center")

x_feb = xi["2022-02"] - 0.5   # las votaciones de normas parten el 15-feb-2022
ax.axvline(x_feb, color=INK, lw=1.0, ls="--", zorder=2)
ax.text(x_feb + 0.08, 0.505, "constitutional norms\nvoted under 2/3 rule",
        fontsize=7.5, color=INK2, va="bottom")

LAB = {"2021-07": "Jul", "2021-08": "Aug", "2021-09": "Sep", "2021-10": "Oct",
       "2021-11": "Nov", "2021-12": "Dec", "2022-01": "Jan '22", "2022-02": "Feb",
       "2022-03": "Mar", "2022-04": "Apr", "2022-05": "May", "2022-06": "Jun"}
all_months = sorted(set(months) | {"2021-11"})
ax.set_xticks([xi.get(m, (int(m[:4]) - 2021) * 12 + int(m[5:]) - 7) for m in all_months])
ax.set_xticklabels([LAB[m] for m in all_months], fontsize=8)
ax.set_ylim(0.5, 1.0)
ax.set_ylabel("Rice cohesion index (monthly mean)", fontsize=9)
ax.set_xlabel("Month (Jul 2021 – Jun 2022)", fontsize=9)
ax.set_title("Voting cohesion by electoral list: no list sustains discipline beyond its ideological neighborhood",
             fontsize=10, color=INK, loc="left", pad=8)
ax.grid(axis="y", color=GRID, lw=0.6)
ax.set_axisbelow(True)
for side in ("top", "right"):
    ax.spines[side].set_visible(False)
ax.spines["left"].set_color(BASE)
ax.spines["bottom"].set_color(BASE)
ax.tick_params(labelsize=8.5)
ax.set_xlim(-0.4, max(xi.values()) + 3.2)   # aire para etiquetas directas
ax.legend(handles=[plt.Line2D([], [], color=c, lw=2, label=l) for c, l in STYLE.values()],
          fontsize=7.5, frameon=False, loc="lower left", ncol=2)

fig.tight_layout()
os.makedirs(RESULTS_FIGURES, exist_ok=True)
for ext in ("pdf", "png"):
    fig.savefig(os.path.join(RESULTS_FIGURES, f"rice_cohesion_monthly.{ext}"),
                dpi=300, bbox_inches="tight")
print("figura: rice_cohesion_monthly.pdf/.png")

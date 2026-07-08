"""
11-m2-motivation-figures.py — Figuras de motivación descriptiva de M2 (panel b).

FB1 m2_positions_dynamics       (arriba) trayectorias de theta_{i,t} de los 154
                                sobre las 91 fechas de votación, gradiente rojo=
                                izquierda/azul=derecha + medias de bloque;
                                (abajo) distribución de |Delta theta| por onda.
FB2 theta_dynamics_by_commission  mismas trayectorias (líneas débiles, color de
                                la comisión del convencional) + 7 medias gruesas.

Membresía de comisión: data/raw/commission_membership.csv (snapshot). Si no
existe, se construye una vez desde el texto `integracion_comisiones` del
scraping BCN (CPT) con keywords temáticas; los no matcheados se asignan por
actividad (comisión donde firmaron más iniciativas) y quedan marcados.
"""

import csv
import json
import os
import re
import sys
import unicodedata
from collections import Counter, defaultdict
from datetime import date

import numpy as np

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
from paths import COMMISSIONS, DATA_PROCESSED, DATA_RAW, RESULTS_FIGURES  # noqa: E402

import matplotlib
matplotlib.use("Agg")
import matplotlib.dates as mdates
import matplotlib.pyplot as plt
from matplotlib.cm import ScalarMappable
from matplotlib.colors import TwoSlopeNorm
from matplotlib.lines import Line2D

CAT = {1: "#2a78d6", 2: "#1baf7a", 3: "#eda100", 4: "#008300",
       5: "#4a3aa7", 6: "#e34948", 7: "#e87ba4"}
INK, INK2, MUTED = "#0b0b0b", "#52514e", "#898781"
GRID, BASE, SURF = "#e1e0d9", "#c3c2b7", "#fcfcfb"
IDEO_CMAP = plt.get_cmap("coolwarm_r")
IDEO_NORM = TwoSlopeNorm(vcenter=0.0, vmin=-3.7, vmax=5.1)
LEFT_C, RIGHT_C = "#a81f1f", "#12457f"
plt.rcParams.update({"font.family": "sans-serif",
                     "font.sans-serif": ["Helvetica Neue", "Helvetica", "Arial", "DejaVu Sans"],
                     "figure.facecolor": SURF, "axes.facecolor": SURF, "savefig.facecolor": SURF,
                     "text.color": INK, "axes.labelcolor": INK2,
                     "xtick.color": INK2, "ytick.color": INK2})

MEMBERSHIP_CSV = os.path.join(DATA_RAW, "commission_membership.csv")
CPT_RAW = ("/Users/anibaloliveramorales/Desktop/Doctorado/-Projects-/"
           "B - constitutional-proposal-tracking/conventionals-bcn-webscrapping/"
           "conventional-profiles-raw.json")
KEYWORDS = {
    1: ["sistema politico", "poder legislativo"],
    2: ["principios constitucionales"],
    3: ["forma de estado", "forma del estado", "justicia territorial", "descentralizacion"],
    4: ["derechos fundamentales"],
    5: ["medio ambiente", "medioambiente", "derechos de la naturaleza", "modelo economico"],
    6: ["sistemas de justicia", "organos autonomos"],
    7: ["sistemas de conocimiento", "conocimientos, culturas", "ciencia, tecnologia"],
}


def norm(s):
    return unicodedata.normalize("NFKD", s or "").encode("ascii", "ignore").decode().lower()


def build_membership():
    raw = json.load(open(CPT_RAW, encoding="utf-8"))
    registry = list(csv.DictReader(open(os.path.join(DATA_PROCESSED, "initiative_registry.csv"),
                                        encoding="utf-8")))
    activity = defaultdict(Counter)
    for r in registry:
        for a in r["firmantes"].split("; "):
            activity[a][int(r["commission"][1])] += 1

    rows, n_text, n_activity = [], 0, 0
    for p in raw:
        name = p["nombre_original_json"]
        t = norm(p.get("integracion_comisiones") or "")
        hits = Counter()
        for k, kws in KEYWORDS.items():
            for kw in kws:
                hits[k] += t.count(kw)
        hits = +hits
        if hits:
            k, source = hits.most_common(1)[0][0], "integracion_comisiones"
            n_text += 1
        elif activity.get(name):
            k, source = activity[name].most_common(1)[0][0], "actividad_firmas"
            n_activity += 1
        else:
            k, source = None, "sin_dato"
        rows.append({"nombre_armonizado": name, "commission": f"C{k}" if k else "",
                     "source": source})
    with open(MEMBERSHIP_CSV, "w", newline="", encoding="utf-8") as fh:
        wr = csv.DictWriter(fh, fieldnames=["nombre_armonizado", "commission", "source"])
        wr.writeheader()
        wr.writerows(rows)
    print(f"membresía: {n_text} por texto BCN, {n_activity} por actividad, "
          f"{sum(1 for r in rows if not r['commission'])} sin dato -> {MEMBERSHIP_CSV}")


if not os.path.exists(MEMBERSHIP_CSV):
    build_membership()
membership = {r["nombre_armonizado"]: r["commission"]
              for r in csv.DictReader(open(MEMBERSHIP_CSV, encoding="utf-8")) if r["commission"]}
print(f"membresía cargada: {len(membership)} | por comisión: "
      + ", ".join(f"C{k}: {sum(1 for v in membership.values() if v == f'C{k}')}"
                  for k in COMMISSIONS))

# --- puntos ideales ---
theta_seq = defaultdict(list)
dates = {}
for r in csv.DictReader(open(os.path.join(DATA_PROCESSED, "emirt_ideal_points_full.csv"),
                             encoding="utf-8")):
    p = int(r["period"])
    theta_seq[r["legislator"]].append((p, float(r["theta"])))
    dates[p] = date(*map(int, r["date"].split("-")))
periods = sorted(dates)
xdates = [dates[p] for p in periods]
for leg in theta_seq:
    theta_seq[leg] = [t for _, t in sorted(theta_seq[leg])]
theta_mean = {leg: float(np.mean(v)) for leg, v in theta_seq.items()}

# =============================================================================
# FB1 — posiciones en el tiempo + |Delta theta| por onda
# =============================================================================
fig, (ax1, ax2) = plt.subplots(2, 1, figsize=(8.2, 6.4), height_ratios=[2.1, 1.0])

for leg, vals in theta_seq.items():
    ax1.plot(xdates, vals, color=IDEO_CMAP(IDEO_NORM(theta_mean[leg])),
             lw=0.55, alpha=0.38, zorder=2, rasterized=True)
left = [leg for leg in theta_seq if theta_mean[leg] < 0]
right = [leg for leg in theta_seq if theta_mean[leg] >= 0]
ax1.plot(xdates, [np.mean([theta_seq[l][i] for l in left]) for i in range(len(periods))],
         color=LEFT_C, lw=2.6, zorder=4, label=f"Left-bloc mean ($\\bar\\theta<0$; n = {len(left)})")
ax1.plot(xdates, [np.mean([theta_seq[l][i] for l in right]) for i in range(len(periods))],
         color=RIGHT_C, lw=2.6, zorder=4, label=f"Right-bloc mean ($\\bar\\theta\\geq 0$; n = {len(right)})")
ax1.xaxis.set_major_locator(mdates.MonthLocator(interval=2))
ax1.xaxis.set_major_formatter(mdates.DateFormatter("%b %Y"))
ax1.tick_params(labelsize=8.5)
ax1.set_ylabel("Ideal point $\\theta_{i,t}$ (dynIRT)", fontsize=9)
ax1.grid(axis="y", color=GRID, lw=0.6)
ax1.set_axisbelow(True)
for side in ("top", "right"):
    ax1.spines[side].set_visible(False)
ax1.spines["left"].set_color(BASE)
ax1.spines["bottom"].set_color(BASE)
ax1.legend(frameon=False, fontsize=8.5, loc="upper left")
ax1.set_title("Positions move: ideal-point trajectories of all 154 delegates "
              "(color = mean position, red = left)",
              fontsize=10.3, color=INK, loc="left", pad=8)

# |Delta theta| por onda (panel M2)
dt_by_step = defaultdict(list)
for r in csv.DictReader(open(os.path.join(DATA_PROCESSED, "network_exposure_panel.csv"),
                             encoding="utf-8")):
    if r["delta_theta"] not in ("", "NA"):
        dt_by_step[int(r["step"])].append(abs(float(r["delta_theta"])))
steps = sorted(dt_by_step)
bp = ax2.boxplot([dt_by_step[s] for s in steps], positions=steps, widths=0.55,
                 showfliers=False, patch_artist=True,
                 medianprops=dict(color=INK, lw=1.4),
                 boxprops=dict(facecolor="#e8e7e2", edgecolor=BASE),
                 whiskerprops=dict(color=BASE), capprops=dict(color=BASE))
for s in steps:
    ax2.text(s, 0.965, f"n={len(dt_by_step[s])}", ha="center", va="top", fontsize=6.8,
             color=MUTED, transform=ax2.get_xaxis_transform())
ax2.set_xticks(steps)
ax2.set_xticklabels([str(s) for s in steps], fontsize=8.5)
ax2.set_xlabel("Commission wave (pooled across commissions)", fontsize=9)
ax2.set_ylabel("$|\\Delta\\theta_{i,t}|$", fontsize=9)
ax2.grid(axis="y", color=GRID, lw=0.6)
ax2.set_axisbelow(True)
for side in ("top", "right"):
    ax2.spines[side].set_visible(False)
ax2.spines["left"].set_color(BASE)
ax2.spines["bottom"].set_color(BASE)
ax2.set_title("…and they move at every wave: distribution of per-wave ideological shifts",
              fontsize=10.3, color=INK, loc="left", pad=6)
fig.tight_layout(h_pad=2.2)
for ext in ("pdf", "png"):
    fig.savefig(os.path.join(RESULTS_FIGURES, f"m2_positions_dynamics.{ext}"),
                dpi=300, bbox_inches="tight")
plt.close(fig)
print("  m2_positions_dynamics.pdf/.png")

# =============================================================================
# FB2 — trayectorias con medias por comisión (membresía)
# =============================================================================
fig, ax = plt.subplots(figsize=(8.2, 4.6))
members_by_k = defaultdict(list)
for leg in theta_seq:
    if leg in membership:
        members_by_k[int(membership[leg][1])].append(leg)
for leg, vals in theta_seq.items():
    k = int(membership[leg][1]) if leg in membership else None
    ax.plot(xdates, vals, color=CAT.get(k, MUTED), lw=0.5, alpha=0.16, zorder=2,
            rasterized=True)
for k in COMMISSIONS:
    legs = members_by_k[k]
    if not legs:
        continue
    ax.plot(xdates, [np.mean([theta_seq[l][i] for l in legs]) for i in range(len(periods))],
            color=CAT[k], lw=2.4, zorder=4, label=f"C{k} (n = {len(legs)})")
ax.xaxis.set_major_locator(mdates.MonthLocator(interval=2))
ax.xaxis.set_major_formatter(mdates.DateFormatter("%b %Y"))
ax.tick_params(labelsize=8.5)
ax.set_ylabel("Ideal point $\\theta_{i,t}$ (dynIRT)", fontsize=9)
ax.grid(axis="y", color=GRID, lw=0.6)
ax.set_axisbelow(True)
for side in ("top", "right"):
    ax.spines[side].set_visible(False)
ax.spines["left"].set_color(BASE)
ax.spines["bottom"].set_color(BASE)
ax.legend(frameon=False, fontsize=8, ncol=4, loc="upper left")
ax.set_title("Ideal-point trajectories by commission membership "
             "(thin lines = delegates, thick lines = commission means)",
             fontsize=10.3, color=INK, loc="left", pad=8)
for ext in ("pdf", "png"):
    fig.savefig(os.path.join(RESULTS_FIGURES, f"theta_dynamics_by_commission.{ext}"),
                dpi=300, bbox_inches="tight")
plt.close(fig)
print("  theta_dynamics_by_commission.pdf/.png")
print("--- Done ---")

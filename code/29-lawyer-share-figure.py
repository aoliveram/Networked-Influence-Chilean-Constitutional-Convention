"""
29-lawyer-share-figure.py  (comentario del autor 2026-07-15, punto 1)
F11: ¿los abogados se concentran en ciertas iniciativas/comisiones?
Sin clasificar contenido: (a) distribución de la proporción de abogados
firmantes por iniciativa (con el benchmark de sorteo aleatorio de firmantes);
(b) la misma proporción por comisión (cajas + medias).

Input:  data/processed/initiative_registry.csv (<=16, D8),
        data/raw/conventional-profiles.json
Output: results/figures/lawyer_share_initiatives.{pdf,png}
"""

import csv
import json
import os
import random
import sys

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
from paths import DATA_PROCESSED, DATA_RAW, RESULTS_FIGURES  # noqa: E402

import matplotlib
matplotlib.use("Agg")
import matplotlib.pyplot as plt
import numpy as np

INK, INK2 = "#0b0b0b", "#52514e"
GRID, BASE, SURF = "#e1e0d9", "#c3c2b7", "#fcfcfb"
BLUE, RED = "#2a78d6", "#e34948"
plt.rcParams.update({"font.family": "sans-serif",
                     "font.sans-serif": ["Helvetica Neue", "Helvetica", "Arial", "DejaVu Sans"],
                     "figure.facecolor": SURF, "axes.facecolor": SURF, "savefig.facecolor": SURF,
                     "text.color": INK, "axes.labelcolor": INK2,
                     "xtick.color": INK2, "ytick.color": INK2})

profiles = json.load(open(os.path.join(DATA_RAW, "conventional-profiles.json"), encoding="utf-8"))
abog = {p["nombre_armonizado"]: int(p["es_abogado"]) for p in profiles}
base_rate = sum(abog.values()) / len(abog)

reg = [r for r in csv.DictReader(open(os.path.join(DATA_PROCESSED, "initiative_registry.csv"),
                                      encoding="utf-8"))
       if 2 <= int(r["n_firmantes"]) <= 16]
shares, sizes, comms = [], [], []
for r in reg:
    S = [a for a in r["firmantes"].split("; ") if a in abog]
    if len(S) < 2:
        continue
    shares.append(np.mean([abog[a] for a in S]))
    sizes.append(len(S))
    comms.append(r["commission"])
shares = np.array(shares)
print(f"iniciativas: {len(shares)} | tasa base de abogados: {base_rate:.3f} "
      f"| media por iniciativa: {shares.mean():.3f}")

# benchmark: mismos tamaños, firmantes sorteados al azar de los 154 (500 réplicas)
random.seed(42)
names = list(abog)
null_means = []
null_pool = []
for b in range(500):
    sh = [np.mean([abog[a] for a in random.sample(names, s)]) for s in sizes]
    null_pool.extend(sh)
    null_means.append(np.mean(sh))
null_pool = np.array(null_pool)
p95_null = np.quantile(null_pool, 0.95)
top_tail = (shares > p95_null).mean()
print(f"cola superior observada sobre el p95 del azar: {top_tail:.1%} (esperado 5%)")

fig, (ax1, ax2) = plt.subplots(1, 2, figsize=(9.6, 3.6))

bins = np.linspace(0, 1, 18)
ax1.hist(null_pool, bins=bins, density=True, histtype="stepfilled",
         color="#f2b134", alpha=0.45, edgecolor="#c98a00", lw=1.4,
         label="random signers (same sizes)", zorder=2)
ax1.hist(shares, bins=bins, density=True, color=BLUE, alpha=0.75,
         edgecolor=SURF, lw=0.5, label="observed initiatives", zorder=3)
# curvas de densidad ajustadas (KDE) y medias de cada distribución, cada una
# del color de su histograma (comentario del autor 2026-07-20, punto 5)
from scipy.stats import gaussian_kde
xs = np.linspace(0, 1, 300)
# bw fijo: el pool nulo es discreto (k/s) y el ancho de banda por defecto oscila
ax1.plot(xs, gaussian_kde(null_pool, bw_method=0.3)(xs), color="#c98a00", lw=2.0, zorder=5)
ax1.plot(xs, gaussian_kde(shares, bw_method=0.3)(xs), color="#1a5cab", lw=2.0, zorder=6)
ax1.axvline(float(np.mean(null_pool)), color="#c98a00", lw=1.2, ls=":", zorder=4)
ax1.axvline(float(np.mean(shares)), color="#1a5cab", lw=1.2, ls=":", zorder=4)
ax1.axvline(base_rate, color=INK, lw=1.1, ls="--", zorder=4)
ax1.text(0.60, ax1.get_ylim()[1] * 0.55,
         f"convention rate = {base_rate:.2f}\nmean obs = {np.mean(shares):.2f}\n"
         f"mean random = {np.mean(null_pool):.2f}", fontsize=7.5, color=INK2, va="top")
ax1.set_xlabel(f"Share of lawyers among signers (n = {len(shares)} initiatives)", fontsize=9)
ax1.set_ylabel("Density", fontsize=9)
ax1.set_title("(a) Lawyer share per initiative vs. random-signer benchmark",
              fontsize=9.6, color=INK, loc="left", pad=6)
ax1.legend(fontsize=7.5, frameon=False, loc="upper right")

order = [f"C{k}" for k in range(1, 8)]
data_by_c = [shares[np.array(comms) == c] for c in order]
bp = ax2.boxplot(data_by_c, tick_labels=order, patch_artist=True, widths=0.55,
                 medianprops=dict(color=INK, lw=1.2),
                 flierprops=dict(marker=".", markersize=3, markerfacecolor=INK2,
                                 markeredgecolor="none"))
for patch in bp["boxes"]:
    patch.set_facecolor(BLUE)
    patch.set_alpha(0.55)
    patch.set_edgecolor(BASE)
means = [d.mean() for d in data_by_c]
ax2.plot(range(1, 8), means, "o", color=RED, ms=5, zorder=4, label="commission mean")
ax2.axhline(base_rate, color=INK, lw=1.0, ls="--", zorder=2)
for i, (m, d) in enumerate(zip(means, data_by_c), 1):
    ax2.annotate(f"n={len(d)}", (i, -0.115), fontsize=6.8, color=INK2, ha="center",
                 annotation_clip=False)
ax2.set_ylabel("Share of lawyers among signers", fontsize=9)
ax2.set_title("(b) By thematic commission (dashed line = convention rate)",
              fontsize=9.6, color=INK, loc="left", pad=6)
ax2.legend(fontsize=7.5, frameon=False, loc="upper right")
ax2.set_ylim(-0.02, 1.02)

for ax in (ax1, ax2):
    ax.grid(axis="y", color=GRID, lw=0.6)
    ax.set_axisbelow(True)
    for side in ("top", "right"):
        ax.spines[side].set_visible(False)
    ax.spines["left"].set_color(BASE)
    ax.spines["bottom"].set_color(BASE)
    ax.tick_params(labelsize=8.5)

fig.tight_layout(w_pad=2.2)
os.makedirs(RESULTS_FIGURES, exist_ok=True)
for ext in ("pdf", "png"):
    fig.savefig(os.path.join(RESULTS_FIGURES, f"lawyer_share_initiatives.{ext}"),
                dpi=300, bbox_inches="tight")
print("medias por comisión:", {c: round(float(m), 3) for c, m in zip(order, means)})
print("figura: lawyer_share_initiatives.pdf/.png")

# ---- figura complementaria (punto 10, 2026-07-19): medias por comisión como
# barras con etiqueta de porcentaje, contra la tasa de la Convención ----------
fig2, ax3 = plt.subplots(figsize=(6.8, 3.4))
bars = ax3.bar(order, means, color=BLUE, alpha=0.75, edgecolor=SURF, width=0.62, zorder=3)
for rect, m in zip(bars, means):
    ax3.annotate(f"{100*m:.0f}%", (rect.get_x() + rect.get_width()/2, m + 0.012),
                 fontsize=9, color=INK, ha="center", fontweight="bold")
ax3.axhline(base_rate, color=RED, lw=1.4, ls="--", zorder=4)
ax3.annotate(f"all convention members: {100*base_rate:.0f}%",
             (6.45, base_rate + 0.012), fontsize=8, color=RED, ha="right")
ax3.set_ylabel("Mean share of lawyers among signers", fontsize=9)
ax3.set_ylim(0, 0.62)
ax3.set_title("Average lawyer share of initiative coalitions, by commission",
              fontsize=10, color=INK, loc="left", pad=8)
ax3.grid(axis="y", color=GRID, lw=0.6)
ax3.set_axisbelow(True)
for side in ("top", "right"):
    ax3.spines[side].set_visible(False)
ax3.spines["left"].set_color(BASE)
ax3.spines["bottom"].set_color(BASE)
ax3.tick_params(labelsize=8.5)
fig2.tight_layout()
for ext in ("pdf", "png"):
    fig2.savefig(os.path.join(RESULTS_FIGURES, f"lawyer_share_by_commission.{ext}"),
                 dpi=300, bbox_inches="tight")
print("figura: lawyer_share_by_commission.pdf/.png")

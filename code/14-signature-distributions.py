"""
14-signature-distributions.py  (respuesta a revisión: D8 y Q3)
F9: dos paneles lado a lado —
  (a) distribución de FIRMAS POR CONVENCIONAL (nº de iniciativas que firmó cada
      uno; habla del "presupuesto" de firmas: ¿firmantes seriales vs selectivos?)
  (b) histograma de FIRMANTES POR INICIATIVA (regla 8-16: ¿bunching en los
      bordes?; valores <8 = firmantes no-persona removidos o no recuperados)

Output: results/figures/signature_distributions.{pdf,png}
"""

import csv
import os
import sys
from collections import Counter

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
from paths import DATA_PROCESSED, RESULTS_FIGURES  # noqa: E402

import matplotlib
matplotlib.use("Agg")
import matplotlib.pyplot as plt

INK, INK2, MUTED = "#0b0b0b", "#52514e", "#898781"
GRID, BASE, SURF = "#e1e0d9", "#c3c2b7", "#fcfcfb"
BLUE, RED = "#2a78d6", "#e34948"
plt.rcParams.update({"font.family": "sans-serif",
                     "font.sans-serif": ["Helvetica Neue", "Helvetica", "Arial", "DejaVu Sans"],
                     "figure.facecolor": SURF, "axes.facecolor": SURF, "savefig.facecolor": SURF,
                     "text.color": INK, "axes.labelcolor": INK2,
                     "xtick.color": INK2, "ytick.color": INK2})

registry = list(csv.DictReader(open(os.path.join(DATA_PROCESSED, "initiative_registry.csv"),
                                    encoding="utf-8")))
registry = [r for r in registry if int(r["n_firmantes"]) >= 2]

per_delegate = Counter()
for r in registry:
    for a in r["firmantes"].split("; "):
        per_delegate[a] += 1
per_initiative = [int(r["n_firmantes"]) for r in registry]

fig, (ax1, ax2) = plt.subplots(1, 2, figsize=(9.2, 3.4))

# (a) firmas por convencional
vals = sorted(per_delegate.values())
ax1.hist(vals, bins=range(0, max(vals) + 4, 3), color=BLUE, edgecolor=SURF, lw=0.6, zorder=3)
med = sorted(vals)[len(vals) // 2]
ax1.axvline(med, color=INK, lw=1.1, ls="--", zorder=4)
ax1.text(med + 1, ax1.get_ylim()[1] * 0.92, f"median = {med}", fontsize=8, color=INK2)
ax1.set_xlabel(f"Initiatives signed per delegate (n = {len(per_delegate)} delegates)",
               fontsize=9)
ax1.set_ylabel("Delegates", fontsize=9)
ax1.set_title("(a) Signing is not scarce for the signer:\nsome delegates sign 3x the median",
              fontsize=9.6, color=INK, loc="left", pad=6)

# (b) firmantes por iniciativa
cnt = Counter(per_initiative)
xs = sorted(cnt)
ax2.bar(xs, [cnt[x] for x in xs], color=RED, edgecolor=SURF, lw=0.6, width=0.85, zorder=3)
for lim in (8, 16):
    ax2.axvline(lim, color=INK, lw=1.1, ls="--", zorder=4)
ax2.text(8, ax2.get_ylim()[1] * 0.02, " rule: min 8", fontsize=7.5, color=INK2,
         rotation=90, va="bottom")
ax2.text(16, ax2.get_ylim()[1] * 0.02, " rule: max 16", fontsize=7.5, color=INK2,
         rotation=90, va="bottom")
ax2.set_xlabel(f"Person-signers per initiative (n = {len(per_initiative)} initiatives)",
               fontsize=9)
ax2.set_ylabel("Initiatives", fontsize=9)
ax2.set_title("(b) Signers per initiative under the 8–16 rule\n(<8 = non-person or unrecovered signers removed)",
              fontsize=9.6, color=INK, loc="left", pad=6)

for ax in (ax1, ax2):
    ax.grid(axis="y", color=GRID, lw=0.6)
    ax.set_axisbelow(True)
    for side in ("top", "right"):
        ax.spines[side].set_visible(False)
    ax.spines["left"].set_color(BASE)
    ax.spines["bottom"].set_color(BASE)
    ax.tick_params(labelsize=8.5)

fig.tight_layout(w_pad=2.5)
os.makedirs(RESULTS_FIGURES, exist_ok=True)
for ext in ("pdf", "png"):
    fig.savefig(os.path.join(RESULTS_FIGURES, f"signature_distributions.{ext}"),
                dpi=300, bbox_inches="tight")

import numpy as np
v = np.array(vals)
print(f"firmas/convencional: min {v.min()}, mediana {med}, media {v.mean():.1f}, máx {v.max()}")
print(f"firmantes/iniciativa: en [8,16]: {sum(1 for x in per_initiative if 8 <= x <= 16)}"
      f"/{len(per_initiative)}; <8: {sum(1 for x in per_initiative if x < 8)}; "
      f">16: {sum(1 for x in per_initiative if x > 16)}; modas: {cnt.most_common(3)}")
print("figura: signature_distributions.pdf/.png")

"""
05-nlp-text-similarity.py  (v2)
Retención léxica génesis -> borrador final y agregación por convencional.

Similitud primaria: coseno TF-IDF (uni+bigramas). Robustez: Sentence-BERT.
DV por artículo: para artículos trazados, el máximo de similitud sobre sus pares;
para fallido/eliminado, 0 (decisión ART-FALLIDO, 2026-07-06).

Scores por convencional (i sobre sus artículos génesis co-firmados A_i):
  retention_all  (y' PRINCIPAL) = media de sim con fracasos = 0
                                = tasa de supervivencia x retención condicional
  retention_traced (y_cond, robustez) = media solo sobre artículos trazados

Outputs (data/processed/): article_similarity_scores.csv, author_success_scores.csv
"""

import csv
import json
import os
import re
import unicodedata
from collections import defaultdict

import numpy as np
from sklearn.feature_extraction.text import TfidfVectorizer
from sklearn.metrics.pairwise import cosine_similarity

from paths import DATA_PROCESSED, MEMBERS

csv.field_size_limit(10_000_000)


def normalize_text(text):
    text = (text or "").lower()
    text = unicodedata.normalize("NFC", text)
    text = re.sub(r"art[ií]culo\s+[\dx]+[\w]*\.?\s*-?\s*", "", text)
    text = re.sub(r"°", "", text)
    text = re.sub(r"[^\w\sáéíóúüñ-]", " ", text)
    return re.sub(r"\s+", " ", text).strip()


# --- pares y desenlaces ---
pairs = list(csv.DictReader(open(os.path.join(DATA_PROCESSED, "article_mapping_unified.csv"), encoding="utf-8")))
outcomes = list(csv.DictReader(open(os.path.join(DATA_PROCESSED, "track_article_outcomes.csv"), encoding="utf-8")))
print(f"Pares: {len(pairs)} | artículos TRACK: {len(outcomes)}")

gen_texts = [normalize_text(p["genesis_text"]) for p in pairs]
fin_texts = [normalize_text(p["final_text"]) for p in pairs]

vec = TfidfVectorizer(analyzer="word", ngram_range=(1, 2), sublinear_tf=True,
                      max_features=20000, min_df=2)
M = vec.fit_transform(gen_texts + fin_texts)
n = len(pairs)
tfidf = np.array([cosine_similarity(M[i], M[n + i])[0, 0] for i in range(n)])

ident = np.array([p["status"] == "identical" for p in pairs])
simil = np.array([p["status"] == "similar" for p in pairs])
print(f"TF-IDF: media global {tfidf.mean():.3f} | identical {tfidf[ident].mean():.3f} "
      f"| similar {tfidf[simil].mean():.3f}  (validación de medida)")

try:
    from sentence_transformers import SentenceTransformer
    model = SentenceTransformer("paraphrase-multilingual-MiniLM-L12-v2")
    ge = model.encode(gen_texts, show_progress_bar=False, batch_size=32)
    fe = model.encode(fin_texts, show_progress_bar=False, batch_size=32)
    sbert = np.array([cosine_similarity(ge[i].reshape(1, -1), fe[i].reshape(1, -1))[0, 0]
                      for i in range(n)])
    has_sbert = True
    print(f"SBERT:  media global {sbert.mean():.3f} | identical {sbert[ident].mean():.3f} "
          f"| similar {sbert[simil].mean():.3f}")
except Exception as e:
    print(f"SBERT no disponible ({e}); solo TF-IDF")
    sbert = np.full(n, np.nan)
    has_sbert = False

# --- score por artículo: máx sobre sus pares; fracaso = 0 ---
by_uid = defaultdict(list)
for i, p in enumerate(pairs):
    by_uid[p["source_uid"]].append(i)

art_rows = []
for o in outcomes:
    uid, cls = o["article_uid"], o["outcome_class"]
    idxs = by_uid.get(uid, [])
    if cls in ("identico", "similar") and idxs:
        s_tfidf = float(tfidf[idxs].max())
        s_sbert = float(np.nanmax(sbert[idxs])) if has_sbert else None
    elif cls in ("fallido", "eliminado"):
        s_tfidf, s_sbert = 0.0, (0.0 if has_sbert else None)
    else:  # trazado sin par de texto u "otro": sin score
        s_tfidf, s_sbert = None, None
    art_rows.append({**o, "sim_tfidf": s_tfidf, "sim_sbert": s_sbert})

with open(os.path.join(DATA_PROCESSED, "article_similarity_scores.csv"), "w", newline="", encoding="utf-8") as fh:
    wr = csv.DictWriter(fh, fieldnames=list(art_rows[0].keys()))
    wr.writeheader()
    wr.writerows(art_rows)
n_scored = sum(1 for r in art_rows if r["sim_tfidf"] is not None)
print(f"Artículos con score: {n_scored}/{len(art_rows)}")

# --- agregación por convencional (roster completo) ---
members = json.load(open(MEMBERS, encoding="utf-8"))
rows_by_author = defaultdict(list)
for r in art_rows:
    if r["sim_tfidf"] is None:
        continue
    for a in (r["authors"].split("; ") if r["authors"] else []):
        rows_by_author[a].append(r)

out = []
for m in sorted(members):
    arts = rows_by_author.get(m, [])
    n_arts = len(arts)
    traced = [r for r in arts if r["outcome_class"] in ("identico", "similar")]
    vals_all = [r["sim_tfidf"] for r in arts]
    vals_traced = [r["sim_tfidf"] for r in traced]
    row = {
        "nombre_armonizado": m,
        "n_articles": n_arts,
        "n_traced": len(traced),
        "n_failed": n_arts - len(traced),
        "survival_rate": len(traced) / n_arts if n_arts else None,
        "retention_all": float(np.mean(vals_all)) if vals_all else None,      # y' principal
        "retention_traced": float(np.mean(vals_traced)) if vals_traced else None,  # robustez
        "n_identical": sum(1 for r in traced if r["outcome_class"] == "identico"),
        "n_similar": sum(1 for r in traced if r["outcome_class"] == "similar"),
    }
    if has_sbert:
        sb_all = [r["sim_sbert"] for r in arts if r["sim_sbert"] is not None]
        sb_tr = [r["sim_sbert"] for r in traced if r["sim_sbert"] is not None]
        row["retention_all_sbert"] = float(np.mean(sb_all)) if sb_all else None
        row["retention_traced_sbert"] = float(np.mean(sb_tr)) if sb_tr else None
    out.append(row)

with open(os.path.join(DATA_PROCESSED, "author_success_scores.csv"), "w", newline="", encoding="utf-8") as fh:
    wr = csv.DictWriter(fh, fieldnames=list(out[0].keys()))
    wr.writeheader()
    wr.writerows(out)

with_score = [r for r in out if r["retention_all"] is not None]
print(f"Convencionales con score: {len(with_score)}/154 | "
      f"y' medio: {np.mean([r['retention_all'] for r in with_score]):.3f} | "
      f"supervivencia media: {np.mean([r['survival_rate'] for r in with_score]):.3f}")
print("--- Done ---")

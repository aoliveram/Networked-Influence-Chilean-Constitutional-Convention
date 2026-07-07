"""
paths.py — Configuración central de rutas del pipeline (cierra P3/P14).

Todos los scripts derivan sus rutas de este módulo. Ningún script lee del
repositorio CPT en runtime: los insumos son snapshots versionados en data/raw/.
Snapshot vigente de dataverse-final: CPT branch paper-draft, commit 5852519.
"""

import os
import glob

REPO_DIR = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))

DATA_RAW = os.path.join(REPO_DIR, "data", "raw")
DATA_PROCESSED = os.path.join(REPO_DIR, "data", "processed")
RESULTS_FIGURES = os.path.join(REPO_DIR, "results", "figures")
RESULTS_TABLES = os.path.join(REPO_DIR, "results", "tables")

# --- snapshot dataverse-final (7 comisiones) ---
DATAVERSE_DIR = os.path.join(DATA_RAW, "dataverse-final")
COMMISSIONS = [1, 2, 3, 4, 5, 6, 7]
COINCIDENCIAS = os.path.join(DATAVERSE_DIR, "coincidencias_comisiones.csv")


def commission_dir(k):
    return os.path.join(DATAVERSE_DIR, f"comision-{k}")


def _one(pattern):
    hits = glob.glob(pattern)
    if len(hits) != 1:
        raise FileNotFoundError(f"esperaba exactamente 1 archivo para {pattern}, hay {len(hits)}")
    return hits[0]


def genesis_path(k):
    return _one(os.path.join(commission_dir(k), f"C{k}_GENESIS_master*.json"))


def track_full_path(k):
    return _one(os.path.join(commission_dir(k), f"C{k}_TRACK_full.json"))


def track_articles_path(k):
    return _one(os.path.join(commission_dir(k), f"C{k}_TRACK_articles.json"))


def borrador_path(k):
    return _one(os.path.join(commission_dir(k), f"C{k}_BORRADOR_final.json"))


# --- referencia de convencionales ---
MEMBERS = os.path.join(DATA_RAW, "convention_members.json")           # 154 nombres canónicos
PROFILES = os.path.join(DATA_RAW, "conventional-profiles.json")       # perfiles curados (auditoría P5)
PROFILE_AUDIT_DIR = os.path.join(DATA_RAW, "profile-audit")

# --- puntos ideales dynIRT ---
EMIRT_DIR = os.path.join(DATA_RAW, "emirt")

# --- legado (redes pre-actualización; solo para comparación) ---
LEGACY_COMMISSIONS_DIR = os.path.join(DATA_RAW, "commissions")
LEGACY_NETWORKS_DIR = os.path.join(DATA_RAW, "network-visualization")

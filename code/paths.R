# paths.R — Configuración central de rutas del pipeline (cierra P3/P14).
# Espejo de paths.py para los scripts R. Uso: source("code/paths.R") desde la
# raíz del repo (o desde cualquier subdirectorio: el root se detecta buscando
# data/raw/dataverse-final hacia arriba).

find_repo_root <- function(start = getwd()) {
  d <- normalizePath(start)
  for (i in 1:6) {
    if (dir.exists(file.path(d, "data", "raw", "dataverse-final"))) return(d)
    parent <- dirname(d)
    if (parent == d) break
    d <- parent
  }
  stop("No se encontró la raíz del repo (data/raw/dataverse-final). Ejecuta desde el repositorio.")
}

REPO_DIR <- find_repo_root()

DATA_RAW <- file.path(REPO_DIR, "data", "raw")
DATA_PROCESSED <- file.path(REPO_DIR, "data", "processed")
RESULTS_FIGURES <- file.path(REPO_DIR, "results", "figures")
RESULTS_TABLES <- file.path(REPO_DIR, "results", "tables")

DATAVERSE_DIR <- file.path(DATA_RAW, "dataverse-final")
COMMISSIONS <- 1:7
COINCIDENCIAS <- file.path(DATAVERSE_DIR, "coincidencias_comisiones.csv")

one_file <- function(pattern_dir, pattern) {
  hits <- Sys.glob(file.path(pattern_dir, pattern))
  if (length(hits) != 1) stop(sprintf("esperaba 1 archivo para %s, hay %d", pattern, length(hits)))
  hits
}

commission_dir <- function(k) file.path(DATAVERSE_DIR, sprintf("comision-%d", k))
genesis_path <- function(k) one_file(commission_dir(k), sprintf("C%d_GENESIS_master*.json", k))
track_full_path <- function(k) one_file(commission_dir(k), sprintf("C%d_TRACK_full.json", k))
track_articles_path <- function(k) one_file(commission_dir(k), sprintf("C%d_TRACK_articles.json", k))
borrador_path <- function(k) one_file(commission_dir(k), sprintf("C%d_BORRADOR_final.json", k))

MEMBERS <- file.path(DATA_RAW, "convention_members.json")
PROFILES <- file.path(DATA_RAW, "conventional-profiles.json")
PROFILE_AUDIT_DIR <- file.path(DATA_RAW, "profile-audit")
EMIRT_DIR <- file.path(DATA_RAW, "emirt")

LEGACY_COMMISSIONS_DIR <- file.path(DATA_RAW, "commissions")
LEGACY_NETWORKS_DIR <- file.path(DATA_RAW, "network-visualization")

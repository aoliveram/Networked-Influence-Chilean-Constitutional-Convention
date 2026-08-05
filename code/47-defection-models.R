# =============================================================================
# 47-defection-models.R  (ronda polnet26, punto 4: las covariables candidatas
# del modelo de defección, un modelo por candidata + el modelo final)
# Base = 15-vote-defection.R (misma construcción de D, E y ventana era de
# normas). Modelos:
#   D1 base:      D ~ E | persona + votación                  (phi = 11.2)
#   D2 bloc-vote: D ~ E | persona + bloque^votación           (la mecánica,
#                 absorbida paramétricamente: el análogo de la permutación)
#   D3 corte:     D1 + |theta_i - c_v| (distancia al punto de corte de la
#                 votación; c_v = alpha_v/beta_v del dynIRT, recortado a ±4)
#   D4 marginal:  D1 + |theta_i - mediana de su bloque| (marginalidad interna)
#   D5 receptor:  D1 con E partida por experiencia del RECEPTOR
#   FINAL:        D ~ E + dist_corte | persona + bloque^votación
# Todas las theta son del dynIRT en el período de la votación. EE cluster por
# persona.
#
# Output: results/tables/M_defection_models.csv
# =============================================================================

cat("=== 47-defection-models.R ===\n")
suppressPackageStartupMessages({ library(jsonlite); library(fixest) })
set.seed(42)
source("code/paths.R")
T0 <- Sys.time()

inp <- readRDS(file.path(EMIRT_DIR, "emIRT_data_input.rds"))
meta <- readRDS(file.path(EMIRT_DIR, "emIRT_metadata.rds"))
orig <- readRDS(file.path(EMIRT_DIR, "emIRT_model_output.rds"))
listas <- read.csv(file.path(DATA_RAW, "electoral_lists.csv"), stringsAsFactors = FALSE)
profiles <- fromJSON(PROFILES)
edges <- read.csv(file.path(DATA_PROCESSED, "genesis_network_initiative.csv"), stringsAsFactors = FALSE)

rc <- inp$rc
votantes <- meta$votantes
vote_date <- meta$unique_dates[inp$bill.session + 1]
per_v <- inp$bill.session + 1L                      # período dynIRT de cada votación
congl <- listas$conglomerado[match(votantes, listas$nombre_armonizado)]
exper <- profiles$experiencia_previa_institucional[match(votantes, profiles$nombre_armonizado)]
BLOQUES <- c("Vamos por Chile", "Apruebo Dignidad", "Lista del Apruebo",
             "Lista del Pueblo", "Escaños Reservados PPOO")

W <- matrix(0, length(votantes), length(votantes), dimnames = list(votantes, votantes))
okE <- edges$source %in% votantes & edges$target %in% votantes
W[cbind(edges$source[okE], edges$target[okE])] <- edges$weight[okE]
W[cbind(edges$target[okE], edges$source[okE])] <- edges$weight[okE]

defection_matrix <- function(rcm) {
  D <- matrix(NA_real_, nrow(rcm), ncol(rcm))
  for (b in BLOQUES) {
    m <- which(congl == b)
    y <- colSums(rcm[m, , drop = FALSE] == 1)
    n <- colSums(rcm[m, , drop = FALSE] == -1)
    modal <- sign(y - n)
    sub <- rcm[m, , drop = FALSE]
    Db <- (sub != 0) * (sub != matrix(modal, length(m), ncol(rcm), byrow = TRUE))
    Db[sub == 0] <- NA
    Db[, modal == 0] <- NA
    D[m, ] <- Db
  }
  D
}
exposure_matrix <- function(D) {
  M <- !is.na(D); D0 <- D; D0[!M] <- 0
  num <- W %*% D0; den <- W %*% M
  E <- num / den; E[den == 0] <- NA
  E
}
D <- defection_matrix(rc)
E <- exposure_matrix(D)

# --- covariables (i, v): distancia al corte y marginalidad dentro del bloque
THETA <- orig$means$x                                # N x 91
cut_v <- orig$means$alpha[, 1] / orig$means$beta[, 1]
cut_v <- pmin(pmax(cut_v, -4), 4)
theta_iv <- THETA[, per_v]                           # N x V (theta en el período del voto)
DIST <- abs(theta_iv - matrix(cut_v, nrow(rc), ncol(rc), byrow = TRUE))
MARG <- matrix(NA_real_, nrow(rc), ncol(rc))
for (b in BLOQUES) {
  m <- which(congl == b)
  med_b <- apply(theta_iv[m, , drop = FALSE], 2, median, na.rm = TRUE)
  MARG[m, ] <- abs(theta_iv[m, , drop = FALSE] - matrix(med_b, length(m), ncol(rc), byrow = TRUE))
}

cols_main <- which(vote_date >= as.Date("2022-02-15"))
idx <- which(!is.na(D[, cols_main, drop = FALSE]) & !is.na(E[, cols_main, drop = FALSE]),
             arr.ind = TRUE)
long <- data.frame(
  nombre = votantes[idx[, 1]], vote_id = cols_main[idx[, 2]],
  bloque = congl[idx[, 1]], exper = exper[idx[, 1]],
  D = D[, cols_main, drop = FALSE][idx], E = E[, cols_main, drop = FALSE][idx],
  dist_corte = DIST[, cols_main, drop = FALSE][idx],
  marg = MARG[, cols_main, drop = FALSE][idx])
long$bv <- interaction(long$bloque, long$vote_id, drop = TRUE)
long$E_exp <- long$E * (long$exper == 1)
long$E_nov <- long$E * (long$exper == 0)
cat(sprintf("  long: %d filas, defección media %.3f\n", nrow(long), mean(long$D)))

run <- function(f, fes, label, terms) {
  m <- feglm(as.formula(paste("D ~", f, "|", fes)), data = long, family = binomial())
  ct <- summary(m, cluster = ~nombre)$coeftable
  out <- do.call(rbind, lapply(terms, function(tt) if (tt %in% rownames(ct))
    data.frame(modelo = label, term = tt, estimate = ct[tt, 1], se = ct[tt, 2],
               p = ct[tt, 4]) else NULL))
  out$n <- nobs(m)
  cat(sprintf("  %s listo (N = %d)\n", label, nobs(m)))
  out
}

res <- list(
  run("E", "nombre + vote_id", "D1 base", "E"),
  run("E", "nombre + bv", "D2 FE bloque x votacion", "E"),
  run("E + dist_corte", "nombre + vote_id", "D3 + distancia al corte", c("E", "dist_corte")),
  run("E + marg", "nombre + vote_id", "D4 + marginalidad en el bloque", c("E", "marg")),
  run("E_exp + E_nov", "nombre + vote_id", "D5 receptor partido", c("E_exp", "E_nov")),
  run("E + dist_corte", "nombre + bv", "FINAL: FE bloque x votacion + corte", c("E", "dist_corte")))
tab <- do.call(rbind, res)
write.csv(tab, file.path(RESULTS_TABLES, "M_defection_models.csv"), row.names = FALSE)
print(tab, row.names = FALSE, digits = 4)
cat(sprintf("--- Done (%.1f min) ---\n", as.numeric(difftime(Sys.time(), T0, units = "mins"))))

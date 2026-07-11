# =============================================================================
# 19-vote-defection.R  (respuesta a revisión: P4a — influencia como conducta)
# ¿Rompo la disciplina de mi bloque cuando mis co-firmantes la rompen?
#   D_iv = 1{voto_iv != voto modal del conglomerado de i en v}  (solo votos S/N)
#   Pr(D_iv = 1) = Lambda( eta_i + mu_v + phi * E_iv ),
#   E_iv = sum_j w_ij D_jv / sum_j w_ij   (leave-one-out, vecinos génesis de i
#          que votaron S/N en v, cualquier bloque)
# FE de persona y de votación via fixest::feglm. Ventana principal: era de
# normas (>= 2022-02-15, quórum 2/3); período completo como robustez.
# Inferencia: SE cluster por convencional + test de permutación (200 perms,
# identidades de defectores permutadas DENTRO de bloque x votación, manteniendo
# la tasa de defección del bloque en esa votación).
#
# Output: results/tables/M_defection.csv
# =============================================================================

cat("=== 19-vote-defection.R ===\n")
suppressPackageStartupMessages({ library(jsonlite); library(fixest); library(parallel) })
set.seed(42)
source("code/paths.R")
T0 <- Sys.time()

inp <- readRDS(file.path(EMIRT_DIR, "emIRT_data_input.rds"))
meta <- readRDS(file.path(EMIRT_DIR, "emIRT_metadata.rds"))
listas <- read.csv(file.path(DATA_RAW, "electoral_lists.csv"), stringsAsFactors = FALSE)
edges <- read.csv(file.path(DATA_PROCESSED, "genesis_network_initiative.csv"), stringsAsFactors = FALSE)

rc <- inp$rc
votantes <- meta$votantes
vote_date <- meta$unique_dates[inp$bill.session + 1]
congl <- listas$conglomerado[match(votantes, listas$nombre_armonizado)]
BLOQUES <- c("Vamos por Chile", "Apruebo Dignidad", "Lista del Apruebo",
             "Lista del Pueblo", "Escaños Reservados PPOO")

W <- matrix(0, length(votantes), length(votantes), dimnames = list(votantes, votantes))
ok <- edges$source %in% votantes & edges$target %in% votantes
W[cbind(edges$source[ok], edges$target[ok])] <- edges$weight[ok]
W[cbind(edges$target[ok], edges$source[ok])] <- edges$weight[ok]

# --- matriz de defección D (NA = sin bloque, sin voto S/N, o bloque empatado)
defection_matrix <- function(rcm) {
  D <- matrix(NA_real_, nrow(rcm), ncol(rcm))
  for (b in BLOQUES) {
    m <- which(congl == b)
    y <- colSums(rcm[m, , drop = FALSE] == 1)
    n <- colSums(rcm[m, , drop = FALSE] == -1)
    modal <- sign(y - n)                 # 0 = empate -> NA
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
build_long <- function(D, E, cols) {
  idx <- which(!is.na(D[, cols, drop = FALSE]) & !is.na(E[, cols, drop = FALSE]),
               arr.ind = TRUE)
  data.frame(nombre = votantes[idx[, 1]], vote_id = cols[idx[, 2]],
             D = D[, cols, drop = FALSE][idx], E = E[, cols, drop = FALSE][idx])
}

D <- defection_matrix(rc)
E <- exposure_matrix(D)
cat(sprintf("  Defección media (celdas válidas): %.3f\n", mean(D, na.rm = TRUE)))

fit_phi <- function(long) {
  m <- feglm(D ~ E | nombre + vote_id, data = long, family = binomial())
  ct <- summary(m, cluster = ~nombre)$coeftable
  c(phi = ct["E", 1], se = ct["E", 2], p = ct["E", 4], n = nobs(m))
}

cols_main <- which(vote_date >= as.Date("2022-02-15"))
cols_all <- seq_len(ncol(rc))
long_main <- build_long(D, E, cols_main)
long_all <- build_long(D, E, cols_all)
r_main <- fit_phi(long_main)
r_all <- fit_phi(long_all)
cat(sprintf("  Era de normas (>=2022-02-15): phi = %.3f (SE %.3f, p = %.2e), N = %d\n",
            r_main["phi"], r_main["se"], r_main["p"], r_main["n"]))
cat(sprintf("  Período completo:             phi = %.3f (SE %.3f, p = %.2e), N = %d\n",
            r_all["phi"], r_all["se"], r_all["p"], r_all["n"]))

# --- permutación dentro de bloque x votación (ventana principal) -------------
N_PERM <- 200
cat(sprintf("  Permutaciones (%d, dentro de bloque x votación, %d cores)...\n",
            N_PERM, min(8, detectCores() - 2)))
perm_one <- function(seed) {
  set.seed(seed)
  Dp <- D
  for (b in BLOQUES) {
    m <- which(congl == b)
    for (v in cols_main) {
      obs <- m[!is.na(D[m, v])]
      if (length(obs) > 1) Dp[obs, v] <- D[sample(obs), v]
    }
  }
  Ep <- exposure_matrix(Dp)
  lp <- build_long(Dp, Ep, cols_main)
  tryCatch(fit_phi(lp)["phi"], error = function(e) NA_real_)
}
phis <- unlist(mclapply(1:N_PERM, perm_one, mc.cores = min(8, detectCores() - 2)))
phis <- phis[!is.na(phis)]
p_perm <- mean(phis >= r_main["phi"])
cat(sprintf("  phi observado = %.3f | permutaciones: media %.3f, p95 %.3f | p_perm = %.3f (%d válidas)\n",
            r_main["phi"], mean(phis), quantile(phis, 0.95), p_perm, length(phis)))

tab <- data.frame(
  window = c("normas (>=2022-02-15)", "completo (jul21-jun22)"),
  phi = c(r_main["phi"], r_all["phi"]), se_cluster = c(r_main["se"], r_all["se"]),
  p_cluster = c(r_main["p"], r_all["p"]), n = c(r_main["n"], r_all["n"]),
  p_perm = c(p_perm, NA), perm_mean = c(mean(phis), NA), perm_p95 = c(quantile(phis, 0.95), NA)
)
dir.create(RESULTS_TABLES, recursive = TRUE, showWarnings = FALSE)
write.csv(tab, file.path(RESULTS_TABLES, "M_defection.csv"), row.names = FALSE)
cat(sprintf("--- Done (%.1f min) ---\n", as.numeric(difftime(Sys.time(), T0, units = "mins"))))

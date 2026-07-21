# =============================================================================
# 45-m2-measurement-propagation.R  (comentario del autor 2026-07-21, punto 7)
# PROPAGACIÓN DEL ERROR DE MEDICIÓN de theta al modelo M2 (FE).
#
# Idea: theta no se observa — se estima; su error de medición (bootstrap
# paramétrico del dynIRT, code/40) tiene mediana 0.14, 3.6 veces el movimiento
# mediano entre períodos. ¿Cuánta incertidumbre agrega eso a lambda (el
# coeficiente de exposición)? Procedimiento:
#   1. B = 50 réplicas theta*_b (mismas semillas del 40: re-simular votos con
#      las probabilidades del modelo y re-ajustar el dynIRT; ~1 s c/u).
#   2. Para cada réplica: reconstruir delta_theta*, theta_lag* y la exposición
#      E*_lag (redes de onda x theta* de los vecinos) y re-estimar el FE:
#         delta* = alpha_i + b theta_lag* + lambda E*_lag + e.
#   3. Combinar (regla de Rubin): Var_total = mean(se_b^2) + (1+1/B) var(lambda_b).
#      MDE honesto = 2.8 x sqrt(Var_total).
#
# Output: results/tables/M2_measurement.csv + consola
# =============================================================================

cat("=== 45-m2-measurement-propagation.R ===\n")
suppressPackageStartupMessages({
  library(emIRT); library(jsonlite); library(plm); library(lmtest); library(parallel)
})
set.seed(42)
source("code/paths.R")
T0 <- Sys.time()
B <- 50
N_CORES <- 8

# ---------- 1. réplicas theta* (idéntico a code/40, semillas 1001..1050) -----
inp <- readRDS(file.path(EMIRT_DIR, "emIRT_data_input.rds"))
meta <- readRDS(file.path(EMIRT_DIR, "emIRT_metadata.rds"))
orig <- readRDS(file.path(EMIRT_DIR, "emIRT_model_output.rds"))
rc <- inp$rc
N <- nrow(rc); J <- ncol(rc)
Tt <- max(inp$bill.session) + 1L
per <- inp$bill.session[, 1] + 1L
alpha <- orig$means$alpha[, 1]; beta <- orig$means$beta[, 1]
X <- orig$means$x
PRJ <- pnorm(matrix(alpha, N, J, byrow = TRUE) + matrix(beta, N, J, byrow = TRUE) * X[, per])
OBS <- rc != 0
priors <- list(x.mu0 = matrix(0, N, 1), x.sigma0 = matrix(1, N, 1),
               beta.mu = matrix(0, 2, 1), beta.sigma = diag(25, 2),
               omega2 = matrix(0.025, N, 1))
priors$x.mu0[meta$idx_derecha] <- 2;  priors$x.sigma0[meta$idx_derecha] <- 0.05
priors$x.mu0[meta$idx_izquierda] <- -2; priors$x.sigma0[meta$idx_izquierda] <- 0.05
one_rep <- function(b) {
  set.seed(1000 + b)
  rcs <- matrix(0L, N, J)
  rcs[OBS] <- ifelse(runif(sum(OBS)) < PRJ[OBS], 1L, -1L)
  keep <- which(apply(rcs, 2, function(v) min(sum(v == 1), sum(v == -1)) >= 2))
  perk <- per[keep]; fk <- sort(unique(perk))
  dat <- list(rc = rcs[, keep, drop = FALSE], startlegis = matrix(0L, N, 1),
              endlegis = matrix(length(fk) - 1L, N, 1),
              bill.session = matrix(as.integer(match(perk, fk) - 1L), ncol = 1), T = length(fk))
  starts <- list(alpha = orig$means$alpha[keep, , drop = FALSE],
                 beta = orig$means$beta[keep, , drop = FALSE], x = X[, fk, drop = FALSE])
  fit <- dynIRT(.data = dat, .starts = starts, .priors = priors,
                .control = list(threads = 1, verbose = FALSE, thresh = 1e-6, maxit = 500))
  th <- matrix(NA_real_, N, Tt); th[, fk] <- fit$means$x
  th
}
cat(sprintf("  Generando B = %d réplicas de theta* (mclapply %d)...\n", B, N_CORES))
reps <- mclapply(1:B, one_rep, mc.cores = N_CORES)

# ---------- 2. panel M2 por réplica ------------------------------------------
panel <- read.csv(file.path(DATA_PROCESSED, "network_exposure_panel.csv"), stringsAsFactors = FALSE)
roster <- sort(unique(panel$legislator))
stopifnot(identical(roster, sort(meta$votantes)))
ridx <- match(roster, meta$votantes)
n <- length(roster)
# período emIRT de cada (comisión, step)
key <- unique(panel[, c("commission", "step", "emirt_date")])
key$period <- match(key$emirt_date, as.character(meta$unique_dates))
stopifnot(!anyNA(key$period))
# redes de onda acumuladas por comisión
expo <- function(W, th) { num <- W %*% ifelse(is.na(th), 0, th); den <- rowSums(W)
  out <- as.numeric(num) / den; out[den == 0] <- NA; out }
Wlist <- list()
for (k in 1:7) {
  comm <- sprintf("C%d", k)
  waves <- fromJSON(file.path(DATA_PROCESSED, sprintf("C%d_dynamic_networks.json", k)),
                    simplifyDataFrame = TRUE)
  Wc <- lapply(names(waves), function(wn) {
    ed <- waves[[wn]]
    W <- matrix(0, n, n, dimnames = list(roster, roster))
    if (length(ed) && nrow(ed)) {
      W[cbind(ed$source, ed$target)] <- ed$weight
      W[cbind(ed$target, ed$source)] <- ed$weight
    }
    W
  })
  Wlist[[comm]] <- Wc
}

fe_lambda <- function(th_mat) {          # th_mat: n x Tt (orden roster)
  rows <- list()
  for (comm in names(Wlist)) {
    kk <- key[key$commission == comm, ]
    kk <- kk[order(kk$step), ]
    th_step <- lapply(seq_len(nrow(kk)), function(i) th_mat[, kk$period[i]])
    for (t in 2:nrow(kk)) {
      if (kk$period[t] == kk$period[t - 1]) next   # mismo período emIRT: delta mecánico 0
      rows[[length(rows) + 1]] <- data.frame(
        legislator = roster, commission = comm, step = kk$step[t],
        delta = th_step[[t]] - th_step[[t - 1]], theta_lag = th_step[[t - 1]],
        E_lag = expo(Wlist[[comm]][[t - 1]], th_step[[t - 1]]))
    }
  }
  df <- do.call(rbind, rows)
  df <- df[!is.na(df$delta) & !is.na(df$theta_lag) & !is.na(df$E_lag), ]
  pd <- pdata.frame(df, index = "legislator")
  m <- plm(delta ~ theta_lag + E_lag, data = pd, model = "within")
  ct <- coeftest(m, vcov = vcovHC(m, method = "arellano", cluster = "group"))
  c(lambda = ct["E_lag", 1], se = ct["E_lag", 2], n = nrow(df))
}

# sanity: con el theta ORIGINAL debe reproducir el M2 base (~+0.0066, EE 0.0043)
th0 <- matrix(NA_real_, n, Tt); th0 <- X[ridx, , drop = FALSE]
base <- fe_lambda(th0)
cat(sprintf("  Sanity (theta original): lambda = %+.4f (EE %.4f), N = %d\n",
            base["lambda"], base["se"], base["n"]))

cat("  FE por réplica...\n")
out <- mclapply(reps, function(th) fe_lambda(th[ridx, , drop = FALSE]), mc.cores = N_CORES)
lam <- sapply(out, `[[`, "lambda"); ses <- sapply(out, `[[`, "se")

# ---------- 3. combinación (Rubin) -------------------------------------------
var_within <- mean(ses^2)                 # incertidumbre muestral media
var_between <- var(lam) * (1 + 1 / B)     # incertidumbre por medición
se_total <- sqrt(var_within + var_between)
tab <- data.frame(
  cantidad = c("lambda media entre réplicas", "EE muestral (media)",
               "DE entre réplicas (medición)", "EE total (Rubin)",
               "MDE honesto (2.8 x EE total)", "lambda con theta original",
               "MDE previo (solo muestral)"),
  valor = c(mean(lam), sqrt(var_within), sd(lam), se_total, 2.8 * se_total,
            base["lambda"], 2.8 * base["se"]))
print(tab, row.names = FALSE, digits = 3)
write.csv(tab, file.path(RESULTS_TABLES, "M2_measurement.csv"), row.names = FALSE)
cat(sprintf("--- Done (%.1f min) ---\n", as.numeric(difftime(Sys.time(), T0, units = "mins"))))

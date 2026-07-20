# =============================================================================
# 40-dynirt-param-bootstrap.R  (comentario del autor 2026-07-20, punto 7)
# PILOTO del bootstrap PARAMÉTRICO del dynIRT, para propagar el error de
# medición de theta a los modelos de RQ2 (el MDE actual solo refleja
# incertidumbre muestral de la regresión).
#
# Procedimiento (estándar para modelos IRT ajustados por EM, que no entregan
# EEs de theta):
#   1. Del ajuste original tomamos alpha_j, beta_j y theta_it (medias EM).
#   2. Simulamos matrices de votación rc* con
#      Pr(sí)_ij = pnorm(alpha_j + beta_j * theta_{i,t(j)}), en las celdas
#      efectivamente observadas (los ausentes quedan ausentes).
#   3. Re-ajustamos el dynIRT COMPLETO (91 períodos) sobre cada rc*, con los
#      mismos priors/anclas y warm starts del original (threads = 1).
#   4. La dispersión de theta*_it entre réplicas estima su error de medición.
# PILOTO: B = 2 réplicas para (a) verificar que el re-ajuste converge y
# (b) cronometrar; el run real (B = 50-100) se extrapola del tiempo medido.
#
# Output: consola + data/processed/dynirt_boot_pilot.rds (las réplicas piloto)
# =============================================================================

cat("=== 40-dynirt-param-bootstrap.R (PILOTO B=2) ===\n")
suppressPackageStartupMessages({ library(emIRT) })
set.seed(42)
source("code/paths.R")

inp <- readRDS(file.path(EMIRT_DIR, "emIRT_data_input.rds"))
meta <- readRDS(file.path(EMIRT_DIR, "emIRT_metadata.rds"))
orig <- readRDS(file.path(EMIRT_DIR, "emIRT_model_output.rds"))
rc <- inp$rc
N <- nrow(rc); J <- ncol(rc)
Tt <- max(inp$bill.session) + 1L
per <- inp$bill.session[, 1] + 1L              # período de cada votación (1..T)
cat(sprintf("  Original: %d convencionales x %d votaciones, %d períodos\n", N, J, Tt))

alpha <- orig$means$alpha[, 1]
beta <- orig$means$beta[, 1]
X <- orig$means$x                              # N x T
PRJ <- pnorm(matrix(alpha, N, J, byrow = TRUE) +
             matrix(beta, N, J, byrow = TRUE) * X[, per])
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
  # descartar columnas sin variación (como en 37): singularizan el EM
  keep <- which(apply(rcs, 2, function(v) min(sum(v == 1), sum(v == -1)) >= 2))
  rck <- rcs[, keep, drop = FALSE]
  perk <- per[keep]
  fk <- sort(unique(perk))
  bs <- matrix(as.integer(match(perk, fk) - 1L), ncol = 1)
  starts <- list(alpha = orig$means$alpha[keep, , drop = FALSE],
                 beta = orig$means$beta[keep, , drop = FALSE],
                 x = X[, fk, drop = FALSE])
  dat <- list(rc = rck, startlegis = matrix(0L, N, 1),
              endlegis = matrix(length(fk) - 1L, N, 1),
              bill.session = bs, T = length(fk))
  t0 <- Sys.time()
  fit <- dynIRT(.data = dat, .starts = starts, .priors = priors,
                .control = list(threads = 1, verbose = FALSE, thresh = 1e-6, maxit = 500))
  secs <- as.numeric(difftime(Sys.time(), t0, units = "secs"))
  th <- matrix(NA_real_, N, Tt); th[, fk] <- fit$means$x
  cat(sprintf("    réplica %d: %d/%d columnas con variación, re-ajuste en %.1f s | cor con original = %.4f\n",
              b, length(keep), J, secs, cor(as.vector(th[, fk]), as.vector(X[, fk]))))
  list(theta = th, secs = secs)
}

reps <- lapply(1:2, one_rep)
saveRDS(reps, file.path(DATA_PROCESSED, "dynirt_boot_pilot.rds"))

secs <- sapply(reps, `[[`, "secs")
cat(sprintf("\n  Tiempo por réplica: %.1f s (media)\n", mean(secs)))
cat(sprintf("  Extrapolación run real: B = 50 -> %.1f min | B = 100 -> %.1f min (secuencial, 1 hilo)\n",
            50 * mean(secs) / 60, 100 * mean(secs) / 60))
cat(sprintf("  Con mclapply(8): B = 100 -> ~%.1f min\n", 100 * mean(secs) / 60 / 8))

# dispersión piloto (2 réplicas: solo orden de magnitud, NO el EE final)
d <- abs(reps[[1]]$theta - reps[[2]]$theta) / sqrt(2)
cat(sprintf("  Orden de magnitud del error de medición (2 réplicas): mediana %.4f, p90 %.4f\n",
            median(d, na.rm = TRUE), quantile(d, 0.9, na.rm = TRUE)))
cat("  (Comparar con |delta theta| mediana por onda ~0.12-0.25 y con MDE lambda = 0.012.)\n")
cat("--- Done ---\n")

# ---------------------------------------------------------------------------
# RUN REAL (agregado tras el piloto: 1.0 s/réplica -> B = 50 es gratis).
# Guarda el array de réplicas y el EE de medición por (convencional, período).
# ---------------------------------------------------------------------------
if (Sys.getenv("BOOT_FULL") == "1") {
  cat("\n=== RUN REAL: B = 50 (mclapply 8) ===\n")
  suppressPackageStartupMessages(library(parallel))
  t0 <- Sys.time()
  reps50 <- mclapply(1:50, function(b) one_rep(b)$theta, mc.cores = 8)
  arr <- simplify2array(reps50)                    # N x T x 50
  se_theta <- apply(arr, c(1, 2), sd, na.rm = TRUE)
  dimnames(se_theta) <- list(meta$votantes, NULL)
  saveRDS(list(se_theta = se_theta, B = 50), file.path(DATA_PROCESSED, "dynirt_measurement_se.rds"))
  cat(sprintf("  B = 50 en %.1f min | EE de medición de theta: mediana %.4f, p90 %.4f, max %.4f\n",
              as.numeric(difftime(Sys.time(), t0, units = "mins")),
              median(se_theta, na.rm = TRUE), quantile(se_theta, .9, na.rm = TRUE),
              max(se_theta, na.rm = TRUE)))
  q <- quantile(abs(orig$means$x[, -1] - orig$means$x[, -ncol(orig$means$x)]), c(.5, .9))
  cat(sprintf("  Referencia |delta theta| entre períodos consecutivos: mediana %.4f, p90 %.4f\n", q[1], q[2]))
}

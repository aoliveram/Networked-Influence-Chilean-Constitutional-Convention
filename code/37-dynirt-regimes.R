# =============================================================================
# 37-dynirt-regimes.R  (comentario del autor 2026-07-20, punto 2)
# dynIRT SEPARADO POR RÉGIMEN DE VOTACIÓN, con el corte institucional del
# 15-feb-2022 (primeras votaciones de normas bajo 2/3; verificado IV.D4):
#   - era MAYORÍA:   votaciones con fecha <  2022-02-15
#   - era DOS TERCIOS: votaciones con fecha >= 2022-02-15
# Cada era se estima como un dynIRT independiente (emIRT), con la misma
# arquitectura del run original: anclas Marinovic (+) / Baradit (-),
# omega2 = 0.025, todos los legisladores sirviendo el período completo.
# NOTA: los priors exactos del run original no están en el repo; se
# reconstruyen con la convención estándar (documentada abajo). Validación:
# correlación de las medias por era contra el dynIRT original.
#
# Outputs: data/processed/dynirt_regime_mayoria.csv
#          data/processed/dynirt_regime_dostercios.csv
#          (formato largo: legislator, period, date, theta)
# =============================================================================

cat("=== 37-dynirt-regimes.R ===\n")
suppressPackageStartupMessages({ library(emIRT) })
set.seed(42)
source("code/paths.R")

inp <- readRDS(file.path(EMIRT_DIR, "emIRT_data_input.rds"))
meta <- readRDS(file.path(EMIRT_DIR, "emIRT_metadata.rds"))
orig_fit <- readRDS(file.path(EMIRT_DIR, "emIRT_model_output.rds"))
N <- nrow(inp$rc)
vote_date <- meta$unique_dates[inp$bill.session + 1]
CORTE <- as.Date("2022-02-15")

run_era <- function(cols, label) {
  rc <- inp$rc[, cols, drop = FALSE]
  # descartar votaciones sin variación (unánimes entre los que votaron):
  # no aportan información y pueden singularizar la actualización variacional
  con_var <- apply(rc, 2, function(v) min(sum(v == 1), sum(v == -1)) >= 2)
  cols <- cols[con_var]
  rc <- rc[, con_var, drop = FALSE]
  fechas_era <- sort(unique(vote_date[cols]))
  Tk <- length(fechas_era)
  bill.session <- matrix(as.integer(match(vote_date[cols], fechas_era) - 1L), ncol = 1)
  cat(sprintf("  %s: %d votaciones | %d períodos | %s a %s\n",
              label, ncol(rc), Tk, format(min(fechas_era)), format(max(fechas_era))))

  # priors reconvención estándar: anclas con prior apretado +-2, resto difuso
  x.mu0 <- matrix(0, N, 1); x.sigma0 <- matrix(1, N, 1)
  x.mu0[meta$idx_derecha] <- 2;  x.sigma0[meta$idx_derecha] <- 0.05
  x.mu0[meta$idx_izquierda] <- -2; x.sigma0[meta$idx_izquierda] <- 0.05
  priors <- list(x.mu0 = x.mu0, x.sigma0 = x.sigma0,
                 beta.mu = matrix(0, 2, 1), beta.sigma = diag(25, 2),
                 omega2 = matrix(0.025, N, 1))
  # warm starts desde el modelo original conjunto (numéricamente estable):
  # x en los períodos originales correspondientes a las fechas de la era;
  # alpha/beta de las columnas de la era
  per_orig <- match(fechas_era, meta$unique_dates)
  x.start <- orig_fit$means$x[, per_orig, drop = FALSE]
  starts <- list(alpha = orig_fit$means$alpha[cols, , drop = FALSE],
                 beta = orig_fit$means$beta[cols, , drop = FALSE],
                 x = x.start)
  dat <- list(rc = rc, startlegis = matrix(0L, N, 1),
              endlegis = matrix(Tk - 1L, N, 1),
              bill.session = bill.session, T = Tk)
  t0 <- Sys.time()
  fit <- dynIRT(.data = dat, .starts = starts, .priors = priors,
                .control = list(threads = 1, verbose = FALSE, thresh = 1e-6,
                                maxit = 500))
  cat(sprintf("    convergió en %.1f s\n", as.numeric(difftime(Sys.time(), t0, units = "secs"))))
  theta <- fit$means$x                       # N x Tk
  long <- data.frame(
    legislator = rep(meta$votantes, times = Tk),
    period = rep(seq_len(Tk) - 1L, each = N),
    date = rep(as.character(fechas_era), each = N),
    theta = as.vector(theta))
  long
}

era1 <- run_era(which(vote_date < CORTE), "era MAYORÍA (jul-21 a 14-feb-22)")
era2 <- run_era(which(vote_date >= CORTE), "era DOS TERCIOS (15-feb a jun-22)")
write.csv(era1, file.path(DATA_PROCESSED, "dynirt_regime_mayoria.csv"), row.names = FALSE)
write.csv(era2, file.path(DATA_PROCESSED, "dynirt_regime_dostercios.csv"), row.names = FALSE)

# ---------------- validación contra el dynIRT original -----------------------
orig <- read.csv(file.path(DATA_PROCESSED, "emirt_summary_metrics.csv"), stringsAsFactors = FALSE)
m1 <- aggregate(theta ~ legislator, era1, mean)
m2 <- aggregate(theta ~ legislator, era2, mean)
v <- merge(merge(m1, m2, by = "legislator", suffixes = c("_may", "_23")),
           orig[, c("nombre_armonizado", "theta_mean")],
           by.x = "legislator", by.y = "nombre_armonizado")
cat(sprintf("\n  Validación: cor(era mayoría, original) = %.3f | cor(era 2/3, original) = %.3f | cor(entre eras) = %.3f\n",
            cor(v$theta_may, v$theta_mean), cor(v$theta_23, v$theta_mean),
            cor(v$theta_may, v$theta_23)))
chk <- v[v$legislator %in% c("Marinovic, Teresa", "Baradit, Jorge"), ]
print(chk, row.names = FALSE, digits = 2)
cat("--- Done ---\n")

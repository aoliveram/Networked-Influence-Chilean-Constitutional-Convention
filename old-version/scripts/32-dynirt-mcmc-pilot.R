# =============================================================================
# 32-dynirt-mcmc-pilot.R  (comentario del autor 2026-07-15, punto 6; P10)
# ¿Cuánto costaría pasar el dynIRT a MCMC (MCMCpack::MCMCdynamicIRT1d) para
# tener incertidumbre de theta? PILOTO de timing: ajustes cortos sobre
# subconjuntos de votaciones, extrapolación lineal en (J x iteraciones).
# NO produce estimaciones utilizables — solo mide el costo.
#
# Output: results/tables/dynirt_mcmc_pilot.csv (+ consola)
# =============================================================================

cat("=== 32-dynirt-mcmc-pilot.R ===\n")
suppressPackageStartupMessages({ library(MCMCpack) })
set.seed(42)
source("code/paths.R")

inp <- readRDS(file.path(EMIRT_DIR, "emIRT_data_input.rds"))
meta <- readRDS(file.path(EMIRT_DIR, "emIRT_metadata.rds"))
rc <- inp$rc                                    # 154 x 4707: 1 sí, -1 no, 0 NA
Y <- matrix(NA_real_, nrow(rc), ncol(rc), dimnames = list(meta$votantes, NULL))
Y[rc == 1] <- 1; Y[rc == -1] <- 0
periods <- inp$bill.session + 1                 # 1..91 por votación

pilot <- function(J, burnin, mcmc) {
  cols <- seq_len(J)
  itm <- periods[cols]
  itm <- as.integer(factor(itm))                # períodos consecutivos 1..T
  t0 <- Sys.time()
  fit <- MCMCdynamicIRT1d(Y[, cols], item.time.map = itm,
                          burnin = burnin, mcmc = mcmc, thin = 1, verbose = 0,
                          seed = 42, tau2.start = 0.025,
                          theta.constraints = list("Marinovic, Teresa" = "+",
                                                   "Baradit, Jorge" = "-"))
  as.numeric(difftime(Sys.time(), t0, units = "secs"))
}

res <- list()
for (cfg in list(c(J = 200, it = 75), c(J = 600, it = 75), c(J = 600, it = 150))) {
  burnin <- 25; mcmc <- cfg["it"] - burnin
  secs <- tryCatch(pilot(cfg["J"], burnin, mcmc), error = function(e) NA)
  cat(sprintf("  J = %4d votaciones | %3d iteraciones -> %.1f s\n",
              cfg["J"], cfg["it"], secs))
  res[[length(res) + 1]] <- data.frame(J = cfg["J"], iters = cfg["it"], secs = secs)
}
tab <- do.call(rbind, res)

# extrapolación: costo ~ lineal en J y en iteraciones (verificable con las filas)
sec_por_Jiter <- tab$secs / (tab$J * tab$iters)
cat(sprintf("\n  seg por (votación x iteración): %s (media %.2e)\n",
            paste(sprintf("%.2e", sec_por_Jiter), collapse = ", "), mean(sec_por_Jiter)))
J_full <- ncol(Y)
for (iters in c(2000, 20000, 40000)) {
  est <- mean(sec_por_Jiter) * J_full * iters
  cat(sprintf("  Corrida completa (J = %d, %d iteraciones): ~%.1f horas\n",
              J_full, iters, est / 3600))
}
tab$sec_por_Jiter <- sec_por_Jiter
dir.create(RESULTS_TABLES, recursive = TRUE, showWarnings = FALSE)
write.csv(tab, file.path(RESULTS_TABLES, "dynirt_mcmc_pilot.csv"), row.names = FALSE)
cat("--- Done ---\n")

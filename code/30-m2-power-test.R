# =============================================================================
# 30-m2-power-test.R  (comentario del autor 2026-07-15, punto 2; revisión D7)
# ¿Tenía M2 espacio para detectar influencia? Dos cantidades, cada una con su
# benchmark de "mundo aleatorio" (theta permutado sobre la MISMA red):
#
#  (A) GAP = |theta_i - NetExp_i| por delegado-onda: cuánto podría "jalarte"
#      tu vecindario. Benchmark: permutar theta entre delegados dentro de cada
#      onda (misma topología y pesos, vecinos aleatorios respecto de ideología)
#      y recomputar el gap. Si gap_obs << gap_perm, la selección homofílica ya
#      había cerrado el espacio donde la influencia se habría visto.
#  (B) VARIACIÓN IDENTIFICADORA del FE = sd within-delegado de NetExp a través
#      de las ondas. Benchmark: UNA permutación de theta aplicada consistente-
#      mente en todas las ondas de la comisión (para preservar la estructura
#      temporal), recomputar NetExp por onda, sd within. El estimador FE solo
#      ve esta variación; si obs << perm, el SE observado es grande "porque
#      no había con qué identificar".
#  (C) MDE: efecto mínimo detectable de lambda (= beta_3 del panel FE) con el
#      SE observado, y su reescalamiento al mundo permutado vía el cociente (B).
#
# Inputs: data/processed/network_exposure_panel.csv,
#         data/processed/C{k}_dynamic_networks.json
# Output: results/tables/M2_power.csv
# =============================================================================

cat("=== 30-m2-power-test.R ===\n")
suppressPackageStartupMessages({ library(jsonlite) })
set.seed(42)
source("code/paths.R")
B <- 200

panel <- read.csv(file.path(DATA_PROCESSED, "network_exposure_panel.csv"), stringsAsFactors = FALSE)
roster <- sort(unique(panel$legislator))
n <- length(roster)

exposure <- function(W, th) {
  num <- W %*% th; den <- rowSums(W)
  out <- as.numeric(num) / den
  out[den == 0] <- NA
  out
}

rows <- list()
for (k in 1:7) {
  comm <- sprintf("C%d", k)
  waves <- fromJSON(file.path(DATA_PROCESSED, sprintf("C%d_dynamic_networks.json", k)),
                    simplifyDataFrame = TRUE)
  wave_names <- names(waves)
  # matrices acumuladas por onda
  W_list <- lapply(wave_names, function(wn) {
    ed <- waves[[wn]]
    W <- matrix(0, n, n, dimnames = list(roster, roster))
    if (length(ed) && nrow(ed)) {
      W[cbind(ed$source, ed$target)] <- ed$weight
      W[cbind(ed$target, ed$source)] <- ed$weight
    }
    W
  })
  # theta por onda (del panel, alineado emIRT)
  th_list <- lapply(seq_along(wave_names) - 1L, function(st) {
    sub <- panel[panel$commission == comm & panel$step == st, ]
    setNames(sub$theta, sub$legislator)[roster]
  })

  # ---- (A) gap por onda: observado vs theta permutado por onda --------------
  for (t in seq_along(wave_names)) {
    th <- th_list[[t]]
    if (all(is.na(th))) next
    ne_obs <- exposure(W_list[[t]], ifelse(is.na(th), 0, th))
    ok <- !is.na(ne_obs) & !is.na(th)
    if (sum(ok) < 5) next
    gap_obs <- mean(abs(th[ok] - ne_obs[ok]))
    gap_perm <- replicate(B, {
      thp <- setNames(sample(th), roster)
      nep <- exposure(W_list[[t]], ifelse(is.na(thp), 0, thp))
      mean(abs(thp[ok] - nep[ok]), na.rm = TRUE)
    })
    rows[[length(rows) + 1]] <- data.frame(
      commission = comm, metric = "gap", step = t - 1L, n_delegados = sum(ok),
      obs = gap_obs, perm_media = mean(gap_perm),
      perm_p5 = quantile(gap_perm, 0.05), ratio = gap_obs / mean(gap_perm))
  }

  # ---- (B) sd within de NetExp: observado vs permutación consistente ---------
  ne_mat <- sapply(seq_along(wave_names), function(t) {
    th <- th_list[[t]]
    exposure(W_list[[t]], ifelse(is.na(th), 0, th))
  })
  sd_within_obs <- mean(apply(ne_mat, 1, sd, na.rm = TRUE), na.rm = TRUE)
  sd_within_perm <- replicate(B, {
    perm <- sample(n)                     # una permutación para toda la comisión
    nep <- sapply(seq_along(wave_names), function(t) {
      th <- th_list[[t]][perm]
      exposure(W_list[[t]], ifelse(is.na(th), 0, th))
    })
    mean(apply(nep, 1, sd, na.rm = TRUE), na.rm = TRUE)
  })
  rows[[length(rows) + 1]] <- data.frame(
    commission = comm, metric = "sd_within_netexp", step = NA, n_delegados = n,
    obs = sd_within_obs, perm_media = mean(sd_within_perm),
    perm_p5 = quantile(sd_within_perm, 0.05),
    ratio = sd_within_obs / mean(sd_within_perm))
}
tab <- do.call(rbind, rows)
dir.create(RESULTS_TABLES, recursive = TRUE, showWarnings = FALSE)
write.csv(tab, file.path(RESULTS_TABLES, "M2_power.csv"), row.names = FALSE)

# ---- resumen agregado + MDE --------------------------------------------------
gaps <- tab[tab$metric == "gap", ]
sds <- tab[tab$metric == "sd_within_netexp", ]
cat(sprintf("\n(A) GAP |theta - NetExp| (%d comisión-ondas):\n", nrow(gaps)))
cat(sprintf("    observado medio = %.3f | permutado medio = %.3f | RATIO medio = %.2f\n",
            mean(gaps$obs), mean(gaps$perm_media), mean(gaps$ratio)))
cat(sprintf("    (ratio por comisión: %s)\n",
            paste(sprintf("%s %.2f", unique(gaps$commission),
                          tapply(gaps$ratio, gaps$commission, mean)[unique(gaps$commission)]),
                  collapse = ", ")))
cat(sprintf("\n(B) SD within-delegado de NetExp:\n"))
print(sds[, c("commission", "obs", "perm_media", "ratio")], row.names = FALSE, digits = 3)

m2 <- read.csv(file.path(RESULTS_TABLES, "M2_panel.csv"), stringsAsFactors = FALSE)
se_fe <- m2$se[m2$model == "Efectos fijos"]
mde <- 2.8 * se_fe
ratio_sd <- mean(sds$ratio)
cat(sprintf("\n(C) MDE de lambda (2.8 x SE_FE = 2.8 x %.4f) = %.3f\n", se_fe, mde))
cat(sprintf("    En el mundo theta-permutado, SD(NetExp within) seria x%.1f mayor,\n", 1 / ratio_sd))
cat(sprintf("    o sea MDE ~ %.3f (SE escala ~1/SD de la exposicion).\n", mde * ratio_sd))
cat(sprintf("    Gap medio observado = %.3f: una influencia lambda = MDE moveria\n", mean(gaps$obs)))
cat(sprintf("    |Delta theta| ~ %.4f por onda (vs mediana observada de |Delta theta| ~0.12-0.25).\n",
            mde * mean(gaps$obs)))
cat("--- Done ---\n")

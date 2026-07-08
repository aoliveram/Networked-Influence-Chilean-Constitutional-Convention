# =============================================================================
# 07-model-spatial-durbin.R  (v2, actualización 7 comisiones)
# Modelo 3: éxito legislativo — Spatial Durbin Model.
#
#   y = rho*W*y + X*beta + W*X*gamma + e
#   y  = retention_all (y' PRINCIPAL: retención media con fracasos = 0;
#        decisión ART-FALLIDO 2026-07-06)
#   W  = red génesis de INICIATIVAS row-normalizada (P9: la estructura precede
#        al resultado). Covariables con grado académico 0-3.
# =============================================================================

cat("=== 07-model-spatial-durbin.R (v2) ===\n")
suppressPackageStartupMessages({ library(spdep); library(spatialreg) })
set.seed(42)
source("code/paths.R")

dataset <- read.csv(file.path(DATA_PROCESSED, "integrated_dataset.csv"), stringsAsFactors = FALSE)
edges <- read.csv(file.path(DATA_PROCESSED, "genesis_network_initiative.csv"), stringsAsFactors = FALSE)
cat(sprintf("  Dataset: %d filas | aristas W: %d\n", nrow(dataset), nrow(edges)))

complete <- dataset[!is.na(dataset$retention_all) & !is.na(dataset$theta_mean) &
                      !is.na(dataset$theta_sd) & !is.na(dataset$es_mujer) &
                      !is.na(dataset$es_abogado) & !is.na(dataset$experiencia_previa_institucional) &
                      !is.na(dataset$edad_al_asumir) & !is.na(dataset$grado_academico_nivel) &
                      !is.na(dataset$ego_heterophily) & dataset$degree > 0, ]
cat(sprintf("  Casos completos: %d de %d\n", nrow(complete), nrow(dataset)))

build_listw <- function(sample_names, edges_df, binary = FALSE) {
  n <- length(sample_names)
  W <- matrix(0, n, n, dimnames = list(sample_names, sample_names))
  for (i in seq_len(nrow(edges_df))) {
    s <- edges_df$source[i]; t <- edges_df$target[i]
    if (s %in% sample_names && t %in% sample_names) {
      w <- if (binary) 1 else edges_df$weight[i]
      W[s, t] <- w; W[t, s] <- w
    }
  }
  rs <- rowSums(W); rs[rs == 0] <- 1
  mat2listw(W / rs, style = "W")
}
W_listw <- build_listw(complete$nombre_armonizado, edges)
set.ZeroPolicyOption(TRUE)

cat("\n--- Moran's I (retention_all) ---\n")
moran_res <- moran.test(complete$retention_all, W_listw)
cat(sprintf("  I = %.4f (esperado %.4f), z = %.2f, p = %.2e\n",
            moran_res$estimate[1], moran_res$estimate[2],
            moran_res$statistic, moran_res$p.value))

formula_base <- retention_all ~ degree + betweenness + es_abogado +
  experiencia_previa_institucional + es_mujer + edad_al_asumir +
  grado_academico_nivel + theta_mean + theta_sd + ego_heterophily

cat("\n--- OLS base ---\n")
mod_ols <- lm(formula_base, data = complete)
print(round(coef(summary(mod_ols)), 4))

cat("\n--- Modelos espaciales ---\n")
mod_sar <- lagsarlm(formula_base, data = complete, listw = W_listw)
mod_sem <- errorsarlm(formula_base, data = complete, listw = W_listw)
mod_sdm <- lagsarlm(formula_base, data = complete, listw = W_listw, type = "mixed")
cat(sprintf("  SAR rho = %.3f | SEM lambda = %.3f | SDM rho = %.3f\n",
            mod_sar$rho, mod_sem$lambda, mod_sdm$rho))

cmp <- data.frame(
  model = c("OLS", "SEM", "SAR", "SDM"),
  AIC = c(AIC(mod_ols), AIC(mod_sem), AIC(mod_sar), AIC(mod_sdm)),
  logLik = c(as.numeric(logLik(mod_ols)), as.numeric(logLik(mod_sem)),
             as.numeric(logLik(mod_sar)), as.numeric(logLik(mod_sdm))),
  param = c(NA, mod_sem$lambda, mod_sar$rho, mod_sdm$rho)
)
print(cmp, row.names = FALSE, digits = 5)

cat("\n--- Impactos SDM (directo / indirecto / total) ---\n")
imp <- impacts(mod_sdm, listw = W_listw, R = 500)
imp_sum <- summary(imp, zstats = TRUE)
print(imp_sum)

dir.create(RESULTS_TABLES, recursive = TRUE, showWarnings = FALSE)
write.csv(cmp, file.path(RESULTS_TABLES, "M3_model_comparison.csv"), row.names = FALSE)
imp_df <- data.frame(variable = attr(imp, "bnames"),
                     direct = sapply(imp$res$direct, identity),
                     indirect = sapply(imp$res$indirect, identity),
                     total = sapply(imp$res$total, identity))
write.csv(imp_df, file.path(RESULTS_TABLES, "M3_sdm_impacts.csv"), row.names = FALSE)
write.csv(imp_df, file.path(DATA_PROCESSED, "sdm_impacts.csv"), row.names = FALSE)

saveRDS(list(mod_ols = mod_ols, mod_sar = mod_sar, mod_sem = mod_sem,
             mod_sdm = mod_sdm, moran = moran_res, impacts = imp),
        file.path(DATA_PROCESSED, "sdm_results.rds"))
cat("--- Done ---\n")

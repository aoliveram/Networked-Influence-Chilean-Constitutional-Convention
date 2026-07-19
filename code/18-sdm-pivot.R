# =============================================================================
# 18-sdm-pivot.R  (respuesta a revisión: D6 — el rival institucional de rho)
# Test pivotal en M3: se agrega a X la distancia al pívot de 2/3,
#   dist_pivot_i = |theta1_fm_i - theta1_(103)|,
# donde theta1_(103) es el estadístico de orden que completa 103 votos (2/3 de
# 154 en ejercicio; IV.D4). Ideología del primer mes = pre-red y pre-comisiones.
# Si rho colapsa al incluirla, el "derrame de éxito por la red" era en realidad
# geometría espacial bajo supermajoría (D6). Si rho sobrevive, ambos canales
# coexisten.
#
# Output: results/tables/M3_sdm_pivot.csv
# =============================================================================

cat("=== 18-sdm-pivot.R ===\n")
suppressPackageStartupMessages({ library(spdep); library(spatialreg) })
set.seed(42)
source("code/paths.R")

dataset <- read.csv(file.path(DATA_PROCESSED, "integrated_dataset.csv"), stringsAsFactors = FALSE)
edges <- read.csv(file.path(DATA_PROCESSED, "genesis_network_initiative.csv"), stringsAsFactors = FALSE)
ip2d <- read.csv(file.path(DATA_PROCESSED, "ideal_points_2d_firstmonth.csv"), stringsAsFactors = FALSE)
registry <- read.csv(file.path(DATA_PROCESSED, "initiative_registry.csv"), stringsAsFactors = FALSE)
registry <- registry[registry$n_firmantes >= 2 & registry$n_firmantes <= 16, ]

# control de actividad (autor, 2026-07-18): nº total de iniciativas firmadas
# por cada convencional — el éxito podría ser mera exposición por volumen
firmas <- table(unlist(strsplit(registry$firmantes, "; ", fixed = TRUE)))
dataset$n_iniciativas <- as.integer(firmas[dataset$nombre_armonizado])
dataset$n_iniciativas[is.na(dataset$n_iniciativas)] <- 0

dataset$theta1_fm <- ip2d$theta1_fm[match(dataset$nombre_armonizado, ip2d$nombre_armonizado)]
pivot <- sort(ip2d$theta1_fm)[103]
dataset$dist_pivot <- abs(dataset$theta1_fm - pivot)
cat(sprintf("  Pívot 2/3: theta1_(103) = %.3f | dist_pivot: media %.3f, rango [%.3f, %.3f]\n",
            pivot, mean(dataset$dist_pivot, na.rm = TRUE),
            min(dataset$dist_pivot, na.rm = TRUE), max(dataset$dist_pivot, na.rm = TRUE)))

complete <- dataset[!is.na(dataset$retention_all) & !is.na(dataset$theta_mean) &
                      !is.na(dataset$theta_sd) & !is.na(dataset$es_mujer) &
                      !is.na(dataset$es_abogado) & !is.na(dataset$experiencia_previa_institucional) &
                      !is.na(dataset$edad_al_asumir) & !is.na(dataset$grado_academico_nivel) &
                      !is.na(dataset$ego_heterophily) & !is.na(dataset$dist_pivot) &
                      dataset$degree > 0, ]
cat(sprintf("  Casos completos: %d\n", nrow(complete)))

build_listw <- function(sample_names, edges_df) {
  n <- length(sample_names)
  W <- matrix(0, n, n, dimnames = list(sample_names, sample_names))
  for (i in seq_len(nrow(edges_df))) {
    s <- edges_df$source[i]; t <- edges_df$target[i]
    if (s %in% sample_names && t %in% sample_names) {
      W[s, t] <- edges_df$weight[i]; W[t, s] <- edges_df$weight[i]
    }
  }
  rs <- rowSums(W); rs[rs == 0] <- 1
  mat2listw(W / rs, style = "W")
}
W_listw <- build_listw(complete$nombre_armonizado, edges)
set.ZeroPolicyOption(TRUE)

f_base <- retention_all ~ n_iniciativas + degree + betweenness + es_abogado +
  experiencia_previa_institucional + es_mujer + edad_al_asumir +
  grado_academico_nivel + theta_mean + theta_sd + ego_heterophily
f_piv <- update(f_base, . ~ . + dist_pivot)

fits <- list(
  "OLS base" = lm(f_base, data = complete),
  "OLS + dist_pivot" = lm(f_piv, data = complete),
  "SDM base" = lagsarlm(f_base, data = complete, listw = W_listw, type = "mixed"),
  "SDM + dist_pivot" = lagsarlm(f_piv, data = complete, listw = W_listw, type = "mixed")
)

rows <- list()
for (lab in names(fits)) {
  m <- fits[[lab]]
  if (inherits(m, "Sarlm")) {
    sm <- summary(m)
    co <- sm$Coef
    rows[[lab]] <- data.frame(model = lab, term = rownames(co), estimate = co[, 1],
                              se = co[, 2], p = co[, 4],
                              rho = m$rho, rho_se = sm$rho.se,
                              aic = AIC(m), row.names = NULL)
  } else {
    co <- coef(summary(m))
    rows[[lab]] <- data.frame(model = lab, term = rownames(co), estimate = co[, 1],
                              se = co[, 2], p = co[, 4], rho = NA, rho_se = NA,
                              aic = AIC(m), row.names = NULL)
  }
}
tab <- do.call(rbind, rows)
dir.create(RESULTS_TABLES, recursive = TRUE, showWarnings = FALSE)
write.csv(tab, file.path(RESULTS_TABLES, "M3_sdm_pivot.csv"), row.names = FALSE)

cat("\n--- Resumen: rho y dist_pivot ---\n")
for (lab in names(fits)) {
  m <- fits[[lab]]
  piv <- tab[tab$model == lab & tab$term %in% c("dist_pivot", "lag.dist_pivot"), ]
  cat(sprintf("  %-18s AIC %8.2f | rho = %s | %s\n", lab, AIC(m),
              if (inherits(m, "Sarlm")) sprintf("%.3f", m$rho) else "—",
              if (nrow(piv)) paste(sprintf("%s = %.4f (p=%.3g)", piv$term, piv$estimate, piv$p),
                                   collapse = " | ") else ""))
}
cat("--- Done ---\n")

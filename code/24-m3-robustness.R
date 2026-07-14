# =============================================================================
# 24-m3-robustness.R
# Variantes de robustez de M3 (portadas del antiguo 08, archivado en
# old-version/scripts/, para poder recalcularlas sobre la red vigente ≤16):
#   (d) DV condicional antigua, (e) W binaria, (f) W artículo, (g) DV SBERT.
# Output: results/tables/M3_robustness.csv
# =============================================================================

cat("=== 24-m3-robustness.R ===\n")
suppressPackageStartupMessages({ library(spdep); library(spatialreg) })
set.seed(42)
source("code/paths.R")

dataset <- read.csv(file.path(DATA_PROCESSED, "integrated_dataset.csv"), stringsAsFactors = FALSE)
edges_init <- read.csv(file.path(DATA_PROCESSED, "genesis_network_initiative.csv"),
                       stringsAsFactors = FALSE)
edges_art <- read.csv(file.path(DATA_PROCESSED, "genesis_network_article.csv"),
                      stringsAsFactors = FALSE)

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
set.ZeroPolicyOption(TRUE)

run_sdm <- function(dv, edges_df, label, binary = FALSE) {
  d <- dataset[!is.na(dataset[[dv]]) & !is.na(dataset$theta_mean) &
                 !is.na(dataset$es_mujer) & !is.na(dataset$grado_academico_nivel) &
                 dataset$degree > 0, ]
  lw <- build_listw(d$nombre_armonizado, edges_df, binary = binary)
  f <- as.formula(paste(dv, "~ degree + betweenness + es_abogado +",
                        "experiencia_previa_institucional + es_mujer + edad_al_asumir +",
                        "grado_academico_nivel + theta_mean + theta_sd + ego_heterophily"))
  ols <- lm(f, data = d)
  sdm <- tryCatch(lagsarlm(f, data = d, listw = lw, type = "mixed"),
                  error = function(e) NULL)
  mor <- moran.test(d[[dv]], lw)
  data.frame(variant = label, N = nrow(d),
             moran_I = mor$estimate[1], moran_p = mor$p.value,
             rho = if (!is.null(sdm)) sdm$rho else NA,
             AIC_ols = AIC(ols), AIC_sdm = if (!is.null(sdm)) AIC(sdm) else NA)
}

m3 <- rbind(
  run_sdm("retention_all", edges_init, "y' | W iniciativa (principal)"),
  run_sdm("retention_traced", edges_init, "y condicional (DV antigua) | W iniciativa"),
  run_sdm("retention_all", edges_init, "y' | W binaria", binary = TRUE),
  run_sdm("retention_all", edges_art, "y' | W artículo"),
  if ("retention_all_sbert" %in% names(dataset) && any(!is.na(dataset$retention_all_sbert)))
    run_sdm("retention_all_sbert", edges_init, "y' SBERT | W iniciativa") else NULL
)
print(m3, row.names = FALSE, digits = 4)
dir.create(RESULTS_TABLES, recursive = TRUE, showWarnings = FALSE)
write.csv(m3, file.path(RESULTS_TABLES, "M3_robustness.csv"), row.names = FALSE)
cat("--- Done ---\n")

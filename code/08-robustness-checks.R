# =============================================================================
# 08-robustness-checks.R  (v2, actualización 7 comisiones)
# Robustez de M1 y M3 (las robusteces de ventana de M2 viven en 03):
#
# M1: (a) ERGM por comisión (red génesis-iniciativa T0 de cada una),
#     (b) ERGM pooled con unidad ARTÍCULO (punto a revisar: iniciativa vs artículo),
#     (c) ERGM pooled iniciativa con los PERFILES ANTIGUOS pre-auditoría
#         (git 8b5a990) — descompone si el vuelco de H1b viene de las
#         covariables corregidas o de la definición de red.
# M3: (d) DV antigua condicional (retention_traced), (e) W binaria,
#     (f) W con red artículo, (g) DV SBERT (y').
# =============================================================================

cat("=== 08-robustness-checks.R (v2) ===\n")
suppressPackageStartupMessages({
  library(jsonlite); library(statnet); library(spdep); library(spatialreg)
})
set.seed(42)
source("code/paths.R")

profiles <- fromJSON(PROFILES)
roster <- sort(fromJSON(MEMBERS))
dir.create(RESULTS_TABLES, recursive = TRUE, showWarnings = FALSE)

ATTRS <- c("afiliacion_agrupada", "es_abogado", "es_mujer", "edad_al_asumir",
           "experiencia_previa_institucional", "grado_academico_nivel")

build_net <- function(edges_df, prof) {
  nodes <- roster
  net <- network.initialize(length(nodes), directed = FALSE)
  network.vertex.names(net) <- nodes
  for (col in ATTRS) {
    set.vertex.attribute(net, col, prof[[col]][match(nodes, prof$nombre_armonizado)])
  }
  add.edges(net, tail = match(edges_df$source, nodes), head = match(edges_df$target, nodes))
  set.edge.attribute(net, "weight", edges_df$weight)
  net
}

TERMS <- paste("sum + nodematch('afiliacion_agrupada') +",
               "nodematch('experiencia_previa_institucional') + nodematch('es_abogado') +",
               "nodematch('es_mujer') + absdiff('edad_al_asumir') +",
               "absdiff('grado_academico_nivel') + nodecov('edad_al_asumir')")

fit_ergm <- function(net, label) {
  cat(sprintf("\n[ERGM] %s ...\n", label))
  f <- as.formula(paste("net ~", TERMS), env = environment())
  tryCatch({
    fit <- ergm(f, response = "weight", reference = ~Poisson,
                control = control.ergm(seed = 42))
    sm <- summary(fit)
    data.frame(model = label, term = rownames(sm$coefficients),
               estimate = sm$coefficients[, 1], se = sm$coefficients[, 2],
               p = sm$coefficients[, ncol(sm$coefficients)], row.names = NULL)
  }, error = function(e) {
    cat(sprintf("  ERROR: %s\n", conditionMessage(e)))
    data.frame(model = label, term = "ERROR", estimate = NA, se = NA, p = NA)
  })
}

ergm_tabs <- list()

# (a) por comisión: T0 génesis de cada dynamic_networks.json
for (k in 1:7) {
  nd <- fromJSON(file.path(DATA_PROCESSED, sprintf("C%d_dynamic_networks.json", k)),
                 simplifyVector = FALSE)
  t0 <- nd[["T0_Genesis"]]
  edges_df <- do.call(rbind, lapply(t0, function(e)
    data.frame(source = e$source, target = e$target, weight = e$weight)))
  ergm_tabs[[length(ergm_tabs) + 1]] <- fit_ergm(build_net(edges_df, profiles),
                                                 sprintf("C%d (génesis-iniciativa)", k))
}

# (b) pooled, unidad artículo
edges_art <- read.csv(file.path(DATA_PROCESSED, "genesis_network_article.csv"),
                      stringsAsFactors = FALSE)
ergm_tabs[[length(ergm_tabs) + 1]] <- fit_ergm(build_net(edges_art, profiles),
                                               "Pooled (unidad artículo)")

# (c) pooled iniciativa con perfiles ANTIGUOS (pre-auditoría, git 8b5a990)
old_json <- tryCatch(
  system2("git", c("show", "8b5a990:data/raw/conventional-profiles.json"),
          stdout = TRUE, stderr = FALSE),
  error = function(e) NULL)
if (!is.null(old_json) && length(old_json) > 0) {
  old_prof <- fromJSON(paste(old_json, collapse = "\n"))
  # esquema antiguo: 147 perfiles, defaults para los 7 ausentes, grado 0-2
  missing <- setdiff(roster, old_prof$nombre_armonizado)
  if (length(missing) > 0) {
    defaults <- data.frame(nombre_armonizado = missing, es_mujer = 0,
                           afiliacion_agrupada = "Desconocida", distrito = "Desconocido",
                           es_abogado = 0, edad_al_asumir = 45,
                           grado_academico_nivel = 0, experiencia_previa_institucional = 0)
    old_prof <- rbind(old_prof[, names(defaults)], defaults)
  }
  old_prof$edad_al_asumir[is.na(old_prof$edad_al_asumir)] <- 45
  edges_init <- read.csv(file.path(DATA_PROCESSED, "genesis_network_initiative.csv"),
                         stringsAsFactors = FALSE)
  ergm_tabs[[length(ergm_tabs) + 1]] <- fit_ergm(build_net(edges_init, old_prof),
                                                 "Pooled iniciativa (perfiles PRE-auditoría)")
} else {
  cat("  (no se pudo extraer el perfil antiguo desde git; variante omitida)\n")
}

ergm_tab <- do.call(rbind, ergm_tabs)
write.csv(ergm_tab, file.path(RESULTS_TABLES, "M1_robustness.csv"), row.names = FALSE)
cat("\nM1 robustez -> results/tables/M1_robustness.csv\n")

# ---------------------------- M3: variantes -------------------------------
dataset <- read.csv(file.path(DATA_PROCESSED, "integrated_dataset.csv"), stringsAsFactors = FALSE)
edges_init <- read.csv(file.path(DATA_PROCESSED, "genesis_network_initiative.csv"),
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
write.csv(m3, file.path(RESULTS_TABLES, "M3_robustness.csv"), row.names = FALSE)
saveRDS(list(ergm = ergm_tab, m3 = m3), file.path(DATA_PROCESSED, "robustness_results.rds"))
cat("--- Done ---\n")

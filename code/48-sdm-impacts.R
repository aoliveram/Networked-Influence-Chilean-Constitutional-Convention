# =============================================================================
# 48-sdm-impacts.R  (IV-P.4 puntos 1-3, aprobados por el autor 2026-07-21)
# Upgrades del SDM tomados del workshop del autor (3-network-from-surveys.qmd):
#   1. Descomposición de impactos Directo/Indirecto/Total. Con N = 154 el
#      multiplicador (I - rho W)^{-1} se calcula EXACTO (denso) — sin la
#      aproximación por trazas que era inestable con rho ~ 0.9.
#      Inferencia: 200 draws MVN de (rho, beta, gamma) con la covarianza ML.
#   2. Tabla "beta OLS vs impacto TOTAL" con % de reducción (la comparación
#      correcta; comparar OLS contra el beta crudo del SDM es aproximación).
#   3. Contrafactuales: (a) clima: rho * (Wy_p90 - Wy_p10); (b) covariable:
#      mover a UN convencional de P10 a P90 en x_r y propagar por el
#      multiplicador exacto -> efecto propio + derrame al resto.
# Modelo: el "SDM + dist_pivot" de la Tabla 13 (mismos datos que code/18).
#
# Outputs: results/tables/M3_sdm_impacts_exact.csv (+ consola)
# =============================================================================

cat("=== 48-sdm-impacts.R ===\n")
suppressPackageStartupMessages({ library(spdep); library(spatialreg); library(MASS) })
set.seed(42)
source("code/paths.R")

dataset <- read.csv(file.path(DATA_PROCESSED, "integrated_dataset.csv"), stringsAsFactors = FALSE)
edges <- read.csv(file.path(DATA_PROCESSED, "genesis_network_initiative.csv"), stringsAsFactors = FALSE)
ip2d <- read.csv(file.path(DATA_PROCESSED, "ideal_points_2d_firstmonth.csv"), stringsAsFactors = FALSE)
registry <- read.csv(file.path(DATA_PROCESSED, "initiative_registry.csv"), stringsAsFactors = FALSE)
registry <- registry[registry$n_firmantes >= 2 & registry$n_firmantes <= 16, ]
firmas <- table(unlist(strsplit(registry$firmantes, "; ", fixed = TRUE)))
dataset$n_iniciativas <- as.integer(firmas[dataset$nombre_armonizado])
dataset$n_iniciativas[is.na(dataset$n_iniciativas)] <- 0
dataset$theta1_fm <- ip2d$theta1_fm[match(dataset$nombre_armonizado, ip2d$nombre_armonizado)]
pivot <- sort(ip2d$theta1_fm)[103]
dataset$dist_pivot <- abs(dataset$theta1_fm - pivot)
complete <- dataset[!is.na(dataset$retention_all) & !is.na(dataset$theta_mean) &
                      !is.na(dataset$theta_sd) & !is.na(dataset$es_mujer) &
                      !is.na(dataset$es_abogado) & !is.na(dataset$experiencia_previa_institucional) &
                      !is.na(dataset$edad_al_asumir) & !is.na(dataset$grado_academico_nivel) &
                      !is.na(dataset$ego_heterophily) & !is.na(dataset$dist_pivot) &
                      dataset$degree > 0, ]
n <- nrow(complete)
Wm <- matrix(0, n, n, dimnames = list(complete$nombre_armonizado, complete$nombre_armonizado))
for (i in seq_len(nrow(edges))) {
  s <- edges$source[i]; t <- edges$target[i]
  if (s %in% rownames(Wm) && t %in% rownames(Wm)) { Wm[s, t] <- edges$weight[i]; Wm[t, s] <- edges$weight[i] }
}
rs <- rowSums(Wm); rs[rs == 0] <- 1
Wn <- Wm / rs
W_listw <- mat2listw(Wn, style = "W")
set.ZeroPolicyOption(TRUE)

XVARS <- c("n_iniciativas", "degree", "betweenness", "es_abogado",
           "experiencia_previa_institucional", "es_mujer", "edad_al_asumir",
           "grado_academico_nivel", "theta_mean", "theta_sd", "ego_heterophily",
           "dist_pivot")
f <- as.formula(paste("retention_all ~", paste(XVARS, collapse = " + ")))
fit <- lagsarlm(f, data = complete, listw = W_listw, type = "mixed")
ols <- lm(f, data = complete)
rho <- fit$rho
co <- coef(fit)                                # incluye rho, betas y lag.*
beta <- co[XVARS]
gama <- co[paste0("lag.", XVARS)]
cat(sprintf("  Modelo: SDM + dist_pivot | rho = %.3f | n = %d\n", rho, n))

# ---------------- 1. impactos EXACTOS + inferencia MC ------------------------
In <- diag(n)
impacts_exact <- function(rho_, beta_, gama_) {
  A <- solve(In - rho_ * Wn)
  sapply(XVARS, function(v) {
    S <- A %*% (beta_[v] * In + gama_[paste0("lag.", v)] * Wn)
    tot <- mean(rowSums(S)); dir <- mean(diag(S))
    c(direct = dir, indirect = tot - dir, total = tot)
  })
}
imp0 <- impacts_exact(rho, beta, gama)          # 3 x length(XVARS)

V <- fit$resvar
cat("  resvar dims:", nrow(V), "x", ncol(V), "| primeras filas:", paste(head(rownames(V), 3), collapse=", "), "\n")
keep <- which(rownames(V) %in% c("rho", XVARS, paste0("lag.", XVARS)))
V <- V[keep, keep]
nm <- rownames(V)
th_all <- c(rho = unname(rho), beta, gama)
th_hat <- th_all[nm]
stopifnot(!anyNA(th_hat))
V <- (V + t(V)) / 2                              # simetrizar por seguridad numerica
R <- 200
draws <- MASS::mvrnorm(R, mu = th_hat, Sigma = V, tol = 1e-8)
tot_d <- dir_d <- matrix(NA_real_, R, length(XVARS), dimnames = list(NULL, XVARS))
for (r in seq_len(R)) {
  d <- draws[r, ]
  bb <- d[XVARS]; gg <- d[paste0("lag.", XVARS)]
  im <- impacts_exact(unname(d["rho"]), bb, gg)
  dir_d[r, ] <- im["direct", ]; tot_d[r, ] <- im["total", ]
}
tab <- data.frame(
  term = XVARS,
  ols = coef(ols)[XVARS],
  direct = imp0["direct", ], indirect = imp0["indirect", ], total = imp0["total", ],
  se_total = apply(tot_d, 2, sd),
  p_total = 2 * pnorm(-abs(imp0["total", ] / apply(tot_d, 2, sd))),
  row.names = NULL)
tab$pct_reduccion <- 100 * (1 - tab$total / tab$ols)
cat("\n--- Impactos exactos (MC R = 200) vs OLS ---\n")
print(tab, row.names = FALSE, digits = 2)
write.csv(tab, file.path(RESULTS_TABLES, "M3_sdm_impacts_exact.csv"), row.names = FALSE)

# ---------------- 3a. contrafactual de clima ---------------------------------
Wy <- as.numeric(Wn %*% complete$retention_all)
d_clima <- rho * (quantile(Wy, .9) - quantile(Wy, .1))
cat(sprintf("\n  Contrafactual de clima: rho*(Wy_p90 - Wy_p10) = %.3f*(%.3f - %.3f) = %.3f\n",
            rho, quantile(Wy, .9), quantile(Wy, .1), d_clima))
cat(sprintf("  (referencia: sd(y) = %.3f, media(y) = %.3f)\n",
            sd(complete$retention_all), mean(complete$retention_all)))

# ---------------- 3b. contrafactual de covariable P10 -> P90 -----------------
A <- solve(In - rho * Wn)
cat("\n--- Mover a UN convencional de P10 a P90 en x_r (promedio sobre quién) ---\n")
cf <- do.call(rbind, lapply(c("dist_pivot", "n_iniciativas", "es_abogado",
                              "experiencia_previa_institucional"), function(v) {
  x <- complete[[v]]
  dx <- if (length(unique(x)) <= 2) 1 else unname(quantile(x, .9) - quantile(x, .1))
  S <- A %*% (beta[v] * In + gama[paste0("lag.", v)] * Wn)
  own <- mean(diag(S)) * dx                     # efecto sobre si mismo
  spill <- mean(colSums(S) - diag(S)) * dx      # suma del derrame sobre TODOS los demas
  data.frame(term = v, delta_x = dx, efecto_propio = own, derrame_total = spill)
}))
print(cf, row.names = FALSE, digits = 2)
write.csv(cf, file.path(RESULTS_TABLES, "M3_sdm_counterfactuals.csv"), row.names = FALSE)
cat("--- Done ---\n")

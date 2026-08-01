# =============================================================================
# 25-ergm-initiative-bootstrap.R  (revisión D1: inferencia honesta para el
# ERGM valuado proyectado — Tier A completo)
#
# La especificación díado-independiente factoriza como Poisson sobre díadas
# (equivalencia verificada en 23), así que el bootstrap por INICIATIVAS es
# barato: se re-muestrean las 487 iniciativas (<=16, decisión D8) con
# reemplazo, se reconstruye W y se re-ajusta el glm. El acto que genera las
# observaciones (la iniciativa) pasa a ser la unidad de re-muestreo — la
# corrección directa a la pseudo-replicación de cliques (D1).
#
# Output: results/tables/M1_ergm_bootstrap.csv
#   (coef Poisson, SE Poisson ingenuo, SE bootstrap, IC percentil 95%, razón)
# =============================================================================

cat("=== 25-ergm-initiative-bootstrap.R ===\n")
suppressPackageStartupMessages({ library(jsonlite) })
set.seed(42)
source("code/paths.R")
T0 <- Sys.time()
B <- 1000

profiles <- fromJSON(PROFILES)
roster <- sort(fromJSON(MEMBERS))
ip2d <- read.csv(file.path(DATA_PROCESSED, "ideal_points_2d_firstmonth.csv"), stringsAsFactors = FALSE)
registry <- read.csv(file.path(DATA_PROCESSED, "initiative_registry.csv"), stringsAsFactors = FALSE)
registry <- registry[registry$n_firmantes >= 2 & registry$n_firmantes <= 16, ]  # D8

n <- length(roster)
att <- function(col) profiles[[col]][match(roster, profiles$nombre_armonizado)]
af <- att("afiliacion_agrupada"); ab <- att("es_abogado"); mu <- att("es_mujer")
ed <- att("edad_al_asumir"); gr <- att("grado_academico_nivel")
ex <- att("experiencia_previa_institucional")
t1 <- ip2d$theta1_fm[match(roster, ip2d$nombre_armonizado)]
t2 <- ip2d$theta2_fm[match(roster, ip2d$nombre_armonizado)]

iu <- rep(1:n, times = n); ju <- rep(1:n, each = n)
keep <- iu < ju; iu <- iu[keep]; ju <- ju[keep]
X <- data.frame(
  nm_afiliacion = as.integer(af[iu] == af[ju]),
  nm_experiencia = as.integer(ex[iu] == ex[ju]),
  nm_abogado = as.integer(ab[iu] == ab[ju]),
  nm_mujer = as.integer(mu[iu] == mu[ju]),
  ad_edad = abs(ed[iu] - ed[ju]),
  ad_grado = abs(gr[iu] - gr[ju]),
  nc_edad = ed[iu] + ed[ju],
  ad_theta1 = abs(t1[iu] - t1[ju]),
  ad_theta2 = abs(t2[iu] - t2[ju])
)
memb <- lapply(seq_len(nrow(registry)), function(k) {
  S <- match(strsplit(registry$firmantes[k], "; ", fixed = TRUE)[[1]], roster)
  S[!is.na(S)]
})
w_from <- function(mm) {
  W <- matrix(0L, n, n)
  for (S in mm) W[S, S] <- W[S, S] + 1L
  diag(W) <- 0L
  W[cbind(iu, ju)]
}

w_obs <- w_from(memb)
fit0 <- glm(w_obs ~ ., data = X, family = poisson())
cat(sprintf("  Punto: glm Poisson sobre %d díadas | %d iniciativas | B = %d\n",
            length(w_obs), length(memb), B))

boot <- t(replicate(B, {
  coef(glm(w_from(memb[sample(length(memb), replace = TRUE)]) ~ .,
           data = X, family = poisson()))
}))

tab <- data.frame(
  term = names(coef(fit0)),
  estimate = coef(fit0),
  se_poisson = summary(fit0)$coefficients[, 2],
  se_boot = apply(boot, 2, sd),
  ci_lo = apply(boot, 2, quantile, 0.025),
  ci_hi = apply(boot, 2, quantile, 0.975), row.names = NULL
)
tab$ratio_se <- tab$se_boot / tab$se_poisson
tab$sig_boot <- ifelse(tab$ci_lo > 0 | tab$ci_hi < 0, "*", "")
print(tab, row.names = FALSE, digits = 3)
dir.create(RESULTS_TABLES, recursive = TRUE, showWarnings = FALSE)
write.csv(tab, file.path(RESULTS_TABLES, "M1_ergm_bootstrap.csv"), row.names = FALSE)
cat(sprintf("--- Done (%.1f min) ---\n", as.numeric(difftime(Sys.time(), T0, units = "mins"))))

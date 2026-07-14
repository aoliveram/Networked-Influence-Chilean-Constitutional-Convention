# =============================================================================
# 23-ergm-bootstrap-pilot.R  (revisión D1: ¿bootstrap por iniciativas?)
# PILOTO — no corre el bootstrap completo; solo (a) verifica la equivalencia,
# (b) mide el costo por réplica y extrapola el tiempo total.
#
# La observación que ordena todo: la especificación del ERGM valuado del
# proyecto (sum + nodematch + absdiff + nodecov, referencia Poisson) es
# DÍADO-INDEPENDIENTE ⇒ la verosimilitud factoriza sobre díadas y el "ERGM"
# es EXACTAMENTE una regresión de Poisson sobre las 11.781 díadas:
#     w_ij ~ Poisson(exp(theta' x_ij)).
# Por eso: (a) los coeficientes MCMLE archivados deben coincidir con glm();
# (b) el bootstrap por iniciativas (re-muestrear las 487 iniciativas con
#     reemplazo, reconstruir W, re-ajustar) cuesta milisegundos por réplica
#     — Tier A. Solo si se agregan términos ESTRUCTURALES (p. ej.
#     transitiveweights, "más información de red") se pierde la factorización
#     y cada réplica exige MCMC — Tier B, cuyo costo se mide aquí con un
#     ajuste corto (MCMLE.maxit = 3) y se extrapola.
#
# Outputs: results/tables/D1_bootstrap_pilot.csv (+ consola)
# =============================================================================

cat("=== 23-ergm-bootstrap-pilot.R ===\n")
suppressPackageStartupMessages({ library(jsonlite); library(statnet) })
set.seed(42)
source("code/paths.R")

profiles <- fromJSON(PROFILES)
roster <- sort(fromJSON(MEMBERS))
ip2d <- read.csv(file.path(DATA_PROCESSED, "ideal_points_2d_firstmonth.csv"), stringsAsFactors = FALSE)
registry <- read.csv(file.path(DATA_PROCESSED, "initiative_registry.csv"), stringsAsFactors = FALSE)

n <- length(roster)
att <- function(col) profiles[[col]][match(roster, profiles$nombre_armonizado)]
af <- att("afiliacion_agrupada"); ab <- att("es_abogado"); mu <- att("es_mujer")
ed <- att("edad_al_asumir"); gr <- att("grado_academico_nivel")
ex <- att("experiencia_previa_institucional")
t1 <- ip2d$theta1_fm[match(roster, ip2d$nombre_armonizado)]
t2 <- ip2d$theta2_fm[match(roster, ip2d$nombre_armonizado)]

# --- covariables diádicas (mismas estadísticas del ERGM archivado) -----------
iu <- rep(1:n, times = n); ju <- rep(1:n, each = n)
keep <- iu < ju
iu <- iu[keep]; ju <- ju[keep]                     # 11.781 díadas i<j
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

# lista de miembros por iniciativa (índices en roster)
memb_all <- lapply(seq_len(nrow(registry)), function(k) {
  S <- match(strsplit(registry$firmantes[k], "; ", fixed = TRUE)[[1]], roster)
  S[!is.na(S)]
})
sizes <- vapply(memb_all, length, 1L)
memb_full <- memb_all[sizes >= 2]                  # >=2 (red archivada, sin tope)
memb_16 <- memb_all[sizes >= 2 & sizes <= 16]      # decisión D8 (red vigente)

w_from <- function(memb) {
  W <- matrix(0L, n, n)
  for (S in memb) {
    W[S, S] <- W[S, S] + 1L
  }
  diag(W) <- 0L
  W[cbind(iu, ju)]
}

# ------------------- (a) equivalencia glm-Poisson == MCMLE -------------------
# NOTA: la corrida original de este piloto (2026-07-11) comparó la red SIN tope
# contra el M1_ergm_ideologia.csv de esa fecha (pre-filtro): max|dif| = 0.0017.
# Tras la decisión D8, 13 regeneró ese CSV con la red <=16, así que la
# comparación reproducible usa memb_16.
cat("\n--- (a) Equivalencia: glm Poisson sobre díadas vs MCMLE archivado ---\n")
w_full <- w_from(memb_16)
glm_full <- glm(w_full ~ ., data = X, family = poisson())
arch <- tryCatch(read.csv(file.path(RESULTS_TABLES, "M1_ergm_ideologia.csv"),
                          stringsAsFactors = FALSE), error = function(e) NULL)
if (!is.null(arch)) {
  arch <- arch[arch$model == "Base + absdiff(theta1_fm) + absdiff(theta2_fm)", ]
  map <- c("(Intercept)" = "sum",
           nm_afiliacion = "nodematch.sum.afiliacion_agrupada",
           nm_experiencia = "nodematch.sum.experiencia_previa_institucional",
           nm_abogado = "nodematch.sum.es_abogado",
           nm_mujer = "nodematch.sum.es_mujer",
           ad_edad = "absdiff.sum.edad_al_asumir",
           ad_grado = "absdiff.sum.grado_academico_nivel",
           nc_edad = "nodecov.sum.edad_al_asumir",
           ad_theta1 = "absdiff.sum.theta1_fm",
           ad_theta2 = "absdiff.sum.theta2_fm")
  cmp <- data.frame(term = names(map),
                    glm = coef(glm_full)[names(map)],
                    mcmle = arch$estimate[match(map, arch$term)])
  cmp$dif <- cmp$glm - cmp$mcmle
  print(cmp, row.names = FALSE, digits = 4)
  cat(sprintf("  max |dif| = %.4f  (== equivalencia; el MCMC del 13 era innecesario para esta spec)\n",
              max(abs(cmp$dif), na.rm = TRUE)))
}

# ------------------- (b) Tier A: bootstrap por iniciativas con glm -----------
cat("\n--- (b) Tier A (spec actual, díado-independiente): timing de 10 réplicas ---\n")
B_PILOT <- 10
tA <- system.time({
  boot_A <- replicate(B_PILOT, {
    res <- memb_16[sample(length(memb_16), replace = TRUE)]
    wb <- w_from(res)
    coef(glm(wb ~ ., data = X, family = poisson()))["nm_afiliacion"]
  })
})["elapsed"]
per_rep_A <- tA / B_PILOT
cat(sprintf("  %.2f s por réplica -> B=1000: %.1f min secuencial, ~%.1f min en 8 cores\n",
            per_rep_A, per_rep_A * 1000 / 60, per_rep_A * 1000 / 60 / 8))
cat(sprintf("  (dispersión piloto de nodematch.afiliación entre réplicas: DE = %.3f)\n", sd(boot_A)))

# ------------------- (c) Tier B: spec estructural, MCMC corto ----------------
cat("\n--- (c) Tier B (+ transitiveweights, díado-DEPENDIENTE): 1 ajuste con MCMLE.maxit = 3 ---\n")
edges16 <- which(w_from(memb_16) > 0)
net <- network::network.initialize(n, directed = FALSE)
network::network.vertex.names(net) <- roster
for (col in c("afiliacion_agrupada", "es_abogado", "es_mujer", "edad_al_asumir",
              "experiencia_previa_institucional", "grado_academico_nivel")) {
  network::set.vertex.attribute(net, col, att(col))
}
w16 <- w_from(memb_16)
pos <- which(w16 > 0)
network::add.edges(net, tail = iu[pos], head = ju[pos])
network::set.edge.attribute(net, "weight", w16[pos])
f <- as.formula(paste("net ~ sum + nodematch('afiliacion_agrupada') +",
                      "nodematch('experiencia_previa_institucional') + nodematch('es_abogado') +",
                      "nodematch('es_mujer') + absdiff('edad_al_asumir') +",
                      "absdiff('grado_academico_nivel') + nodecov('edad_al_asumir') +",
                      "transitiveweights('min','max','min')"), env = environment())
tB <- system.time({
  fitB <- tryCatch(ergm(f, response = "weight", reference = ~Poisson,
                        control = control.ergm(seed = 42, MCMLE.maxit = 3)),
                   error = function(e) e)
})["elapsed"]
ok <- !inherits(fitB, "error")
per_iter_B <- tB / 3
est_fit_B <- per_iter_B * 25                       # ~25 iteraciones típicas para converger
cat(sprintf("  Ajuste corto (3 iteraciones): %.1f min%s\n", tB / 60,
            if (ok) "" else sprintf("  [ERROR: %s]", conditionMessage(fitB))))
cat(sprintf("  -> por ajuste completo (~25 iter): ~%.0f min; bootstrap B=200: ~%.1f h en 8 cores; B=500: ~%.1f h\n",
            est_fit_B / 60, est_fit_B * 200 / 3600 / 8, est_fit_B * 500 / 3600 / 8))

tab <- data.frame(
  tier = c("A: spec actual (glm Poisson)", "B: + transitiveweights (MCMC)"),
  seg_por_replica = c(per_rep_A, est_fit_B),
  B1000_min_8cores = c(per_rep_A * 1000 / 60 / 8, NA),
  B200_horas_8cores = c(per_rep_A * 200 / 60 / 8 / 60, est_fit_B * 200 / 3600 / 8),
  B500_horas_8cores = c(per_rep_A * 500 / 60 / 8 / 60, est_fit_B * 500 / 3600 / 8)
)
dir.create(RESULTS_TABLES, recursive = TRUE, showWarnings = FALSE)
write.csv(tab, file.path(RESULTS_TABLES, "D1_bootstrap_pilot.csv"), row.names = FALSE)
print(tab, row.names = FALSE, digits = 3)
cat("--- Done ---\n")

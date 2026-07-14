# =============================================================================
# 26-rhem-pilot.R  (revisión IV.P2: piloto de TIMING del RHEM con amorem)
#
# OBJETIVO: NO correr el RHEM completo — solo medir, de mínimo a complejo,
# cuánto cuesta cada pieza y extrapolar el costo de la estimación final.
#
# Escalera:
#   N0  hyperedge_features() sobre los eventos observados (solo casos)
#   N1  caso-control m=5:  subrep_1 + exógenas                (secuencial)
#   N2  caso-control m=5:  + subrep_2                         (secuencial)
#   N3  caso-control m=5:  + subrep_3                         (secuencial)
#   N4  = N2 pero estratos repartidos con mclapply(8 cores)
#   N5  = N2 paralelo con m=20 (para verificar escala ~lineal en 1+m)
#   FIT rem(method="clogit") sobre los datos de N2 (tiempo de ajuste)
#
# Spec objetivo a extrapolar: m=50, stats {subrep_1, subrep_2, subrep_3} +
# exógenas, x10 re-muestreos de controles (chequeo de estabilidad estándar
# de la literatura RHEM). Los coeficientes impresos son PILOTO, no resultados.
#
# Tiempo del evento (PROVISORIO, solo para el piloto): rango del prefijo
# numérico del ICC (483/487 lo tienen; empates por orden de id; los 4 sin
# prefijo van al final). El run real debe verificar fechas oficiales de
# ingreso de las ICC.
#
# Output: results/tables/RHEM_pilot_timing.csv
# =============================================================================

cat("=== 26-rhem-pilot.R ===\n")
suppressPackageStartupMessages({
  library(jsonlite); library(amorem); library(survival); library(parallel)
})
set.seed(42)
source("code/paths.R")
cat(sprintf("  amorem %s | %d cores detectados\n",
            as.character(packageVersion("amorem")), detectCores()))

profiles <- fromJSON(PROFILES)
roster <- sort(fromJSON(MEMBERS))
ip2d <- read.csv(file.path(DATA_PROCESSED, "ideal_points_2d_firstmonth.csv"), stringsAsFactors = FALSE)
listas <- read.csv(file.path(DATA_RAW, "electoral_lists.csv"), stringsAsFactors = FALSE)
memb <- read.csv(file.path(DATA_RAW, "commission_membership.csv"), stringsAsFactors = FALSE)
registry <- read.csv(file.path(DATA_PROCESSED, "initiative_registry.csv"), stringsAsFactors = FALSE)
registry <- registry[registry$n_firmantes >= 2 & registry$n_firmantes <= 16, ]  # D8

# ------------------- orden temporal provisorio (prefijo ICC) -----------------
pref <- suppressWarnings(as.numeric(sub("^([0-9]+).*$", "\\1", registry$initiative_id)))
registry <- registry[order(is.na(pref), pref, registry$initiative_id), ]
registry$t <- seq_len(nrow(registry))            # rango estricto (sin empates)

S_list <- lapply(registry$firmantes, function(s) {
  x <- strsplit(s, "; ", fixed = TRUE)[[1]]
  x[x %in% roster]
})
stopifnot(all(vapply(S_list, length, 1L) >= 2))

# atributos indexados por nombre
theta1 <- setNames(ip2d$theta1_fm[match(roster, ip2d$nombre_armonizado)], roster)
congl <- setNames(listas$conglomerado[match(roster, listas$nombre_armonizado)], roster)
comis <- setNames(memb$commission[match(roster, memb$nombre_armonizado)], roster)

# ------------------- log de hipereventos (no dirigido) -----------------------
hl <- hyperedge_log(I = S_list,
                    J = replicate(length(S_list), character(0), simplify = FALSE),
                    time = registry$t)
cat(sprintf("  hyperedge_log: %d eventos | tamaños %d-%d\n",
            nrow(hl), min(lengths(S_list)), max(lengths(S_list))))

# sanity: subrep en el primer evento debe ser 0 (historia estrictamente pasada)
stopifnot(hyperedge_subrep(hl, I = S_list[[1]], t = registry$t[1], rho = 1) == 0)

# ------------------- covariables exógenas de un candidato --------------------
exog <- function(cand, commission) {
  th <- theta1[cand]
  pares <- combn(length(cand), 2)
  c(disp_theta1 = mean(abs(th[pares[1, ]] - th[pares[2, ]])),
    prop_lista = mean(congl[cand][pares[1, ]] == congl[cand][pares[2, ]]),
    prop_comision = mean(comis[cand] == commission))
}

# ------------------- constructor caso-control por estrato --------------------
build_stratum <- function(k, m, rhos, seed_off = 0) {
  set.seed(42 + k + seed_off)
  size <- length(S_list[[k]])
  cands <- c(list(S_list[[k]]),
             replicate(m, sample(roster, size), simplify = FALSE))
  do.call(rbind, lapply(seq_along(cands), function(j) {
    cand <- cands[[j]]
    sr <- vapply(rhos, function(r)
      hyperedge_subrep(hl, I = cand, t = registry$t[k], rho = r), numeric(1))
    names(sr) <- paste0("subrep_", rhos)
    c(stratum = k, case = as.integer(j == 1), sr, exog(cand, registry$commission[k]))
  }))
}
build_cc <- function(m, rhos, cores = 1) {
  worker <- function(k) build_stratum(k, m, rhos)
  rows <- if (cores > 1) mclapply(seq_len(nrow(registry)), worker, mc.cores = cores)
          else lapply(seq_len(nrow(registry)), worker)
  as.data.frame(do.call(rbind, rows))
}

# ------------------- escalera de mediciones ----------------------------------
res <- list()
tic <- function(expr) as.numeric(system.time(expr)["elapsed"])

cat("\n--- N0: features sobre eventos observados (subrep_1, subrep_2) ---\n")
t0 <- tic(hf <- hyperedge_features(hl, stats = c("subrep_1", "subrep_2")))
cat(sprintf("  %.1f s\n", t0))
res$N0 <- c(nivel = "N0 features casos", m = 0, cores = 1, seg = t0)

cat("--- N1: caso-control m=5, subrep_1 + exógenas (secuencial) ---\n")
t1 <- tic(cc1 <- build_cc(m = 5, rhos = 1))
cat(sprintf("  %.1f s (%d filas)\n", t1, nrow(cc1)))
res$N1 <- c(nivel = "N1 m=5 subrep1", m = 5, cores = 1, seg = t1)

cat("--- N2: + subrep_2 (secuencial) ---\n")
t2 <- tic(cc2 <- build_cc(m = 5, rhos = 1:2))
cat(sprintf("  %.1f s\n", t2))
res$N2 <- c(nivel = "N2 m=5 subrep1-2", m = 5, cores = 1, seg = t2)

cat("--- N3: + subrep_3 (secuencial) ---\n")
t3 <- tic(cc3 <- build_cc(m = 5, rhos = 1:3))
cat(sprintf("  %.1f s\n", t3))
res$N3 <- c(nivel = "N3 m=5 subrep1-3", m = 5, cores = 1, seg = t3)

cat("--- N4: = N3 con mclapply(8 cores) ---\n")
t4 <- tic(cc4 <- build_cc(m = 5, rhos = 1:3, cores = 8))
cat(sprintf("  %.1f s (speedup x%.1f)\n", t4, t3 / t4))
res$N4 <- c(nivel = "N4 m=5 subrep1-3 8c", m = 5, cores = 8, seg = t4)

cat("--- N5: m=20, subrep1-3, 8 cores (escala en m) ---\n")
t5 <- tic(cc5 <- build_cc(m = 20, rhos = 1:3, cores = 8))
cat(sprintf("  %.1f s (vs N4: x%.1f con (1+m) x%.1f)\n", t5, t5 / t4, 21 / 6))
res$N5 <- c(nivel = "N5 m=20 subrep1-3 8c", m = 20, cores = 8, seg = t5)

cat("--- FIT: rem(method='clogit') sobre N3 (m=5) ---\n")
cc3$stratum <- as.integer(cc3$stratum)
tf <- tic(fit <- rem(case ~ subrep_1 + subrep_2 + subrep_3 + disp_theta1 +
                       prop_lista + prop_comision,
                     data = cc3, method = "clogit", stratum = "stratum"))
cat(sprintf("  %.2f s de ajuste\n", tf))
cat("  Coeficientes PILOTO (m=5, tiempo provisorio — NO son resultados):\n")
print(round(summary(fit)$coefficients, 3))
res$FIT <- c(nivel = "FIT clogit m=5", m = 5, cores = 1, seg = tf)

# ------------------- extrapolación al spec objetivo --------------------------
seg_por_candidato <- t5 / (nrow(registry) * 21)      # medición más informativa
target_seq <- seg_por_candidato * nrow(registry) * 51 * (t3 / t4)   # 1 core aprox
target_par <- seg_por_candidato * nrow(registry) * 51
cat(sprintf("\n=== Extrapolación (spec objetivo: m=50, subrep_1-3 + exógenas) ===\n"))
cat(sprintf("  ~%.3f s por candidato (medido en N5, 8 cores)\n", seg_por_candidato))
cat(sprintf("  1 estimación: ~%.1f min en 8 cores (~%.1f min secuencial)\n",
            target_par / 60, target_seq / 60))
cat(sprintf("  + 10 re-muestreos de estabilidad: ~%.1f min en 8 cores\n",
            target_par * 10 / 60))
cat(sprintf("  + ajuste clogit: ~%.1f s por estimación (despreciable)\n", tf))

tab <- as.data.frame(do.call(rbind, res))
tab$extrap_m50_8c_min <- round(target_par / 60, 2)
tab$extrap_m50_x10_8c_min <- round(target_par * 10 / 60, 2)
dir.create(RESULTS_TABLES, recursive = TRUE, showWarnings = FALSE)
write.csv(tab, file.path(RESULTS_TABLES, "RHEM_pilot_timing.csv"), row.names = FALSE)
cat("--- Done ---\n")

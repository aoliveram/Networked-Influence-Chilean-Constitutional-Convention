# =============================================================================
# 51-bipartite-suite-boot.R  (comentario del autor 2026-07-22: ETAPA 2)
# LA SUITE COMPLETA x7 con la especificación S2 (extendida + rangos continuos
# + gwb1degree + gwdsp) estimada por MPLE, con ERRORES ESTÁNDAR POR BOOTSTRAP
# DE INICIATIVAS (B = 500 por comisión: re-sortear las iniciativas de la
# comisión con reemplazo, reconstruir la red bipartita, re-estimar).
# NOTA: sigue siendo UN MODELO POR COMISIÓN (el agregado con intercepto único
# invierte signos — Simpson); "general" = la suite completa con la espec
# completa y la inferencia honesta.
#
# Output: results/tables/M1_bipartite_suite_boot.csv
# =============================================================================

cat("=== 51-bipartite-suite-boot.R (ETAPA 2: suite x7, S2, B = 500) ===\n")
suppressPackageStartupMessages({ library(jsonlite); library(statnet); library(parallel) })
set.seed(42)
source("code/paths.R")
T0 <- Sys.time()
B <- 500

profiles <- fromJSON(PROFILES); roster <- sort(fromJSON(MEMBERS))
ip2d <- read.csv(file.path(DATA_PROCESSED, "ideal_points_2d_firstmonth.csv"), stringsAsFactors = FALSE)
listas <- read.csv(file.path(DATA_RAW, "electoral_lists.csv"), stringsAsFactors = FALSE)
memb <- read.csv(file.path(DATA_RAW, "commission_membership.csv"), stringsAsFactors = FALSE)
registry <- read.csv(file.path(DATA_PROCESSED, "initiative_registry.csv"), stringsAsFactors = FALSE)
registry <- registry[registry$n_firmantes >= 2 & registry$n_firmantes <= 16, ]
n1 <- length(roster)
theta1 <- ip2d$theta1_fm[match(roster, ip2d$nombre_armonizado)]
theta1_q <- as.character(cut(theta1, quantile(theta1, probs = seq(0, 1, .2)), include.lowest = TRUE, labels = paste0("Q",1:5)))
congl <- listas$conglomerado[match(roster, listas$nombre_armonizado)]
congl[congl == "REVISAR (lista local sin conglomerado)"] <- "Otras"
comis_v <- memb$commission[match(roster, memb$nombre_armonizado)]
edad_v <- profiles$edad_al_asumir[match(roster, profiles$nombre_armonizado)]
grado_v <- profiles$grado_academico_nivel[match(roster, profiles$nombre_armonizado)]
edad_q <- as.character(cut(edad_v, quantile(edad_v, probs = seq(0, 1, .2)), include.lowest = TRUE, labels = paste0("E",1:5)))

make_ATR <- function(k) list(
  conglomerado = congl, theta1_q = theta1_q,
  es_abogado = profiles$es_abogado[match(roster, profiles$nombre_armonizado)],
  experiencia = profiles$experiencia_previa_institucional[match(roster, profiles$nombre_armonizado)],
  es_mujer = profiles$es_mujer[match(roster, profiles$nombre_armonizado)],
  distrito = profiles$distrito[match(roster, profiles$nombre_armonizado)],
  grado = as.character(grado_v), edad_q = edad_q,
  theta1_c = theta1, edad_c = edad_v / 10, grado_c = as.numeric(grado_v),
  miembro = as.integer(comis_v == sprintf("C%d", k)))

build_net <- function(reg, ATR) {
  n2 <- nrow(reg)
  net <- network::network.initialize(n1 + n2, directed = FALSE, bipartite = n1)
  for (a in names(ATR)) {
    v <- ATR[[a]]
    pad <- if (is.character(v)) "modo2" else if (a %in% c("theta1_c","edad_c","grado_c")) 0 else -1L
    network::set.vertex.attribute(net, a, c(v, rep(pad, n2)))
  }
  tails <- integer(0); heads <- integer(0)
  for (j in seq_len(n2)) {
    S <- match(strsplit(reg$firmantes[j], "; ", fixed = TRUE)[[1]], roster); S <- S[!is.na(S)]
    tails <- c(tails, S); heads <- c(heads, rep(n1 + j, length(S)))
  }
  network::add.edges(net, tail = tails, head = heads)
}

RHS <- paste("~ edges + b1cov(\"miembro\") + b1nodematch(\"conglomerado\") +",
             "b1nodematch(\"theta1_q\") + b1nodematch(\"distrito\") +",
             "b1nodematch(\"es_abogado\") + b1nodematch(\"experiencia\") +",
             "b1nodematch(\"es_mujer\") + b1nodematch(\"grado\") + b1nodematch(\"edad_q\") +",
             "b2covrange(\"theta1_c\") + b2covrange(\"edad_c\") + b2covrange(\"grado_c\") +",
             "gwb1degree(0.5, fixed = TRUE) + gwdsp(0.5, fixed = TRUE)")

fit_mple <- function(reg, ATR) {
  net <- build_net(reg, ATR)
  coef(ergm(as.formula(paste("net", RHS)), estimate = "MPLE",
            control = control.ergm(seed = 42)))
}

res <- list()
for (k in 1:7) {
  regk <- registry[registry$commission == sprintf("C%d", k), ]
  nE <- nrow(regk); ATR <- make_ATR(k)
  t0 <- Sys.time()
  punto <- fit_mple(regk, ATR)
  boot <- mclapply(1:B, function(b) {
    set.seed(7000 + k * 1000 + b)
    tryCatch(fit_mple(regk[sample(nE, nE, replace = TRUE), ], ATR), error = function(e) NULL)
  }, mc.cores = 8)
  boot <- boot[!sapply(boot, is.null)]
  bm <- do.call(rbind, boot)
  se_boot <- apply(bm, 2, sd)
  res[[k]] <- data.frame(commission = sprintf("C%d", k), n_inic = nE,
                         term = names(punto), estimate = punto,
                         se_boot = se_boot[names(punto)],
                         z = punto / se_boot[names(punto)],
                         p_boot = 2 * pnorm(-abs(punto / se_boot[names(punto)])),
                         B_ok = length(boot), row.names = NULL)
  cat(sprintf("  C%d: %d iniciativas, B validos = %d, %.1f min\n", k, nE, length(boot),
              as.numeric(difftime(Sys.time(), t0, units = "mins"))))
}
tab <- do.call(rbind, res)
write.csv(tab, file.path(RESULTS_TABLES, "M1_bipartite_suite_boot.csv"), row.names = FALSE)
cat(sprintf("--- Done (%.1f min) ---\n", as.numeric(difftime(Sys.time(), T0, units = "mins"))))

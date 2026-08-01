# =============================================================================
# 41-bipartite-nodemix-boot.R  (E4, comentario del autor post-PoliCICS:
# "¿no hay una forma de separar por estrato político?")
# La espec S3 oficial (51) con UNA modificación: b1nodematch(conglomerado)
# se reemplaza por el CENSO DE MEZCLA de pares de co-firmantes por bloque
# político — b2twostar(bloque, bloque): para cada documento, cada par de
# firmantes cuenta en la celda (bloque_i, bloque_j). 5 bloques (los mismos
# del clogit por bloque, code/31-clogit-by-bloc.R): Derecha (VC) | CentroIzq (LA+INN) |
# Izquierda (AD+LdP) | PPOO | Otras -> 15 celdas.
# MPLE + bootstrap de iniciativas (B = 500). Celdas vacías (-Inf) se
# reportan como tales; en el bootstrap los no-finitos van a NA.
#
# Output: results/tables/M1_bipartite_nodemix_boot.csv
# =============================================================================

cat("=== 41-bipartite-nodemix-boot.R (E4: mezcla por bloque politico, B = 500) ===\n")
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
theta2 <- ip2d$theta2_fm[match(roster, ip2d$nombre_armonizado)]
cg <- listas$conglomerado[match(roster, listas$nombre_armonizado)]
bloque <- ifelse(cg == "Vamos por Chile", "Derecha",
          ifelse(cg %in% c("Lista del Apruebo", "Independientes No Neutrales"), "CentroIzq",
          ifelse(cg %in% c("Apruebo Dignidad", "Lista del Pueblo"), "Izquierda",
          ifelse(cg == "Escaños Reservados PPOO", "PPOO", "Otras"))))
comis_v <- memb$commission[match(roster, memb$nombre_armonizado)]
edad_v <- profiles$edad_al_asumir[match(roster, profiles$nombre_armonizado)]
grado_v <- profiles$grado_academico_nivel[match(roster, profiles$nombre_armonizado)]

make_ATR <- function(k) list(
  bloque = bloque,
  es_abogado = profiles$es_abogado[match(roster, profiles$nombre_armonizado)],
  experiencia = profiles$experiencia_previa_institucional[match(roster, profiles$nombre_armonizado)],
  es_mujer = profiles$es_mujer[match(roster, profiles$nombre_armonizado)],
  distrito = profiles$distrito[match(roster, profiles$nombre_armonizado)],
  theta1_c = theta1, theta2_c = theta2,
  edad_c = edad_v / 10, grado_c = as.numeric(grado_v),
  miembro = as.integer(comis_v == sprintf("C%d", k)))

build_net <- function(reg, ATR) {
  n2 <- nrow(reg)
  net <- network::network.initialize(n1 + n2, directed = FALSE, bipartite = n1)
  for (a in names(ATR)) {
    v <- ATR[[a]]
    pad <- if (is.character(v)) "modo2" else if (a %in% c("theta1_c","theta2_c","edad_c","grado_c")) 0 else -1L
    network::set.vertex.attribute(net, a, c(v, rep(pad, n2)))
  }
  tails <- integer(0); heads <- integer(0)
  for (j in seq_len(n2)) {
    S <- match(strsplit(reg$firmantes[j], "; ", fixed = TRUE)[[1]], roster); S <- S[!is.na(S)]
    tails <- c(tails, S); heads <- c(heads, rep(n1 + j, length(S)))
  }
  network::add.edges(net, tail = tails, head = heads)
}

RHS <- paste("~ edges + b1cov(\"miembro\") +",
             "b2twostar(b1attr = \"bloque\", b2attr = \"bloque\") +",
             "b1nodematch(\"distrito\") +",
             "b1nodematch(\"es_abogado\") + b1nodematch(\"experiencia\") +",
             "b1nodematch(\"es_mujer\") +",
             "b2covrange(\"theta1_c\") + b2covrange(\"theta2_c\") +",
             "b2covrange(\"edad_c\") + b2covrange(\"grado_c\") +",
             "gwb1degree(0.5, fixed = TRUE) + gwdsp(0.5, fixed = TRUE)")

fit_mple <- function(reg, ATR) {
  net <- build_net(reg, ATR)
  coef(ergm(as.formula(paste("net", RHS)), estimate = "MPLE",
            control = control.ergm(seed = 42)))
}

fit_mple_hard <- function(reg, ATR, timeout = 25) {
  job <- parallel::mcparallel(fit_mple(reg, ATR))
  out <- parallel::mccollect(job, wait = FALSE, timeout = timeout)
  if (is.null(out)) {
    tools::pskill(job$pid, tools::SIGKILL)
    parallel::mccollect(job, wait = FALSE)
    return(NULL)
  }
  res <- out[[1]]
  if (inherits(res, "try-error") || is.character(res) || is.null(res)) return(NULL)
  res
}

res <- list()
for (k in 1:7) {
  regk <- registry[registry$commission == sprintf("C%d", k), ]
  nE <- nrow(regk); ATR <- make_ATR(k)
  t0 <- Sys.time()
  punto <- fit_mple(regk, ATR)
  boot <- mclapply(1:B, function(b) {
    set.seed(8000 + k * 1000 + b)
    tryCatch(fit_mple_hard(regk[sample(nE, nE, replace = TRUE), ], ATR),
             error = function(e) NULL)
  }, mc.cores = 8, mc.preschedule = FALSE)
  boot <- boot[!sapply(boot, is.null)]
  bm <- do.call(rbind, lapply(boot, function(v) v[names(punto)]))
  bm[!is.finite(bm)] <- NA                       # celdas vacias en resamples
  se_boot <- apply(bm, 2, sd, na.rm = TRUE)
  B_fin <- colSums(is.finite(bm))
  est <- punto; est[!is.finite(est)] <- NA       # celda vacia en la red real
  res[[k]] <- data.frame(commission = sprintf("C%d", k), n_inic = nE,
                         term = names(punto), estimate = est,
                         se_boot = se_boot[names(punto)],
                         z = est / se_boot[names(punto)],
                         p_boot = 2 * pnorm(-abs(est / se_boot[names(punto)])),
                         B_ok = length(boot), B_finite = B_fin[names(punto)],
                         empty_cell = as.integer(!is.finite(punto)), row.names = NULL)
  cat(sprintf("  C%d: %d iniciativas, B validos = %d, %.1f min\n", k, nE, length(boot),
              as.numeric(difftime(Sys.time(), t0, units = "mins"))))
}
tab <- do.call(rbind, res)
write.csv(tab, file.path(RESULTS_TABLES, "M1_bipartite_nodemix_boot.csv"), row.names = FALSE)
cat(sprintf("--- Done (%.1f min) ---\n", as.numeric(difftime(Sys.time(), T0, units = "mins"))))

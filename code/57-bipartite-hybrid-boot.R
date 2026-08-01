# =============================================================================
# 57-bipartite-hybrid-boot.R  (E6: el hibrido CLogit-ERGM)
# La espec S3 + las 15 celdas de mezcla del E4 (control de cierre de bloque)
# + CONTADORES POR BLOQUE para las covariables de perfil — la heterogeneidad
# del CLogit-por-bloque, pero dentro del ERGM bipartito con la red entera:
#   - para X en {abogado, experiencia, genero, distrito}: 5 columnas
#     "firmantes ya presentes del MISMO bloque que comparten mi valor de X"
#     + 1 columna cross (comparten X, distinto bloque). Particion exacta de
#     los two-stars de mismo-X.
#   - rango de theta1 del CONTINGENTE DEL PROPIO BLOQUE por documento
#     (5 columnas): ¿cuanto estira i el rango ideologico de su bloque en a?
# Estimacion: MPLE via glm sobre estadisticas de cambio (certificado contra
# ergm en C3, coef -Inf excluidos) + bootstrap de iniciativas (B = 500).
# Celdas sin pares observados se pre-dropean y reportan como vacias (= ergm).
#
# Output: results/tables/M1_bipartite_hybrid_boot.csv
# =============================================================================

cat("=== 57-bipartite-hybrid-boot.R (E6: hibrido, B = 500) ===\n")
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
BLOQS <- sort(unique(bloque))
comis_v <- memb$commission[match(roster, memb$nombre_armonizado)]
edad_v <- profiles$edad_al_asumir[match(roster, profiles$nombre_armonizado)]
grado_v <- profiles$grado_academico_nivel[match(roster, profiles$nombre_armonizado)]
ATRP <- list(es_abogado = profiles$es_abogado[match(roster, profiles$nombre_armonizado)],
             experiencia = profiles$experiencia_previa_institucional[match(roster, profiles$nombre_armonizado)],
             es_mujer = profiles$es_mujer[match(roster, profiles$nombre_armonizado)],
             distrito = as.character(profiles$distrito[match(roster, profiles$nombre_armonizado)]))

parse_signers <- function(reg) lapply(seq_len(nrow(reg)), function(j) {
  S <- match(strsplit(reg$firmantes[j], "; ", fixed = TRUE)[[1]], roster); S[!is.na(S)]
})

build_net <- function(signers, n2, miembro) {
  net <- network::network.initialize(n1 + n2, directed = FALSE, bipartite = n1)
  ATTRS <- list(bloque = bloque, miembro = miembro,
                theta1_c = theta1, theta2_c = theta2,
                edad_c = edad_v / 10, grado_c = as.numeric(grado_v))
  for (a in names(ATTRS)) {
    v <- ATTRS[[a]]
    pad <- if (is.character(v)) "modo2" else if (a == "miembro") -1L else 0
    network::set.vertex.attribute(net, a, c(v, rep(pad, n2)))
  }
  tails <- unlist(signers)
  heads <- unlist(lapply(seq_len(n2), function(j) rep(n1 + j, length(signers[[j]]))))
  network::add.edges(net, tail = tails, head = heads)
}

manual_cols <- function(signers, n2) {
  cols <- list()
  for (X_ in names(ATRP)) {
    for (g in c(BLOQS, "cross")) cols[[paste0("same_", X_, "_", g)]] <- matrix(0, n1, n2)
  }
  for (g in BLOQS) cols[[paste0("rango_theta1_", g)]] <- matrix(0, n1, n2)
  for (a in seq_len(n2)) {
    S <- signers[[a]]
    for (i in seq_len(n1)) {
      others <- setdiff(S, i)
      if (!length(others)) next
      bi <- bloque[i]
      for (X_ in names(ATRP)) {
        xv <- ATRP[[X_]]
        m <- others[xv[others] == xv[i]]
        if (length(m)) {
          w_in <- sum(bloque[m] == bi)
          cols[[paste0("same_", X_, "_", bi)]][i, a] <- w_in
          cols[[paste0("same_", X_, "_cross")]][i, a] <- length(m) - w_in
        }
      }
      og <- others[bloque[others] == bi]
      r0 <- if (length(og) >= 2) diff(range(theta1[og])) else 0
      cols[[paste0("rango_theta1_", bi)]][i, a] <- diff(range(theta1[c(og, i)])) - r0
    }
  }
  cols
}

fit_e6 <- function(reg) {
  n2 <- nrow(reg)
  signers <- parse_signers(reg)
  miembro <- as.integer(comis_v == reg$commission[1])
  net <- build_net(signers, n2, miembro)
  FORM <- net ~ edges + b1cov("miembro") +
    b2twostar(b1attr = "bloque", b2attr = "bloque") +
    b2covrange("theta1_c") + b2covrange("theta2_c") +
    b2covrange("edad_c") + b2covrange("grado_c") +
    gwb1degree(0.5, fixed = TRUE) + gwdsp(0.5, fixed = TRUE)
  mp <- ergmMPLE(FORM, output = "array")
  pn <- dimnames(mp$predictor)[[3]]
  Y <- as.vector(mp$response)
  X <- matrix(mp$predictor, ncol = length(pn)); colnames(X) <- pn
  MC <- manual_cols(signers, n2)
  XM <- do.call(cbind, lapply(MC, as.vector)); colnames(XM) <- names(MC)
  XX <- cbind(X, XM)
  # celdas sin contribucion observada: su MLE es -Inf. Igual que ergm, se fija
  # el coeficiente en -Inf: fuera la columna Y fuera las dyadas que crearian
  # esos pares (quedan estructuralmente determinadas bajo -Inf).
  obs_contrib <- as.vector(crossprod(XX, Y))
  anyvar <- apply(XX, 2, function(c) any(c != 0))
  whitelist <- colnames(XX) %in% c("edges", grep("covrange|gw|b1cov", colnames(XX), value = TRUE))
  usable <- (obs_contrib > 0 | whitelist) & anyvar
  empty <- which(!usable & anyvar)                 # celdas -Inf (con variacion)
  keep_rows <- if (length(empty)) rowSums(XX[, empty, drop = FALSE] > 0) == 0 else rep(TRUE, length(Y))
  XX2 <- XX[keep_rows, usable, drop = FALSE]
  g <- suppressWarnings(glm.fit(cbind(`edges` = 1, XX2[, colnames(XX2) != "edges", drop = FALSE]),
                                Y[keep_rows], family = binomial(),
                                control = list(maxit = 100)))
  co <- g$coefficients
  names(co)[1] <- "edges"
  attr(co, "dropped") <- colnames(XX)[!usable]
  co
}

fit_e6_hard <- function(reg, timeout = 60) {
  job <- parallel::mcparallel(fit_e6(reg))
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
  nE <- nrow(regk)
  t0 <- Sys.time()
  punto <- fit_e6(regk)
  tp <- as.numeric(difftime(Sys.time(), t0, units = "secs"))
  cat(sprintf("  C%d punto: %d iniciativas, %d params (+%d vacios), %.1f s -> boot estimado %.1f min\n",
              k, nE, length(punto), length(attr(punto, "dropped")), tp, tp * B / 8 / 60))
  t0 <- Sys.time()
  boot <- mclapply(1:B, function(b) {
    set.seed(9000 + k * 1000 + b)
    tryCatch(fit_e6_hard(regk[sample(nE, nE, replace = TRUE), ]),
             error = function(e) NULL)
  }, mc.cores = 8, mc.preschedule = FALSE)
  boot <- boot[!sapply(boot, is.null)]
  bm <- do.call(rbind, lapply(boot, function(v) v[names(punto)]))
  bm[!is.finite(bm)] <- NA
  se_boot <- apply(bm, 2, sd, na.rm = TRUE)
  B_fin <- colSums(is.finite(bm))
  res[[k]] <- rbind(
    data.frame(commission = sprintf("C%d", k), n_inic = nE, term = names(punto),
               estimate = as.numeric(punto), se_boot = se_boot[names(punto)],
               z = as.numeric(punto) / se_boot[names(punto)],
               p_boot = 2 * pnorm(-abs(as.numeric(punto) / se_boot[names(punto)])),
               B_ok = length(boot), B_finite = B_fin[names(punto)],
               empty_cell = 0L, row.names = NULL),
    if (length(attr(punto, "dropped"))) data.frame(
      commission = sprintf("C%d", k), n_inic = nE, term = attr(punto, "dropped"),
      estimate = NA, se_boot = NA, z = NA, p_boot = NA,
      B_ok = length(boot), B_finite = 0L, empty_cell = 1L) else NULL)
  cat(sprintf("  C%d boot: B validos = %d, %.1f min\n", k, length(boot),
              as.numeric(difftime(Sys.time(), t0, units = "mins"))))
}
tab <- do.call(rbind, res)
write.csv(tab, file.path(RESULTS_TABLES, "M1_bipartite_hybrid_boot.csv"), row.names = FALSE)
cat(sprintf("--- Done (%.1f min) ---\n", as.numeric(difftime(Sys.time(), T0, units = "mins"))))

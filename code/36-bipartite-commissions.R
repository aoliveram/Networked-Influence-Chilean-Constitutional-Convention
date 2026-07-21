# =============================================================================
# 36-bipartite-commissions.R  (comentario del autor 2026-07-20, punto 6;
# extendido a las 7 comisiones tras aprobar el piloto C1/C3 y adoptar el
# registro de la plataforma con las 947 iniciativas utilizables)
# ERGM bipartito POR COMISIÓN (redes 154 x n_k). NOTA 2026-07-20: se corrigió
# un bug de construcción (heads sin rep() -> firmas asignadas a documentos
# equivocados); con las redes REALES el MCMLE toma ~2.5 min/iteración incluso
# en C1, así que el modo por defecto es para el run nocturno y el modo
# BIPARTITE_ESTIMATE=MPLE entrega puntos rápidos para el reporte.
# Términos: edges + b1cov(miembro de la comisión) + b1nodematch (conglomerado,
# quintil theta1, abogado, experiencia, mujer). MCMC con 8 cadenas paralelas.
#
# Output: results/tables/M1_bipartite_commissions.csv
# =============================================================================

cat("=== 36-bipartite-commissions.R ===\n")
suppressPackageStartupMessages({ library(jsonlite); library(statnet) })
set.seed(42)
source("code/paths.R")
T0 <- Sys.time()

profiles <- fromJSON(PROFILES)
roster <- sort(fromJSON(MEMBERS))
ip2d <- read.csv(file.path(DATA_PROCESSED, "ideal_points_2d_firstmonth.csv"), stringsAsFactors = FALSE)
listas <- read.csv(file.path(DATA_RAW, "electoral_lists.csv"), stringsAsFactors = FALSE)
memb <- read.csv(file.path(DATA_RAW, "commission_membership.csv"), stringsAsFactors = FALSE)
registry <- read.csv(file.path(DATA_PROCESSED, "initiative_registry.csv"), stringsAsFactors = FALSE)
registry <- registry[registry$n_firmantes >= 2 & registry$n_firmantes <= 16, ]

n1 <- length(roster)
theta1 <- ip2d$theta1_fm[match(roster, ip2d$nombre_armonizado)]
theta1_q <- as.character(cut(theta1, quantile(theta1, probs = seq(0, 1, 0.2)),
                             include.lowest = TRUE, labels = paste0("Q", 1:5)))
congl <- listas$conglomerado[match(roster, listas$nombre_armonizado)]
congl[congl == "REVISAR (lista local sin conglomerado)"] <- "Otras"
comis_v <- memb$commission[match(roster, memb$nombre_armonizado)]

fit_commission <- function(k) {
  regk <- registry[registry$commission == sprintf("C%d", k), ]
  n2 <- nrow(regk)
  cat(sprintf("\n--- C%d: %d iniciativas -> red bipartita %d x %d ---\n", k, n2, n1, n2))
  net <- network::network.initialize(n1 + n2, directed = FALSE, bipartite = n1)
  network::network.vertex.names(net) <- c(roster, paste0("C", k, ":", regk$initiative_id))
  atr <- list(
    conglomerado = c(congl, rep("modo2", n2)),
    theta1_q = c(theta1_q, rep("modo2", n2)),
    es_abogado = c(profiles$es_abogado[match(roster, profiles$nombre_armonizado)], rep(-1L, n2)),
    experiencia = c(profiles$experiencia_previa_institucional[match(roster, profiles$nombre_armonizado)], rep(-1L, n2)),
    es_mujer = c(profiles$es_mujer[match(roster, profiles$nombre_armonizado)], rep(-1L, n2)),
    miembro = c(as.integer(comis_v == sprintf("C%d", k)), rep(0L, n2))
  )
  for (a in names(atr)) network::set.vertex.attribute(net, a, atr[[a]])
  tails <- integer(0); heads <- integer(0)
  for (j in seq_len(n2)) {
    S <- match(strsplit(regk$firmantes[j], "; ", fixed = TRUE)[[1]], roster)
    S <- S[!is.na(S)]
    tails <- c(tails, S); heads <- c(heads, rep(n1 + j, length(S)))
  }
  net <- network::add.edges(net, tail = tails, head = heads)

  t0 <- Sys.time()
  f <- net ~ edges + b1cov("miembro") + b1nodematch("conglomerado") +
    b1nodematch("theta1_q") + b1nodematch("es_abogado") +
    b1nodematch("experiencia") + b1nodematch("es_mujer")
  out <- tryCatch({
    # BIPARTITE_ESTIMATE=MPLE: máxima pseudo-verosimilitud (segundos; puntos
    # consistentes bajo dependencia debil, EEs logísticos ANTI-conservadores).
    # Por defecto, MCMLE completo (horas por comisión en las redes reales).
    fit <- ergm(f, estimate = ifelse(Sys.getenv("BIPARTITE_ESTIMATE") == "MPLE",
                                     "MPLE", "MLE"),
                control = control.ergm(seed = 42, MCMLE.maxit = 60,
                                       MCMC.samplesize = 8192, MCMC.interval = 4096,
                                       parallel = 8, parallel.type = "PSOCK"))
    sm <- summary(fit)
    data.frame(commission = sprintf("C%d", k),
               method = ifelse(Sys.getenv("BIPARTITE_ESTIMATE") == "MPLE", "MPLE", "MCMLE"),
               term = rownames(sm$coefficients),
               estimate = sm$coefficients[, 1], se = sm$coefficients[, 2],
               p = sm$coefficients[, ncol(sm$coefficients)], row.names = NULL)
  }, error = function(e) data.frame(commission = sprintf("C%d", k),
                                    method = ifelse(Sys.getenv("BIPARTITE_ESTIMATE") == "MPLE", "MPLE", "MCMLE"),
                                    term = paste("ERROR:", conditionMessage(e)),
                                    estimate = NA, se = NA, p = NA))
  out$secs <- round(as.numeric(difftime(Sys.time(), t0, units = "secs")), 1)
  print(out, row.names = FALSE, digits = 3)
  out
}

tab <- do.call(rbind, lapply(1:7, fit_commission))
dir.create(RESULTS_TABLES, recursive = TRUE, showWarnings = FALSE)
write.csv(tab, file.path(RESULTS_TABLES, "M1_bipartite_commissions.csv"), row.names = FALSE)
cat(sprintf("\n--- Done (total %.1f min) ---\n", as.numeric(difftime(Sys.time(), T0, units = "mins"))))

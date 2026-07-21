# =============================================================================
# 43-bipartite-mcmc-overnight.R  (comentario del autor 2026-07-20, punto 6.4:
# "¿dejarlo corriendo en la noche?" — sí: este es el run nocturno)
#
# MCMLE COMPLETO sobre las redes bipartitas REALES (post-corrección del bug de
# construcción): (a) la suite por comisión (C1..C7), (b) el pooled 154 x 947.
# En las redes reales cada iteración MCMLE de una comisión toma minutos, por
# eso este script está pensado para correr con nohup durante la noche.
# Guarda INCREMENTALMENTE (un CSV que crece por comisión) para que un corte a
# medias no pierda lo avanzado. Presupuestos generosos:
#   MCMLE.maxit = 60, MCMC.samplesize = 8192, MCMC.interval = 4096, 8 cadenas.
# Validación esperada: los puntos MCMLE deben quedar cerca de los MPLE del
# reporte (Tabla 8); los EE del MCMLE son los válidos.
#
# Output: results/tables/M1_bipartite_mcmc_overnight.csv (incremental)
#         data/processed/bipartite_mcmc_C{k}.rds / _pooled947.rds
# =============================================================================

cat("=== 43-bipartite-mcmc-overnight.R ===\n")
suppressPackageStartupMessages({ library(jsonlite); library(statnet) })
set.seed(42)
source("code/paths.R")
T00 <- Sys.time()
OUT <- file.path(RESULTS_TABLES, "M1_bipartite_mcmc_overnight.csv")

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

build_net <- function(regk, k = NULL) {
  n2 <- nrow(regk)
  net <- network::network.initialize(n1 + n2, directed = FALSE, bipartite = n1)
  miembro <- if (!is.null(k)) as.integer(comis_v == sprintf("C%d", k)) else rep(0L, n1)
  atr <- list(conglomerado = c(congl, rep("modo2", n2)),
              theta1_q = c(theta1_q, rep("modo2", n2)),
              es_abogado = c(profiles$es_abogado[match(roster, profiles$nombre_armonizado)], rep(-1L, n2)),
              experiencia = c(profiles$experiencia_previa_institucional[match(roster, profiles$nombre_armonizado)], rep(-1L, n2)),
              es_mujer = c(profiles$es_mujer[match(roster, profiles$nombre_armonizado)], rep(-1L, n2)),
              miembro = c(miembro, rep(0L, n2)))
  for (a in names(atr)) network::set.vertex.attribute(net, a, atr[[a]])
  tails <- integer(0); heads <- integer(0)
  for (j in seq_len(n2)) {
    S <- match(strsplit(regk$firmantes[j], "; ", fixed = TRUE)[[1]], roster)
    S <- S[!is.na(S)]
    tails <- c(tails, S); heads <- c(heads, rep(n1 + j, length(S)))
  }
  network::add.edges(net, tail = tails, head = heads)
}

CTRL <- control.ergm(seed = 42, MCMLE.maxit = 60, MCMC.samplesize = 8192,
                     MCMC.interval = 4096, parallel = 8, parallel.type = "PSOCK")

append_out <- function(df) {
  write.table(df, OUT, sep = ",", row.names = FALSE, append = file.exists(OUT),
              col.names = !file.exists(OUT))
}

fit_and_save <- function(net, f, label, rds) {
  t0 <- Sys.time()
  out <- tryCatch({
    fit <- ergm(f, control = CTRL)
    saveRDS(fit, rds)
    sm <- summary(fit)
    data.frame(config = label, term = rownames(sm$coefficients),
               estimate = sm$coefficients[, 1], se = sm$coefficients[, 2],
               p = sm$coefficients[, ncol(sm$coefficients)],
               mins = round(as.numeric(difftime(Sys.time(), t0, units = "mins")), 1),
               row.names = NULL)
  }, error = function(e) data.frame(config = label,
                                    term = paste("ERROR:", conditionMessage(e)),
                                    estimate = NA, se = NA, p = NA,
                                    mins = round(as.numeric(difftime(Sys.time(), t0, units = "mins")), 1)))
  append_out(out)
  cat(sprintf("  [%s] %s: %.1f min\n", format(Sys.time(), "%H:%M"), label, out$mins[1]))
  out
}

for (k in 1:7) {
  regk <- registry[registry$commission == sprintf("C%d", k), ]
  net <- build_net(regk, k)
  fit_and_save(net,
               net ~ edges + b1cov("miembro") + b1nodematch("conglomerado") +
                 b1nodematch("theta1_q") + b1nodematch("es_abogado") +
                 b1nodematch("experiencia") + b1nodematch("es_mujer"),
               sprintf("C%d (MCMLE)", k),
               file.path(DATA_PROCESSED, sprintf("bipartite_mcmc_C%d.rds", k)))
}

# pooled 947 (para el registro; interpretación con cautela: intercepto único ->
# la composición entre comisiones confunde la homofilia, Simpson documentado)
membmat <- sapply(registry$commission, function(cc)
  if (nzchar(cc)) as.integer(comis_v == cc) else rep(0L, n1))
netp <- build_net(registry)
fit_and_save(netp,
             netp ~ edges + edgecov(membmat) + b1nodematch("conglomerado") +
               b1nodematch("theta1_q") + b1nodematch("es_abogado") +
               b1nodematch("experiencia") + b1nodematch("es_mujer"),
             "pooled 947 (MCMLE)",
             file.path(DATA_PROCESSED, "bipartite_mcmc_pooled947.rds"))

cat(sprintf("--- Done (total %.1f h) ---\n", as.numeric(difftime(Sys.time(), T00, units = "hours"))))

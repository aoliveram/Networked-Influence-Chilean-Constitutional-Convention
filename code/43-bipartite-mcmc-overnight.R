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
distr_v <- profiles$distrito[match(roster, profiles$nombre_armonizado)]
grado_v <- profiles$grado_academico_nivel[match(roster, profiles$nombre_armonizado)]
edad_v <- profiles$edad_al_asumir[match(roster, profiles$nombre_armonizado)]
edad_q <- as.character(cut(edad_v, quantile(edad_v, probs = seq(0, 1, 0.2)),
                           include.lowest = TRUE, labels = paste0("E", 1:5)))

build_net <- function(regk, k = NULL) {
  n2 <- nrow(regk)
  net <- network::network.initialize(n1 + n2, directed = FALSE, bipartite = n1)
  miembro <- if (!is.null(k)) as.integer(comis_v == sprintf("C%d", k)) else rep(0L, n1)
  atr <- list(conglomerado = c(congl, rep("modo2", n2)),
              theta1_q = c(theta1_q, rep("modo2", n2)),
              es_abogado = c(profiles$es_abogado[match(roster, profiles$nombre_armonizado)], rep(-1L, n2)),
              experiencia = c(profiles$experiencia_previa_institucional[match(roster, profiles$nombre_armonizado)], rep(-1L, n2)),
              es_mujer = c(profiles$es_mujer[match(roster, profiles$nombre_armonizado)], rep(-1L, n2)),
              distrito = c(distr_v, rep("modo2", n2)),
              grado = c(as.character(grado_v), rep("modo2", n2)),
              edad_q = c(edad_q, rep("modo2", n2)),
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

# Presupuesto RECALIBRADO (2026-07-21): el run del 20-jul con samplesize 8192 x
# interval 4096 (32x el default) tomaba ~1-1.5 h POR ITERACIÓN MCMLE -> orden
# 100 h. Con el default (1024 x 1024) una iteración en la C1 real toma ~2.5
# min -> ~1 h por comisión, la suite + pooled en una noche. Cores por env
# BIPARTITE_CORES (default 8; usar 3-4 si la máquina está en uso).
NCORES <- as.integer(Sys.getenv("BIPARTITE_CORES", "8"))
CTRL <- control.ergm(seed = 42, MCMLE.maxit = 60,
                     parallel = NCORES, parallel.type = "PSOCK")

append_out <- function(df) {
  write.table(df, OUT, sep = ",", row.names = FALSE, append = file.exists(OUT),
              col.names = !file.exists(OUT))
}

done_configs <- if (file.exists(OUT)) unique(read.csv(OUT)$config) else character(0)

fit_and_save <- function(net, f, label, rds) {
  if (label %in% done_configs) {
    cat(sprintf("  [skip] %s ya completado en %s\n", label, basename(OUT)))
    return(invisible(NULL))
  }
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
                 b1nodematch("theta1_q") + b1nodematch("distrito") +
                 b1nodematch("es_abogado") + b1nodematch("experiencia") +
                 b1nodematch("es_mujer") + b1nodematch("grado") + b1nodematch("edad_q"),
               sprintf("C%d (MCMLE)", k),
               file.path(DATA_PROCESSED, sprintf("bipartite_mcmc_C%d.rds", k)))
}

# test estructural (punto 4b, 2026-07-21): C3 (la más chica) con la espec
# extendida + gwb1degree(0.5, fixed) — el término estructural más simple y
# menos degenerable. Medido de día: ~15 min/iteración con 8 cadenas.
regk3 <- registry[registry$commission == "C3", ]
net3 <- build_net(regk3, 3)
fit_and_save(net3,
             net3 ~ edges + b1cov("miembro") + b1nodematch("conglomerado") +
               b1nodematch("theta1_q") + b1nodematch("distrito") +
               b1nodematch("es_abogado") + b1nodematch("experiencia") +
               b1nodematch("es_mujer") + b1nodematch("grado") + b1nodematch("edad_q") +
               gwb1degree(0.5, fixed = TRUE),
             "C3 + gwb1degree (estructural)",
             file.path(DATA_PROCESSED, "bipartite_mcmc_C3_gwb1.rds"))

# pooled 947 (para el registro; interpretación con cautela: intercepto único ->
# la composición entre comisiones confunde la homofilia, Simpson documentado)
membmat <- sapply(registry$commission, function(cc)
  if (nzchar(cc)) as.integer(comis_v == cc) else rep(0L, n1))
netp <- build_net(registry)
fit_and_save(netp,
             netp ~ edges + edgecov(membmat) + b1nodematch("conglomerado") +
               b1nodematch("theta1_q") + b1nodematch("distrito") +
               b1nodematch("es_abogado") + b1nodematch("experiencia") +
               b1nodematch("es_mujer") + b1nodematch("grado") + b1nodematch("edad_q"),
             "pooled 947 (MCMLE)",
             file.path(DATA_PROCESSED, "bipartite_mcmc_pooled947.rds"))

cat(sprintf("--- Done (total %.1f h) ---\n", as.numeric(difftime(Sys.time(), T00, units = "hours"))))

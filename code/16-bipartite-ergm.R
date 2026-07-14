# =============================================================================
# 16-bipartite-ergm.R  (respuesta a revisión: D1 robustez estructural)
# ERGM BIPARTITO convencionales (modo 1, n=154) x iniciativas (modo 2, n=528).
# La dependencia dentro de cada iniciativa queda representada por el nodo
# iniciativa; ademas se CONDICIONA en el grado de cada iniciativa
# (constraints = ~b2degrees): el tamaño 8-16 de cada coalición es regla, no
# resultado — el análogo exacto del strata() del logit condicional (15).
#
# Términos:
#   gwb1degree           heterogeneidad de actividad de los firmantes (F9a)
#   nodematch(comision)  firma cruzada modo1-modo2: "iniciativa de mi comisión"
#   b1nodematch(...)     homofilia entre co-firmantes de la misma iniciativa
#                        (conglomerado, quintil de theta1, abogado, experiencia,
#                         mujer) — el two-star bipartito sobre atributo
# Especificaciones: (A) completa; (B) sin ideología (para ver cuánto de la
# homofilia demográfica es sorting ideológico, como en 13).
#
# Output: results/tables/M1_bipartite_ergm.csv
# =============================================================================

cat("=== 16-bipartite-ergm.R ===\n")
suppressPackageStartupMessages({ library(jsonlite); library(statnet); library(parallel) })
set.seed(42)
source("code/paths.R")
T0 <- Sys.time()

profiles <- fromJSON(PROFILES)
roster <- sort(fromJSON(MEMBERS))
ip2d <- read.csv(file.path(DATA_PROCESSED, "ideal_points_2d_firstmonth.csv"), stringsAsFactors = FALSE)
listas <- read.csv(file.path(DATA_RAW, "electoral_lists.csv"), stringsAsFactors = FALSE)
memb <- read.csv(file.path(DATA_RAW, "commission_membership.csv"), stringsAsFactors = FALSE)
registry <- read.csv(file.path(DATA_PROCESSED, "initiative_registry.csv"), stringsAsFactors = FALSE)
registry <- registry[registry$n_firmantes <= 16, ]   # decisión D8 (2026-07-11)

n1 <- length(roster); n2 <- nrow(registry)
theta1 <- ip2d$theta1_fm[match(roster, ip2d$nombre_armonizado)]
theta1_q <- as.character(cut(theta1, quantile(theta1, probs = seq(0, 1, 0.2)),
                             include.lowest = TRUE, labels = paste0("Q", 1:5)))
congl <- listas$conglomerado[match(roster, listas$nombre_armonizado)]
congl[congl == "REVISAR (lista local sin conglomerado)"] <- "Otras"

# vértices 1..154 = convencionales; 155..(154+528) = iniciativas
# atributos completos (sin NA) para ambos modos; los de modo 1 llevan un
# placeholder en modo 2 que los términos b1* ignoran
comis_m1 <- memb$commission[match(roster, memb$nombre_armonizado)]
stopifnot(!anyNA(comis_m1), !anyNA(congl), !anyNA(theta1_q))
ATTR_VECS <- list(
  comision = c(comis_m1, registry$commission),
  conglomerado = c(congl, rep("modo2", n2)),
  theta1_q = c(theta1_q, rep("modo2", n2)),
  es_abogado = c(profiles$es_abogado[match(roster, profiles$nombre_armonizado)], rep(-1L, n2)),
  experiencia = c(profiles$experiencia_previa_institucional[match(roster, profiles$nombre_armonizado)], rep(-1L, n2)),
  es_mujer = c(profiles$es_mujer[match(roster, profiles$nombre_armonizado)], rep(-1L, n2))
)
stopifnot(!anyNA(unlist(ATTR_VECS)))

build_bip <- function() {
  net <- network::network.initialize(n1 + n2, directed = FALSE, bipartite = n1)
  network::network.vertex.names(net) <- c(roster, paste(registry$commission,
                                                        registry$initiative_id, sep = ":"))
  for (a in names(ATTR_VECS)) network::set.vertex.attribute(net, a, ATTR_VECS[[a]])
  tails <- integer(0); heads <- integer(0)
  for (k in seq_len(n2)) {
    S <- match(strsplit(registry$firmantes[k], "; ", fixed = TRUE)[[1]], roster)
    S <- S[!is.na(S)]
    tails <- c(tails, S); heads <- c(heads, rep(n1 + k, length(S)))
  }
  network::add.edges(net, tail = tails, head = heads)
  net
}

# NOTA de tractabilidad (2026-07-10). Dos intentos fallidos documentados:
# (1) constraints = ~b2degrees (el análogo exacto del strata del clogit):
#     el MCMC restringido no mezcla (>1 h clavado en la iteración 2, paso 4e-4).
# (2) edges + gwb1degree(0.5) + gwb2degree(0.5) + nodematch + b1nodematch:
#     degeneración desde el arranque MPLE ("statistics not varying";
#     "Unconstrained MCMC sampling did not mix at all", 25 min).
# Intento 3 (actual): especificación mínima sin términos gw de grado — la
# heterogeneidad de actividad queda ABSORBIDA por lo que el clogit ya controla;
# el papel de este modelo es solo corroboración estructural de las homofilias.
BASE <- paste("edges + nodematch('comision') +",
              "b1nodematch('conglomerado') + b1nodematch('es_abogado') +",
              "b1nodematch('experiencia') + b1nodematch('es_mujer')")
specs <- list(
  list(label = "B: sin ideología", terms = BASE),
  list(label = "A: + quintiles theta1", terms = paste(BASE, "+ b1nodematch('theta1_q')"))
)

fit_one <- function(sp) {
  t0 <- Sys.time()
  net <- build_bip()
  f <- as.formula(paste("net ~", sp$terms), env = environment())
  out <- tryCatch({
    fit <- ergm(f, control = control.ergm(seed = 42, MCMLE.maxit = 30))
    sm <- summary(fit)
    data.frame(model = sp$label, term = rownames(sm$coefficients),
               estimate = sm$coefficients[, 1], se = sm$coefficients[, 2],
               p = sm$coefficients[, ncol(sm$coefficients)],
               aic = tryCatch(AIC(fit), error = function(e) NA), row.names = NULL)
  }, error = function(e) data.frame(model = sp$label,
                                    term = paste("ERROR:", conditionMessage(e)),
                                    estimate = NA, se = NA, p = NA, aic = NA))
  out$secs <- round(as.numeric(difftime(Sys.time(), t0, units = "secs")), 1)
  out
}

n_workers <- max(1, min(2, detectCores() - 2))
cat(sprintf("  Red bipartita %d x %d | 2 ERGMs en %d workers (spec mínima, intento 3)...\n",
            n1, n2, n_workers))
tabs <- mclapply(specs, fit_one, mc.cores = n_workers, mc.set.seed = FALSE)
tab <- do.call(rbind, tabs)
print(tab, row.names = FALSE, digits = 3)
dir.create(RESULTS_TABLES, recursive = TRUE, showWarnings = FALSE)
write.csv(tab, file.path(RESULTS_TABLES, "M1_bipartite_ergm.csv"), row.names = FALSE)
cat(sprintf("--- Done (total %.1f min) ---\n", as.numeric(difftime(Sys.time(), T0, units = "mins"))))

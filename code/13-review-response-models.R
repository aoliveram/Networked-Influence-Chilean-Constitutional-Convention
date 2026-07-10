# =============================================================================
# 13-review-response-models.R  (respuesta a revisión: D3 y D9)
#
# (a) D3 — ERGM valuado actual CON vs SIN distancia ideológica 2D del primer
#     mes (absdiff theta1_fm, theta2_fm). Se mantiene la especificación actual
#     a sabiendas de su problema inferencial (D1): el objetivo es ver cuánto
#     cambian las homofilias demográficas al condicionar en ideología.
#     Ambos fits en paralelo (mclapply, P-cores), semilla fija.
#
# (b) D9 — H1b como Burt la formuló: regresión de brokerage
#     (constraint de Burt y betweenness) sobre abogado/experiencia + controles.
#
# Outputs: results/tables/M1_ergm_ideologia.csv, results/tables/M1_brokerage.csv
# =============================================================================

cat("=== 13-review-response-models.R ===\n")
suppressPackageStartupMessages({
  library(jsonlite); library(statnet); library(parallel)
  library(igraph); library(lmtest); library(sandwich)
})
set.seed(42)
source("code/paths.R")
T0 <- Sys.time()

profiles <- fromJSON(PROFILES)
roster <- sort(fromJSON(MEMBERS))
ip2d <- read.csv(file.path(DATA_PROCESSED, "ideal_points_2d_firstmonth.csv"), stringsAsFactors = FALSE)
edges_df <- read.csv(file.path(DATA_PROCESSED, "genesis_network_initiative.csv"), stringsAsFactors = FALSE)
metrics <- read.csv(file.path(DATA_PROCESSED, "network_metrics.csv"), stringsAsFactors = FALSE)
listas <- read.csv(file.path(DATA_RAW, "electoral_lists.csv"), stringsAsFactors = FALSE)

# ------------------------- (b) D9: brokerage primero (rápido) ---------------
cat("\n--- D9: brokerage (constraint de Burt, betweenness) ---\n")
g <- graph_from_data_frame(edges_df, directed = FALSE, vertices = data.frame(name = roster))
E(g)$weight <- edges_df$weight
cons <- constraint(g, weights = E(g)$weight)

d9 <- data.frame(nombre_armonizado = names(cons), constraint = as.numeric(cons),
                 stringsAsFactors = FALSE)
d9 <- merge(d9, metrics[, c("nombre_armonizado", "betweenness", "degree")], by = "nombre_armonizado")
d9 <- merge(d9, profiles[, c("nombre_armonizado", "es_abogado", "experiencia_previa_institucional",
                             "es_mujer", "edad_al_asumir", "grado_academico_nivel")],
            by = "nombre_armonizado")
d9 <- merge(d9, ip2d[, c("nombre_armonizado", "theta1_fm", "theta2_fm")], by = "nombre_armonizado")
d9 <- merge(d9, listas[, c("nombre_armonizado", "conglomerado")], by = "nombre_armonizado")
d9$abs_theta1 <- abs(d9$theta1_fm)
d9$log_betw <- log1p(d9$betweenness)
cat(sprintf("  N = %d | constraint: media %.3f (menor = más brokerage)\n", nrow(d9), mean(d9$constraint)))

f_terms <- "es_abogado + experiencia_previa_institucional + es_mujer + edad_al_asumir + grado_academico_nivel + abs_theta1 + factor(conglomerado)"
tidy_lm <- function(f, dv_label) {
  m <- lm(as.formula(f), data = d9)
  ct <- coeftest(m, vcov. = vcovHC(m, type = "HC1"))
  keep <- grep("abogado|experiencia|mujer|edad|grado|abs_theta1", rownames(ct))
  data.frame(dv = dv_label, term = rownames(ct)[keep],
             estimate = ct[keep, 1], se = ct[keep, 2], p = ct[keep, 4],
             r2 = summary(m)$r.squared, row.names = NULL)
}
tab_b <- rbind(tidy_lm(paste("constraint ~", f_terms), "constraint (menor = broker)"),
               tidy_lm(paste("log_betw ~", f_terms), "log(1+betweenness)"))
print(tab_b, row.names = FALSE, digits = 3)
dir.create(RESULTS_TABLES, recursive = TRUE, showWarnings = FALSE)
write.csv(tab_b, file.path(RESULTS_TABLES, "M1_brokerage.csv"), row.names = FALSE)

# ------------------------- (a) D3: ERGM con/sin ideología -------------------
cat("\n--- D3: ERGM con vs sin absdiff(theta 2D primer mes) ---\n")
ATTRS <- c("afiliacion_agrupada", "es_abogado", "es_mujer", "edad_al_asumir",
           "experiencia_previa_institucional", "grado_academico_nivel")
build_net <- function() {
  # namespace explícito: igraph (cargado para D9) enmascara add.edges/set.*
  net <- network::network.initialize(length(roster), directed = FALSE)
  network::network.vertex.names(net) <- roster
  for (col in ATTRS) {
    network::set.vertex.attribute(net, col, profiles[[col]][match(roster, profiles$nombre_armonizado)])
  }
  network::set.vertex.attribute(net, "theta1_fm", ip2d$theta1_fm[match(roster, ip2d$nombre_armonizado)])
  network::set.vertex.attribute(net, "theta2_fm", ip2d$theta2_fm[match(roster, ip2d$nombre_armonizado)])
  network::add.edges(net, tail = match(edges_df$source, roster), head = match(edges_df$target, roster))
  network::set.edge.attribute(net, "weight", edges_df$weight)
  net
}
BASE <- paste("sum + nodematch('afiliacion_agrupada') +",
              "nodematch('experiencia_previa_institucional') + nodematch('es_abogado') +",
              "nodematch('es_mujer') + absdiff('edad_al_asumir') +",
              "absdiff('grado_academico_nivel') + nodecov('edad_al_asumir')")
specs <- list(
  list(label = "Base (sin ideología)", terms = BASE),
  list(label = "Base + absdiff(theta1_fm) + absdiff(theta2_fm)",
       terms = paste(BASE, "+ absdiff('theta1_fm') + absdiff('theta2_fm')"))
)
fit_one <- function(sp) {
  t0 <- Sys.time()
  net <- build_net()
  f <- as.formula(paste("net ~", sp$terms), env = environment())
  out <- tryCatch({
    fit <- ergm(f, response = "weight", reference = ~Poisson,
                control = control.ergm(seed = 42))
    sm <- summary(fit)
    data.frame(model = sp$label, term = rownames(sm$coefficients),
               estimate = sm$coefficients[, 1], se = sm$coefficients[, 2],
               p = sm$coefficients[, ncol(sm$coefficients)], row.names = NULL)
  }, error = function(e) data.frame(model = sp$label, term = paste("ERROR:", conditionMessage(e)),
                                    estimate = NA, se = NA, p = NA))
  out$secs <- round(as.numeric(difftime(Sys.time(), t0, units = "secs")), 1)
  out
}
n_workers <- max(1, min(2, detectCores() - 2))
cat(sprintf("  Lanzando 2 ERGMs en %d workers...\n", n_workers))
tabs <- mclapply(specs, fit_one, mc.cores = n_workers, mc.set.seed = FALSE)
tab_a <- do.call(rbind, tabs)
print(tab_a, row.names = FALSE, digits = 3)
write.csv(tab_a, file.path(RESULTS_TABLES, "M1_ergm_ideologia.csv"), row.names = FALSE)

cat(sprintf("--- Done (total %.1f min) ---\n", as.numeric(difftime(Sys.time(), T0, units = "mins"))))

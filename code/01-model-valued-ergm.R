# =============================================================================
# 01-model-valued-ergm.R  (v2, actualización 7 comisiones)
# Modelo 1: formación de la red de co-patrocinio (Valued ERGM, referencia Poisson)
#
# Red: génesis pooled con unidad INICIATIVA (genesis_network_initiative.csv).
# Término nuevo (decisión 2026-07-07): absdiff(grado_academico_nivel) en escala
# 0-3 — distancia educativa. Covariables desde los perfiles curados (P5).
# =============================================================================

if (!requireNamespace("jsonlite", quietly = TRUE)) install.packages("jsonlite", repos = "http://cran.us.r-project.org")
if (!requireNamespace("statnet", quietly = TRUE)) install.packages("statnet", repos = "http://cran.us.r-project.org")

library(jsonlite)
library(statnet)

set.seed(42)
source("code/paths.R")

cat("--- Cargando red génesis (iniciativa) y perfiles curados ---\n")
edges_df <- read.csv(file.path(DATA_PROCESSED, "genesis_network_initiative.csv"),
                     stringsAsFactors = FALSE)
profiles <- fromJSON(PROFILES)
roster <- fromJSON(MEMBERS)
cat(sprintf("  Aristas: %d | peso total: %d | perfiles: %d | roster: %d\n",
            nrow(edges_df), sum(edges_df$weight), nrow(profiles), length(roster)))

# nodos = roster canónico completo (154), en orden alfabético
all_nodes <- sort(roster)
n_nodes <- length(all_nodes)
stopifnot(all(edges_df$source %in% all_nodes), all(edges_df$target %in% all_nodes))

get_attr <- function(name, col) {
  idx <- match(name, profiles$nombre_armonizado)
  profiles[[col]][idx]
}

net <- network.initialize(n_nodes, directed = FALSE)
network.vertex.names(net) <- all_nodes
for (col in c("afiliacion_agrupada", "es_abogado", "es_mujer",
              "edad_al_asumir", "experiencia_previa_institucional",
              "grado_academico_nivel")) {
  set.vertex.attribute(net, col, sapply(all_nodes, get_attr, col = col))
}
add.edges(net, tail = match(edges_df$source, all_nodes),
          head = match(edges_df$target, all_nodes))
set.edge.attribute(net, "weight", edges_df$weight)
cat(sprintf("  Red: %d nodos, %d aristas (aislados: %d)\n",
            network.size(net), network.edgecount(net),
            sum(!(all_nodes %in% unique(c(edges_df$source, edges_df$target))))))

cat("\n--- Valued ERGM (Poisson) ---\n")
fit_valued <- ergm(net ~ sum +
                     nodematch("afiliacion_agrupada") +
                     nodematch("experiencia_previa_institucional") +
                     nodematch("es_abogado") +
                     nodematch("es_mujer") +
                     absdiff("edad_al_asumir") +
                     absdiff("grado_academico_nivel") +
                     nodecov("edad_al_asumir"),
                   response = "weight",
                   reference = ~Poisson,
                   control = control.ergm(seed = 42))
print(summary(fit_valued))
saveRDS(fit_valued, file.path(DATA_PROCESSED, "ergm_pooled_results.rds"))

# tabla de coeficientes exportada
dir.create(RESULTS_TABLES, recursive = TRUE, showWarnings = FALSE)
sm <- summary(fit_valued)
coef_df <- data.frame(term = rownames(sm$coefficients),
                      estimate = sm$coefficients[, 1],
                      std_error = sm$coefficients[, 2],
                      p_value = sm$coefficients[, ncol(sm$coefficients)],
                      row.names = NULL)
write.csv(coef_df, file.path(RESULTS_TABLES, "M1_ergm_iniciativa.csv"), row.names = FALSE)
cat(sprintf("  Tabla: %s\n", file.path(RESULTS_TABLES, "M1_ergm_iniciativa.csv")))

# métricas estructurales sobre la red de iniciativas
cat("\n--- Métricas estructurales ---\n")
deg <- degree(net, gmode = "graph")
w_deg <- sapply(seq_len(n_nodes), function(i) {
  eids <- get.edgeIDs(net, i)
  if (length(eids) == 0) return(0)
  sum(get.edge.attribute(net, "weight")[eids])
})
bet <- betweenness(net, gmode = "graph")
eig <- evcent(net, gmode = "graph")

metrics_df <- data.frame(
  nombre_armonizado = all_nodes,
  degree = deg, weighted_degree = w_deg, betweenness = bet, eigenvector = eig,
  stringsAsFactors = FALSE
)
for (col in c("afiliacion_agrupada", "es_abogado", "es_mujer",
              "edad_al_asumir", "experiencia_previa_institucional",
              "grado_academico_nivel")) {
  metrics_df[[col]] <- sapply(all_nodes, get_attr, col = col)
}
write.csv(metrics_df, file.path(DATA_PROCESSED, "network_metrics.csv"), row.names = FALSE)
cat(sprintf("  network_metrics.csv: %d convencionales\n", nrow(metrics_df)))
cat("\n--- Done ---\n")

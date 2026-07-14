# =============================================================================
# 22-network-metrics.R
# Regenera data/processed/network_metrics.csv desde la red génesis-iniciativa
# vigente (reemplaza la parte de métricas del antiguo 01, archivado en
# old-version/scripts/). Mismas columnas y convenciones que el original:
# degree = nº de vecinos distintos; weighted_degree = fuerza; betweenness y
# eigenvector sobre la red BINARIA (como sna::betweenness/evcent del 01).
# =============================================================================

cat("=== 22-network-metrics.R ===\n")
suppressPackageStartupMessages({ library(jsonlite); library(igraph) })
source("code/paths.R")

profiles <- fromJSON(PROFILES)
roster <- sort(fromJSON(MEMBERS))
edges <- read.csv(file.path(DATA_PROCESSED, "genesis_network_initiative.csv"), stringsAsFactors = FALSE)

g <- graph_from_data_frame(edges, directed = FALSE, vertices = data.frame(name = roster))
E(g)$weight <- edges$weight

metrics_df <- data.frame(
  nombre_armonizado = V(g)$name,
  degree = degree(g),
  weighted_degree = strength(g),
  betweenness = betweenness(g, weights = NA),
  eigenvector = eigen_centrality(g, weights = NA)$vector,
  stringsAsFactors = FALSE
)
for (col in c("afiliacion_agrupada", "es_abogado", "es_mujer",
              "edad_al_asumir", "experiencia_previa_institucional",
              "grado_academico_nivel")) {
  metrics_df[[col]] <- profiles[[col]][match(metrics_df$nombre_armonizado,
                                             profiles$nombre_armonizado)]
}
write.csv(metrics_df, file.path(DATA_PROCESSED, "network_metrics.csv"), row.names = FALSE)
cat(sprintf("  network_metrics.csv: %d convencionales | grado medio %.1f | peso medio %.1f\n",
            nrow(metrics_df), mean(metrics_df$degree), mean(metrics_df$weighted_degree)))
cat("--- Done ---\n")

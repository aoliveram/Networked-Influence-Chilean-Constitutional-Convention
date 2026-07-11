# =============================================================================
# 20-reserved-seats.R  (respuesta a revisión: Q5.1 / D12 — ¿enclave o puente?)
# Los 17 escaños reservados PPOO en la red génesis-iniciativa ponderada:
#   (a) Índice E-I de Krackhardt (grupo e individual):
#       EI = (E - I) / (E + I), E = peso hacia no-PPOO, I = peso hacia PPOO;
#       -1 = enclave puro, +1 = integración pura. Benchmark: 10.000
#       permutaciones de la etiqueta PPOO (mismo n = 17) sobre la misma red.
#   (b) Constraint de Burt: PPOO vs. resto (Wilcoxon), misma maquinaria de 13.
# (Las piezas Q5.2-3 — interacciones PPOO x d_theta en el logit condicional —
#  viven en 15-conditional-logit.R.)
#
# Output: results/tables/Q5_reserved_seats.csv
# =============================================================================

cat("=== 20-reserved-seats.R ===\n")
suppressPackageStartupMessages({ library(jsonlite); library(igraph) })
set.seed(42)
source("code/paths.R")

roster <- sort(fromJSON(MEMBERS))
listas <- read.csv(file.path(DATA_RAW, "electoral_lists.csv"), stringsAsFactors = FALSE)
edges <- read.csv(file.path(DATA_PROCESSED, "genesis_network_initiative.csv"), stringsAsFactors = FALSE)

ppoo <- as.integer(listas$conglomerado[match(roster, listas$nombre_armonizado)] ==
                     "Escaños Reservados PPOO")
g <- graph_from_data_frame(edges, directed = FALSE, vertices = data.frame(name = roster))
E(g)$weight <- edges$weight

ei_group <- function(labels) {
  el <- as_edgelist(g, names = FALSE)
  w <- E(g)$weight
  touch <- labels[el[, 1]] == 1 | labels[el[, 2]] == 1     # lazos con >=1 PPOO
  ext <- sum(w[touch & (labels[el[, 1]] != labels[el[, 2]])])
  int <- sum(w[touch & labels[el[, 1]] == 1 & labels[el[, 2]] == 1])
  (ext - int) / (ext + int)
}

ei_obs <- ei_group(ppoo)
N_PERM <- 10000
ei_perm <- replicate(N_PERM, ei_group(sample(ppoo)))
p_lo <- mean(ei_perm <= ei_obs)     # cola enclave (EI menor que azar)
cat(sprintf("  E-I grupal PPOO = %.3f | permutación: media %.3f [p5 %.3f, p95 %.3f]\n",
            ei_obs, mean(ei_perm), quantile(ei_perm, 0.05), quantile(ei_perm, 0.95)))
cat(sprintf("  p(EI_perm <= EI_obs) = %.4f  (valores bajos = enclave más que azar)\n", p_lo))

# E-I individual de cada PPOO
strengths <- strength(g)
ei_node <- sapply(which(ppoo == 1), function(v) {
  nb <- incident(g, v)
  ends_m <- ends(g, nb, names = FALSE)
  other <- ifelse(ends_m[, 1] == v, ends_m[, 2], ends_m[, 1])
  Ei <- sum(nb$weight[ppoo[other] == 0]); Ii <- sum(nb$weight[ppoo[other] == 1])
  (Ei - Ii) / (Ei + Ii)
})
cat(sprintf("  E-I individual: mediana %.3f, rango [%.3f, %.3f] (17 PPOO)\n",
            median(ei_node), min(ei_node), max(ei_node)))

# Constraint de Burt: PPOO vs resto
cons <- constraint(g, weights = E(g)$weight)
wt <- wilcox.test(cons[ppoo == 1], cons[ppoo == 0])
cat(sprintf("  Constraint: mediana PPOO %.3f vs resto %.3f | Wilcoxon p = %.3g\n",
            median(cons[ppoo == 1]), median(cons[ppoo == 0]), wt$p.value))

tab <- data.frame(
  metric = c("EI_grupal", "EI_perm_media", "EI_perm_p5", "EI_perm_p95", "p_enclave",
             "EI_nodo_mediana", "constraint_ppoo_mediana", "constraint_resto_mediana",
             "wilcoxon_p"),
  value = c(ei_obs, mean(ei_perm), quantile(ei_perm, 0.05), quantile(ei_perm, 0.95),
            p_lo, median(ei_node), median(cons[ppoo == 1]), median(cons[ppoo == 0]),
            wt$p.value)
)
dir.create(RESULTS_TABLES, recursive = TRUE, showWarnings = FALSE)
write.csv(tab, file.path(RESULTS_TABLES, "Q5_reserved_seats.csv"), row.names = FALSE)
cat("--- Done ---\n")

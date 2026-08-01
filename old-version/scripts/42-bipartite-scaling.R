# =============================================================================
# 42-bipartite-scaling.R  (comentario del autor 2026-07-20, puntos 6.2/6.3)
# ¿Cómo escala el tiempo del ERGM bipartito con el número de iniciativas?
# Escalera de redes ACUMULADAS (uniones de comisiones, tamaño ascendente),
# terminando en la red completa 154 x 827 (asignadas) y 154 x 947 (todas las
# utilizables, incluidas las 120 sin comisión). Espec = la de la suite x7,
# con la membresía como edgecov (154 x n_docs: "i es miembro de la comisión
# del documento j"; 0 para documentos sin comisión):
#   edges + edgecov(memb) + b1nodematch(conglomerado, theta1_q, es_abogado,
#                                       experiencia, es_mujer)
# Al final: (a) regla de escalado log-log, (b) intento ergm.multi — las 7
# redes ajustadas CONJUNTAMENTE con coeficientes comunes (la respuesta
# estadísticamente correcta a "un solo número por término").
#
# Output: results/tables/M1_bipartite_scaling.csv (+ consola)
# =============================================================================

cat("=== 42-bipartite-scaling.R ===\n")
suppressPackageStartupMessages({ library(jsonlite); library(statnet) })
set.seed(42)
source("code/paths.R")
T00 <- Sys.time()

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
abog <- profiles$es_abogado[match(roster, profiles$nombre_armonizado)]
exper <- profiles$experiencia_previa_institucional[match(roster, profiles$nombre_armonizado)]
mujer <- profiles$es_mujer[match(roster, profiles$nombre_armonizado)]

build_net <- function(regk) {
  n2 <- nrow(regk)
  net <- network::network.initialize(n1 + n2, directed = FALSE, bipartite = n1)
  atr <- list(conglomerado = c(congl, rep("modo2", n2)),
              theta1_q = c(theta1_q, rep("modo2", n2)),
              es_abogado = c(abog, rep(-1L, n2)),
              experiencia = c(exper, rep(-1L, n2)),
              es_mujer = c(mujer, rep(-1L, n2)))
  for (a in names(atr)) network::set.vertex.attribute(net, a, atr[[a]])
  tails <- integer(0); heads <- integer(0)
  for (j in seq_len(n2)) {
    S <- match(strsplit(regk$firmantes[j], "; ", fixed = TRUE)[[1]], roster)
    S <- S[!is.na(S)]
    tails <- c(tails, S); heads <- c(heads, rep(n1 + j, length(S)))
  }
  net <- network::add.edges(net, tail = tails, head = heads)
  membmat <- sapply(regk$commission, function(cc)
    if (nzchar(cc)) as.integer(comis_v == cc) else rep(0L, n1))
  list(net = net, memb = membmat, n2 = n2)
}

fit_one <- function(regk, label) {
  b <- build_net(regk)
  t0 <- Sys.time()
  out <- tryCatch({
    fit <- ergm(b$net ~ edges + edgecov(b$memb) + b1nodematch("conglomerado") +
                  b1nodematch("theta1_q") + b1nodematch("es_abogado") +
                  b1nodematch("experiencia") + b1nodematch("es_mujer"),
                control = control.ergm(seed = 42, MCMLE.maxit = 40,
                                       parallel = 8, parallel.type = "PSOCK"))
    sm <- summary(fit)
    data.frame(config = label, n_docs = b$n2,
               term = rownames(sm$coefficients),
               estimate = sm$coefficients[, 1], se = sm$coefficients[, 2],
               p = sm$coefficients[, ncol(sm$coefficients)], row.names = NULL)
  }, error = function(e) data.frame(config = label, n_docs = b$n2,
                                    term = paste("ERROR:", conditionMessage(e)),
                                    estimate = NA, se = NA, p = NA))
  out$secs <- round(as.numeric(difftime(Sys.time(), t0, units = "secs")), 1)
  cat(sprintf("  %-22s n_docs = %3d -> %.1f s%s\n", label, b$n2, out$secs[1],
              ifelse(grepl("ERROR", out$term[1]), "  [ERROR]", "")))
  out
}

LADDER <- list(
  "C3+C7"          = c("C3", "C7"),
  "C1+C2+C3"       = c("C1", "C2", "C3"),
  "C4+C5"          = c("C4", "C5"),
  "C1+C2+C4+C5"    = c("C1", "C2", "C4", "C5"),
  "asignadas (x7)" = paste0("C", 1:7)
)
res <- list()
for (lb in names(LADDER)) {
  regk <- registry[registry$commission %in% LADDER[[lb]], ]
  res[[lb]] <- fit_one(regk, lb)
}
res[["completa (947)"]] <- fit_one(registry, "completa (947)")

tab <- do.call(rbind, res)
write.csv(tab, file.path(RESULTS_TABLES, "M1_bipartite_scaling.csv"), row.names = FALSE)

ok <- tab[!grepl("ERROR", tab$term) & tab$term == "edges", c("config", "n_docs", "secs")]
if (nrow(ok) >= 3) {
  m <- lm(log(secs) ~ log(n_docs), data = ok)
  cat(sprintf("\n  Regla de escalado: secs ~ n_docs^%.2f (R2 = %.2f)\n",
              coef(m)[2], summary(m)$r.squared))
  cat(sprintf("  Predicción 947 docs: %.1f s | observado: %s\n",
              exp(predict(m, data.frame(n_docs = 947))),
              ifelse("completa (947)" %in% ok$config,
                     paste0(ok$secs[ok$config == "completa (947)"], " s"), "ERROR")))
}

# ---------------- ergm.multi: las 7 redes con coeficientes COMUNES -----------
cat("\n--- ergm.multi: ajuste conjunto de las 7 redes por comisión ---\n")
t0 <- Sys.time()
multi <- tryCatch({
  suppressPackageStartupMessages(library(ergm.multi))
  nets <- lapply(paste0("C", 1:7), function(cc) {
    b <- build_net(registry[registry$commission == cc, ])
    network::set.network.attribute(b$net, "membmat", b$memb)
    b$net
  })
  NN <- Networks(nets)
  fit <- ergm(NN ~ N(~edges + edgecov("membmat") + b1nodematch("conglomerado") +
                       b1nodematch("theta1_q") + b1nodematch("es_abogado") +
                       b1nodematch("experiencia") + b1nodematch("es_mujer")),
              control = control.ergm(seed = 42, MCMLE.maxit = 40,
                                     parallel = 8, parallel.type = "PSOCK"))
  sm <- summary(fit)
  df <- data.frame(config = "ergm.multi (7 conjuntas)", n_docs = 827,
                   term = rownames(sm$coefficients),
                   estimate = sm$coefficients[, 1], se = sm$coefficients[, 2],
                   p = sm$coefficients[, ncol(sm$coefficients)],
                   secs = round(as.numeric(difftime(Sys.time(), t0, units = "secs")), 1),
                   row.names = NULL)
  print(df[, c("term", "estimate", "se", "p")], row.names = FALSE, digits = 3)
  df
}, error = function(e) {
  cat("  ERROR ergm.multi:", conditionMessage(e), "\n")
  NULL
})
if (!is.null(multi)) {
  write.csv(rbind(tab, multi), file.path(RESULTS_TABLES, "M1_bipartite_scaling.csv"),
            row.names = FALSE)
  cat(sprintf("  ergm.multi en %.1f s\n", multi$secs[1]))
}
cat(sprintf("--- Done (total %.1f min) ---\n", as.numeric(difftime(Sys.time(), T00, units = "mins"))))

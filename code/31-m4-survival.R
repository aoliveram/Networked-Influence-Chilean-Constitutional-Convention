# =============================================================================
# 31-m4-survival.R  (2026-07-20, punto 9: m_degree FUERA de los modelos —
# colineal con m_constraint, VIF ~8-10; la columna sigue en m4_articles.csv)  (M4: supervivencia a nivel ARTÍCULO — diseño IV.D6 refinado)
#
# Unidad: el artículo génesis (no el convencional). DV principal:
#   survive_a = 1{outcome in (identico, similar)}  — llegó al borrador
# DV secundaria: y_a = sim_tfidf (0 si fallido/eliminado) — retención continua.
#
# Predictores = propiedades de la COALICIÓN firmante S_a (autores del artículo):
#   PIVOTAL:  dist_pivot = |media(theta1_fm) - theta1_(103)|; sd_theta1; tamaño
#   SNA:      grado medio, betweenness media, constraint media (red génesis
#             <=16), densidad interna (prop. de pares con w_ij >= 2: co-firma
#             MÁS ALLÁ de la iniciativa propia, que es mecánica)
#   K.HUMANO: prop. abogados, prop. experiencia, grado académico medio
#   + FE de comisión; SE cluster por COALICIÓN (hash del set de autores,
#     proxy de iniciativa: los artículos de una misma iniciativa comparten set)
# Continuas estandarizadas -> gamma_1 (pívot) y gamma_3 (SNA) en la misma cancha.
#
# Se excluyen coaliciones >16 (D8) y <2 autores-persona.
# Outputs: results/tables/M4_survival.csv, data/processed/m4_articles.csv
# =============================================================================

cat("=== 31-m4-survival.R ===\n")
suppressPackageStartupMessages({
  library(jsonlite); library(igraph); library(sandwich); library(lmtest); library(car)
})
set.seed(42)
source("code/paths.R")

profiles <- fromJSON(PROFILES)
roster <- sort(fromJSON(MEMBERS))
ip2d <- read.csv(file.path(DATA_PROCESSED, "ideal_points_2d_firstmonth.csv"), stringsAsFactors = FALSE)
metrics <- read.csv(file.path(DATA_PROCESSED, "network_metrics.csv"), stringsAsFactors = FALSE)
edges <- read.csv(file.path(DATA_PROCESSED, "genesis_network_initiative.csv"), stringsAsFactors = FALSE)
arts <- read.csv(file.path(DATA_PROCESSED, "track_article_outcomes.csv"), stringsAsFactors = FALSE)
sims <- read.csv(file.path(DATA_PROCESSED, "article_similarity_scores.csv"), stringsAsFactors = FALSE)

cat(sprintf("  Artículos con desenlace: %d | con similitud: %d\n", nrow(arts), nrow(sims)))
arts <- merge(arts, sims[, c("article_uid", "sim_tfidf")], by = "article_uid", all.x = TRUE)

# atributos indexados
theta1 <- setNames(ip2d$theta1_fm[match(roster, ip2d$nombre_armonizado)], roster)
pivot <- sort(ip2d$theta1_fm)[103]
att <- function(col) setNames(profiles[[col]][match(roster, profiles$nombre_armonizado)], roster)
abog <- att("es_abogado"); exper <- att("experiencia_previa_institucional")
grado <- att("grado_academico_nivel")
degv <- setNames(metrics$degree, metrics$nombre_armonizado)
betv <- setNames(metrics$betweenness, metrics$nombre_armonizado)
g <- graph_from_data_frame(edges, directed = FALSE, vertices = data.frame(name = roster))
E(g)$weight <- edges$weight
consv <- constraint(g, weights = E(g)$weight)
Wm <- matrix(0, length(roster), length(roster), dimnames = list(roster, roster))
Wm[cbind(edges$source, edges$target)] <- edges$weight
Wm[cbind(edges$target, edges$source)] <- edges$weight

# ---------------- dataset artículo-nivel --------------------------------------
build_row <- function(i) {
  S <- strsplit(arts$authors[i], "; ", fixed = TRUE)[[1]]
  S <- S[S %in% roster]
  s <- length(S)
  if (s < 2 || s > 16) return(NULL)
  th <- theta1[S]
  pares <- combn(s, 2)
  wpar <- Wm[cbind(S[pares[1, ]], S[pares[2, ]])]
  data.frame(
    article_uid = arts$article_uid[i], commission = arts$commission[i],
    survive = as.integer(arts$outcome_class[i] %in% c("identico", "similar")),
    y_art = ifelse(arts$outcome_class[i] %in% c("identico", "similar"),
                   ifelse(is.na(arts$sim_tfidf[i]), NA, arts$sim_tfidf[i]), 0),
    coalicion = paste(sort(S), collapse = "|"),
    size = s,
    dist_pivot = abs(mean(th) - pivot),
    sd_theta1 = sd(th),
    m_degree = mean(degv[S]), m_betw = mean(betv[S]),
    m_constraint = mean(consv[S]),
    dens_interna = mean(wpar >= 2),
    sh_abogado = mean(abog[S]), sh_exper = mean(exper[S]),
    m_grado = mean(grado[S]),
    stringsAsFactors = FALSE
  )
}
m4 <- do.call(rbind, lapply(seq_len(nrow(arts)), build_row))
m4$cl <- as.integer(factor(m4$coalicion))
cat(sprintf("  M4: %d artículos | %d coaliciones distintas | supervivencia media %.3f\n",
            nrow(m4), length(unique(m4$cl)), mean(m4$survive)))
write.csv(m4, file.path(DATA_PROCESSED, "m4_articles.csv"), row.names = FALSE)

CONT <- c("dist_pivot", "sd_theta1", "size", "m_degree", "m_betw",
          "m_constraint", "dens_interna", "sh_abogado", "sh_exper", "m_grado")
m4z <- m4
m4z[CONT] <- scale(m4z[CONT])

fit_cl <- function(f, data, label) {
  m <- glm(f, data = data, family = binomial())
  ct <- coeftest(m, vcov. = vcovCL(m, cluster = data$cl))
  keep <- !grepl("commission|Intercept", rownames(ct))
  data.frame(model = label, term = rownames(ct)[keep], estimate = ct[keep, 1],
             se_cl = ct[keep, 2], p = ct[keep, 4],
             aic = AIC(m), n = nobs(m), row.names = NULL)
}

t1 <- fit_cl(survive ~ dist_pivot + sd_theta1 + size + factor(commission), m4z, "1 pivotal")
t2 <- fit_cl(survive ~ dist_pivot + sd_theta1 + size + m_betw +
               m_constraint + dens_interna + factor(commission), m4z, "2 + SNA")
t3 <- fit_cl(survive ~ dist_pivot + sd_theta1 + size + m_betw +
               m_constraint + dens_interna + sh_abogado + sh_exper + m_grado +
               factor(commission), m4z, "3 + capital humano")
tab <- rbind(t1, t2, t3)
dir.create(RESULTS_TABLES, recursive = TRUE, showWarnings = FALSE)
write.csv(tab, file.path(RESULTS_TABLES, "M4_survival.csv"), row.names = FALSE)
cat("\n--- M4 logit (coef estandarizados; SE cluster por coalición) ---\n")
print(tab, row.names = FALSE, digits = 3)

vf <- vif(glm(survive ~ dist_pivot + sd_theta1 + size + m_betw +
                m_constraint + dens_interna + sh_abogado + sh_exper + m_grado,
              data = m4z, family = binomial()))
cat("\n  VIF (modelo 3, sin FE):\n")
print(round(vf, 1))

# DV secundaria: retención continua con fallidos = 0 (OLS, mismos X)
m_ols <- lm(y_art ~ dist_pivot + sd_theta1 + size + m_betw +
              m_constraint + dens_interna + sh_abogado + sh_exper + m_grado +
              factor(commission), data = m4z)
ct_ols <- coeftest(m_ols, vcov. = vcovCL(m_ols, cluster = m4z$cl[!is.na(m4z$y_art)]))
keep <- !grepl("commission|Intercept", rownames(ct_ols))
cat("\n--- DV secundaria: y_art (retención, fallidos = 0), OLS cluster ---\n")
print(round(ct_ols[keep, ], 4))
write.csv(data.frame(term = rownames(ct_ols)[keep], ct_ols[keep, ], row.names = NULL),
          file.path(RESULTS_TABLES, "M4_retention_ols.csv"), row.names = FALSE)
cat("--- Done ---\n")

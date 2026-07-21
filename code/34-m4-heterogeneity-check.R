# =============================================================================
# 34-m4-heterogeneity-check.R  (comentario del autor 2026-07-18, punto 15)
# ¿La "heterogeneidad ideológica ayuda a sobrevivir" es solo dispersión DENTRO
# de la izquierda (que era mayoría)? Chequeo con lo disponible: clasificar las
# coaliciones por su POSICIÓN MEDIA en theta1 (cuartiles) y ver si el efecto de
# sd_theta1 se sostiene en cada tramo, más el modelo con interacción.
# (La versión con votaciones — clasificar artículos por QUIÉNES los votaron —
#  requiere el vínculo roll-call <-> artículo, que aún no existe en los datos;
#  queda anotada como extensión.)
#
# Input:  data/processed/m4_articles.csv (de 31)
# Output: results/tables/M4_heterogeneity_check.csv
# =============================================================================

cat("=== 34-m4-heterogeneity-check.R ===\n")
suppressPackageStartupMessages({ library(jsonlite); library(sandwich); library(lmtest) })
set.seed(42)
source("code/paths.R")

m4 <- read.csv(file.path(DATA_PROCESSED, "m4_articles.csv"), stringsAsFactors = FALSE)
ip2d <- read.csv(file.path(DATA_PROCESSED, "ideal_points_2d_firstmonth.csv"), stringsAsFactors = FALSE)
pivot <- sort(ip2d$theta1_fm)[103]

# posición media de la coalición (reconstruida del dataset de 31: dist_pivot
# es |media - pivot|, pero necesitamos el signo -> recomputamos la media)
profiles_theta <- setNames(ip2d$theta1_fm, ip2d$nombre_armonizado)
m4$mean_theta1 <- vapply(strsplit(m4$coalicion, "|", fixed = TRUE),
                         function(S) mean(profiles_theta[S]), numeric(1))
cat(sprintf("  Coaliciones: media de theta1 — min %.2f, mediana %.2f, max %.2f | %% con media < 0 (izquierda): %.1f%%\n",
            min(m4$mean_theta1), median(m4$mean_theta1), max(m4$mean_theta1),
            100 * mean(m4$mean_theta1 < 0)))

# CUARTILES (decisión del autor 2026-07-21: gradiente más fino que cuartiles)
m4$cuartil <- cut(m4$mean_theta1, quantile(m4$mean_theta1, probs = seq(0, 1, 0.25)),
                  include.lowest = TRUE,
                  labels = c("Q1 izquierda", "Q2", "Q3", "Q4 centro-derecha"))
print(tapply(m4$mean_theta1, m4$cuartil, function(x) round(range(x), 2)))

CONT <- c("dist_pivot", "sd_theta1", "size", "m_degree", "m_betw",
          "m_constraint", "dens_interna")
m4z <- m4
m4z[CONT] <- scale(m4z[CONT])

fit_cl <- function(data, label) {
  m <- glm(survive ~ dist_pivot + sd_theta1 + size + m_betw +
             m_constraint + dens_interna + factor(commission),
           data = data, family = binomial())
  ct <- coeftest(m, vcov. = vcovCL(m, cluster = data$cl))
  keep <- rownames(ct) %in% c("dist_pivot", "sd_theta1", "dens_interna")
  data.frame(muestra = label, term = rownames(ct)[keep], estimate = ct[keep, 1],
             se = ct[keep, 2], p = ct[keep, 4], n = nobs(m),
             surv_media = mean(data$survive), row.names = NULL)
}

res <- rbind(
  fit_cl(m4z, "todas"),
  do.call(rbind, lapply(levels(m4z$cuartil), function(q)
    fit_cl(m4z[m4z$cuartil == q, ], q))),
  fit_cl(m4z[m4z$cuartil %in% c("Q1 izquierda", "Q2"), ], "Q1+Q2 (izquierda)"),
  fit_cl(m4z[m4z$cuartil %in% c("Q3", "Q4 centro-derecha"), ], "Q3+Q4 (centro)")
)


dir.create(RESULTS_TABLES, recursive = TRUE, showWarnings = FALSE)
write.csv(res, file.path(RESULTS_TABLES, "M4_heterogeneity_check.csv"), row.names = FALSE)
cat("\n--- sd_theta1 (y dist_pivot) por tramo de posición media de la coalición ---\n")
print(res, row.names = FALSE, digits = 3)
cat("--- Done ---\n")

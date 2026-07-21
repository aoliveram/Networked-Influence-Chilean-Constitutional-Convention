# =============================================================================
# 44-clogit-by-bloc.R  (comentario del autor 2026-07-21, punto 2: PROMOVIDO
# al reporte en reemplazo del bloque PPOO)
# Logit condicional de elección de firma SEPARADO POR BLOQUE POLÍTICO DEL
# ELECTOR: mismos menús (estratos = iniciativa), muestra restringida a los
# convencionales del bloque -> beta específico del bloque.
#   5 bloques: Derecha (VC 37) | Centro-izq (LA+INN 28) | Izquierda (AD+LdP 51)
#              | PPOO (17) | Otras listas locales (21)
# NOTA técnica: "misma lista modal" NO es identificable dentro de bloques de
# un solo conglomerado (sin variación intra-estrato -> separación); se excluye
# de estas especificaciones (la coordinación de lista está en la Tabla 2).
# EE cluster por convencional; Efron.
#
# Output: results/tables/M1_clogit_by_bloc.csv
# =============================================================================

cat("=== 44-clogit-by-bloc.R ===\n")
suppressPackageStartupMessages({ library(survival) })
source("code/paths.R")

ch <- read.csv(file.path(DATA_PROCESSED, "choice_dataset.csv"), stringsAsFactors = FALSE)
listas <- read.csv(file.path(DATA_RAW, "electoral_lists.csv"), stringsAsFactors = FALSE)
cong <- setNames(listas$conglomerado, listas$nombre_armonizado)
cong[cong == "REVISAR (lista local sin conglomerado)"] <- "Otras listas locales"
cg <- cong[ch$nombre_armonizado]
ch$bloc5 <- ifelse(cg == "Vamos por Chile", "Derecha (VC)",
            ifelse(cg %in% c("Lista del Apruebo", "Independientes No Neutrales"), "Centro-izq (LA+INN)",
            ifelse(cg %in% c("Apruebo Dignidad", "Lista del Pueblo"), "Izquierda (AD+LdP)",
            ifelse(cg == "Escaños Reservados PPOO", "PPOO", "Otras listas locales"))))

FORM <- signed ~ d_theta1 + d_theta2 + misma_comision +
  distrito_aff + abogado_aff + exper_aff + mujer_aff + d_grado + d_edad +
  strata(initiative_id) + cluster(nombre_armonizado)

res <- do.call(rbind, lapply(sort(unique(ch$bloc5)), function(b) {
  sub <- ch[ch$bloc5 == b, ]
  m <- clogit(FORM, data = sub, method = "efron")
  sm <- summary(m)$coefficients
  data.frame(bloc = b, n_conv = length(unique(sub$nombre_armonizado)),
             term = rownames(sm), estimate = sm[, "coef"], se = sm[, "se(coef)"],
             p = sm[, "Pr(>|z|)"], row.names = NULL)
}))

cat("\n--- coeficientes por bloque (columnas = bloques) ---\n")
for (tm in unique(res$term)) {
  cat(sprintf("%-16s", tm))
  for (b in unique(res$bloc)) {
    r <- res[res$bloc == b & res$term == tm, ]
    st <- ifelse(r$p < .001, "***", ifelse(r$p < .01, "**", ifelse(r$p < .05, "*",
          ifelse(r$p < .1, "+", ""))))
    cat(sprintf(" %+6.2f%-3s", r$estimate, st))
  }
  cat("\n")
}
cat("orden:", paste(unique(res$bloc), collapse = " | "), "\n")
dir.create(RESULTS_TABLES, recursive = TRUE, showWarnings = FALSE)
write.csv(res, file.path(RESULTS_TABLES, "M1_clogit_by_bloc.csv"), row.names = FALSE)
cat("--- Done ---\n")

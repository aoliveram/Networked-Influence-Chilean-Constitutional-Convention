# =============================================================================
# 12-ideal-points-2d.R  (respuesta a revisión: D3/D4/D5)
# Puntos ideales en DOS dimensiones, estimados SOLO con las votaciones del
# primer mes del Pleno (2021-07-04 a 2021-08-12), replicando el diseño de
# Fábrega (2022): W-NOMINATE 2D sobre roll-calls, sin encuestas (las encuestas
# en Fábrega son solo validación externa). Esta ideología es PRE-RED (anterior
# a las comisiones temáticas, creadas en oct-2021, y a las iniciativas de
# ene-mar 2022), por lo que sirve de covariable exógena para M1/M3.
#
# El dynIRT del pipeline (emIRT) es unidimensional; esta estimación 2D es
# complementaria y estática (un punto por convencional).
#
# Output: data/processed/ideal_points_2d_firstmonth.csv
# =============================================================================

cat("=== 12-ideal-points-2d.R ===\n")
suppressPackageStartupMessages({ library(wnominate); library(pscl) })
set.seed(42)
source("code/paths.R")

inp <- readRDS(file.path(EMIRT_DIR, "emIRT_data_input.rds"))
meta <- readRDS(file.path(EMIRT_DIR, "emIRT_metadata.rds"))

cutoff <- as.Date("2021-08-12")
fm_periods <- which(meta$unique_dates <= cutoff)
fm_cols <- which(inp$bill.session + 1 <= max(fm_periods))
cat(sprintf("  Ventana: %s a %s | %d períodos | %d roll-calls (Fábrega 2022: 146)\n",
            format(min(meta$unique_dates)), format(cutoff),
            length(fm_periods), length(fm_cols)))

# rc: 1 = a favor, -1 = en contra, 0 = ausente/abstención (omitida, como Fábrega)
rc_fm <- inp$rc[, fm_cols]
votes <- matrix(9, nrow(rc_fm), ncol(rc_fm))   # 9 = missing para rollcall()
votes[rc_fm == 1] <- 1
votes[rc_fm == -1] <- 6
rownames(votes) <- meta$votantes

rc_obj <- rollcall(votes, yea = 1, nay = 6, missing = 9, notInLegis = NA,
                   legis.names = meta$votantes,
                   desc = "Pleno CC, primer mes (jul-ago 2021)")

# polaridad: dim1 -> Marinovic (derecha positiva); dim2 -> Linconao (solo fija
# el signo; absdiff es invariante a reflexión)
pol1 <- which(meta$votantes == "Marinovic, Teresa")
pol2 <- which(meta$votantes == "Linconao, Francisca")
stopifnot(length(pol1) == 1, length(pol2) == 1)

fit <- wnominate(rc_obj, dims = 2, polarity = c(pol1, pol2))
cat("\n--- Ajuste ---\n")
cat(sprintf("  Clasificación correcta: dim1 %.2f%% | 2D %.2f%% (Fábrega: 89.25%% / 91.43%%)\n",
            fit$fits[1] , fit$fits[2]))

res <- data.frame(
  nombre_armonizado = rownames(fit$legislators),
  theta1_fm = fit$legislators$coord1D,
  theta2_fm = fit$legislators$coord2D,
  n_votes = fit$legislators$correctYea + fit$legislators$wrongYea +
            fit$legislators$correctNay + fit$legislators$wrongNay,
  stringsAsFactors = FALSE
)
n_na <- sum(is.na(res$theta1_fm))
cat(sprintf("  Convencionales estimados: %d/154 (%d NA por pocos votos)\n",
            sum(!is.na(res$theta1_fm)), n_na))

# sanity: signos esperados
chk <- res[res$nombre_armonizado %in%
             c("Marinovic, Teresa", "Baradit, Jorge", "Linconao, Francisca",
               "Cubillos, Marcela", "Atria, Fernando"), ]
print(chk, row.names = FALSE, digits = 3)

# correlación con el dynIRT 1D (media) — deben alinearse en dim1
dyn <- read.csv(file.path(DATA_PROCESSED, "emirt_summary_metrics.csv"), stringsAsFactors = FALSE)
m <- merge(res, dyn[, c("nombre_armonizado", "theta_mean")], by = "nombre_armonizado")
cat(sprintf("  cor(theta1_fm, dynIRT theta_mean) = %.3f (esperado ~alto y positivo)\n",
            cor(m$theta1_fm, m$theta_mean, use = "complete.obs")))

write.csv(res, file.path(DATA_PROCESSED, "ideal_points_2d_firstmonth.csv"), row.names = FALSE)
cat(sprintf("  -> %s\n", file.path(DATA_PROCESSED, "ideal_points_2d_firstmonth.csv")))
cat("--- Done ---\n")

# =============================================================================
# 17-listas-rice.R  (respuesta a revisión: D2.2 — cohesión de voto)
# ¿Las listas ad hoc votaron unidas como partidos? Índice de Rice por lista y
# votación:  R_{lv} = |Y_{lv} - N_{lv}| / (Y_{lv} + N_{lv}),
# sobre los 4.707 roll-calls del Pleno (jul-2021 a jun-2022).
#
# (a) media global por conglomerado, contra el benchmark de PSEUDO-LISTAS:
#     500 grupos aleatorios del mismo tamaño y misma vecindad ideológica
#     (muestreo ponderado por kernel gaussiano centrado en la media theta1 de
#     la lista real; espíritu del test de permutación de Fábrega §VII).
#     El "premio de cohesión" = R real - media pseudo; p = proporción de
#     pseudo-listas con R >= real.
# (b) serie mensual R_{l,t}: un partido sostiene R en el tiempo; una etiqueta
#     electoral sin organización lo ve decaer.
#
# Outputs: results/tables/rice_summary.csv, results/tables/rice_monthly.csv
# =============================================================================

cat("=== 17-listas-rice.R ===\n")
suppressPackageStartupMessages({ library(jsonlite) })
set.seed(42)
source("code/paths.R")

inp <- readRDS(file.path(EMIRT_DIR, "emIRT_data_input.rds"))
meta <- readRDS(file.path(EMIRT_DIR, "emIRT_metadata.rds"))
listas <- read.csv(file.path(DATA_RAW, "electoral_lists.csv"), stringsAsFactors = FALSE)
ip2d <- read.csv(file.path(DATA_PROCESSED, "ideal_points_2d_firstmonth.csv"), stringsAsFactors = FALSE)

rc <- inp$rc                                   # 154 x 4707: 1 sí, -1 no, 0 ausente
votantes <- meta$votantes
vote_date <- meta$unique_dates[inp$bill.session + 1]
vote_month <- format(vote_date, "%Y-%m")
congl <- listas$conglomerado[match(votantes, listas$nombre_armonizado)]
theta1 <- ip2d$theta1_fm[match(votantes, ip2d$nombre_armonizado)]
GROUPS <- c("Vamos por Chile", "Apruebo Dignidad", "Lista del Apruebo",
            "Lista del Pueblo", "Escaños Reservados PPOO")
MIN_VOTERS <- 5    # mínimo de miembros votando S/N para computar R_lv
N_PSEUDO <- 500

rice_by_vote <- function(members_idx) {
  sub <- rc[members_idx, , drop = FALSE]
  Y <- colSums(sub == 1); N <- colSums(sub == -1)
  R <- abs(Y - N) / (Y + N)
  R[(Y + N) < MIN_VOTERS] <- NA
  R
}

summary_rows <- list(); monthly_rows <- list()
for (g in GROUPS) {
  idx <- which(congl == g)
  R <- rice_by_vote(idx)
  mu_g <- mean(theta1[idx]); sd_g <- sd(theta1[idx]); n_g <- length(idx)
  h <- max(sd_g, 0.2)

  # pseudo-listas: mismo tamaño, muestreadas con peso gaussiano en theta1
  wts <- exp(-(theta1 - mu_g)^2 / (2 * h^2))
  pseudo_R <- replicate(N_PSEUDO, {
    ps <- sample(seq_along(votantes), n_g, prob = wts)
    mean(rice_by_vote(ps), na.rm = TRUE)
  })
  real_R <- mean(R, na.rm = TRUE)
  summary_rows[[g]] <- data.frame(
    conglomerado = g, n_miembros = n_g, theta1_media = mu_g,
    rice_real = real_R, rice_pseudo_media = mean(pseudo_R),
    premio_cohesion = real_R - mean(pseudo_R),
    p_perm = mean(pseudo_R >= real_R),
    votos_validos = sum(!is.na(R))
  )
  agg <- aggregate(R, by = list(month = vote_month), FUN = function(x) mean(x, na.rm = TRUE))
  cnt <- aggregate(!is.na(R), by = list(month = vote_month), FUN = sum)
  monthly_rows[[g]] <- data.frame(conglomerado = g, month = agg$month,
                                  rice = agg$x, n_votes = cnt$x)
}
sum_tab <- do.call(rbind, summary_rows)
mon_tab <- do.call(rbind, monthly_rows)
mon_tab <- mon_tab[mon_tab$n_votes >= 10, ]   # meses con base suficiente

cat("\n--- Cohesión de Rice por conglomerado (vs pseudo-listas) ---\n")
print(sum_tab, row.names = FALSE, digits = 3)
dir.create(RESULTS_TABLES, recursive = TRUE, showWarnings = FALSE)
write.csv(sum_tab, file.path(RESULTS_TABLES, "rice_summary.csv"), row.names = FALSE)
write.csv(mon_tab, file.path(RESULTS_TABLES, "rice_monthly.csv"), row.names = FALSE)
cat("\n-> rice_summary.csv, rice_monthly.csv\n--- Done ---\n")

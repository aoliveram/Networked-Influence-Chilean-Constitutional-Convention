# =============================================================================
# 02-extract-emirt-temporal.R  (v2, actualización 7 comisiones)
# Extrae la matriz N x T de puntos ideales dynIRT y la alinea a las ondas de
# las 7 comisiones (fechas observadas en commission_waves.csv, año 2022).
# =============================================================================

cat("=== 02-extract-emirt-temporal.R (v2) ===\n")
source("code/paths.R")

lout <- readRDS(file.path(EMIRT_DIR, "emIRT_model_output.rds"))
metadata <- readRDS(file.path(EMIRT_DIR, "emIRT_metadata.rds"))

x_hat <- lout$means$x
N <- nrow(x_hat); T_periods <- ncol(x_hat)
legislator_names <- metadata$votantes
unique_dates <- metadata$unique_dates
cat(sprintf("  N=%d legisladores, T=%d períodos (%s a %s)\n", N, T_periods,
            format(min(unique_dates)), format(max(unique_dates))))

boot_file <- file.path(EMIRT_DIR, "emIRT_bootstrap_output.rds")
if (file.exists(boot_file)) {
  x_se <- readRDS(boot_file)$bse$x
} else {
  x_se <- matrix(NA, nrow = N, ncol = T_periods)
  cat("  (sin SEs bootstrap; theta_se = NA)\n")
}

# --- puntos ideales completos (formato largo) ---
grid <- expand.grid(period = seq_len(T_periods), legislator_idx = seq_len(N))
full_long <- data.frame(
  legislator = legislator_names[grid$legislator_idx],
  period = grid$period,
  date = format(unique_dates[grid$period], "%Y-%m-%d"),
  theta = x_hat[cbind(grid$legislator_idx, grid$period)],
  theta_se = x_se[cbind(grid$legislator_idx, grid$period)],
  stringsAsFactors = FALSE
)
full_long <- full_long[order(full_long$legislator, full_long$period), ]
write.csv(full_long, file.path(DATA_PROCESSED, "emirt_ideal_points_full.csv"), row.names = FALSE)

# --- resumen por legislador ---
summary_df <- data.frame(
  nombre_armonizado = legislator_names,
  theta_mean = rowMeans(x_hat),
  theta_sd = apply(x_hat, 1, sd),
  theta_min = apply(x_hat, 1, min),
  theta_max = apply(x_hat, 1, max),
  theta_range = apply(x_hat, 1, function(x) max(x) - min(x)),
  theta_first = x_hat[, 1],
  theta_last = x_hat[, T_periods],
  theta_shift = x_hat[, T_periods] - x_hat[, 1],
  stringsAsFactors = FALSE
)
write.csv(summary_df, file.path(DATA_PROCESSED, "emirt_summary_metrics.csv"), row.names = FALSE)
cat(sprintf("  Resumen: %d legisladores\n", nrow(summary_df)))

# --- alineación a las ondas de las 7 comisiones ---
waves <- read.csv(file.path(DATA_PROCESSED, "commission_waves.csv"),
                  stringsAsFactors = FALSE, colClasses = "character")
waves$step <- as.integer(waves$step)

to_full_date <- function(md) as.Date(paste0("2022-", md))
find_nearest_period <- function(target_date) {
  diffs <- as.numeric(target_date - unique_dates)
  valid <- which(diffs >= 0)
  if (length(valid) == 0) return(1L)
  valid[which.min(diffs[valid])]
}

aligned_parts <- list()
for (comm in unique(waves$commission)) {
  sub <- waves[waves$commission == comm, ]
  sub <- sub[order(sub$step), ]
  # T0 génesis: período que precede a la primera onda de indicaciones
  step_labels <- c("T0_Genesis", paste0("T", sub$step, "_", sub$date_mmdd))
  step_dates <- c(to_full_date(sub$date_mmdd[1]), to_full_date(sub$date_mmdd))
  indices <- vapply(step_dates, find_nearest_period, integer(1))
  for (s in seq_along(step_labels)) {
    pidx <- indices[s]
    aligned_parts[[length(aligned_parts) + 1]] <- data.frame(
      legislator = legislator_names,
      commission = comm,
      step = s - 1L,
      step_label = step_labels[s],
      step_date = format(step_dates[s], "%Y-%m-%d"),
      emirt_period = pidx,
      emirt_date = format(unique_dates[pidx], "%Y-%m-%d"),
      theta = x_hat[, pidx],
      theta_se = x_se[, pidx],
      stringsAsFactors = FALSE
    )
  }
  cat(sprintf("  %s: %d pasos -> períodos emIRT %s\n", comm, length(step_labels),
              paste(indices, collapse = ", ")))
}
aligned_df <- do.call(rbind, aligned_parts)
write.csv(aligned_df, file.path(DATA_PROCESSED, "emirt_aligned_to_commissions.csv"), row.names = FALSE)
cat(sprintf("  Alineado: %d filas\n", nrow(aligned_df)))
cat("--- Done ---\n")

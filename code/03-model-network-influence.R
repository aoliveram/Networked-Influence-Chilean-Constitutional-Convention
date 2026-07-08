# =============================================================================
# 03-model-network-influence.R  (v2, actualización 7 comisiones)
# Modelo 2: dinámica ideológica — exposición de red y panel con efectos fijos.
#
#   Delta_theta_it = alpha_i + b1*theta_{i,t-1} + b3*NetExp_{i,t-1} + e_it
#   NetExp: promedio ponderado del theta de los co-firmantes, red ACUMULADA
#   hasta t-1 (especificación principal).
#
# Robusteces de ventana (decisión 2026-07-06): exposición con decaimiento
# exponencial (lambda = 0.5 por onda) y exposición solo-última-onda; además
# Delta_theta estandarizado por días transcurridos. Falsificación con lead.
# Los pasos con el mismo período emIRT que el anterior (delta identicamente 0)
# se excluyen del panel.
# =============================================================================

cat("=== 03-model-network-influence.R (v2) ===\n")
suppressPackageStartupMessages({
  library(jsonlite); library(plm); library(lmtest); library(sandwich)
})
set.seed(42)
source("code/paths.R")

ip_full <- read.csv(file.path(DATA_PROCESSED, "emirt_ideal_points_full.csv"), stringsAsFactors = FALSE)
ip_aligned <- read.csv(file.path(DATA_PROCESSED, "emirt_aligned_to_commissions.csv"), stringsAsFactors = FALSE)
profiles <- fromJSON(PROFILES)
all_legislators <- sort(unique(ip_full$legislator))
n_leg <- length(all_legislators)
commissions <- sort(unique(ip_aligned$commission))
cat(sprintf("  Legisladores: %d | comisiones: %s\n", n_leg, paste(commissions, collapse = ", ")))

# --- matrices de adyacencia acumuladas por (comisión, paso) ---
edges_to_adj <- function(edges) {
  adj <- matrix(0, n_leg, n_leg, dimnames = list(all_legislators, all_legislators))
  for (e in edges) {
    adj[e$source, e$target] <- e$weight
    adj[e$target, e$source] <- e$weight
  }
  adj
}
adj_by_step <- list()
for (comm in commissions) {
  net_data <- fromJSON(file.path(DATA_PROCESSED, paste0(comm, "_dynamic_networks.json")),
                       simplifyVector = FALSE)
  for (w in seq_along(net_data)) {
    adj_by_step[[paste0(comm, "_", w - 1)]] <- edges_to_adj(net_data[[w]])
  }
  cat(sprintf("  %s: %d ondas cargadas\n", comm, length(net_data)))
}

# incrementos por onda (para decaimiento y última-onda)
inc_by_step <- list()
for (comm in commissions) {
  steps <- sort(unique(ip_aligned$step[ip_aligned$commission == comm]))
  for (s in steps) {
    key <- paste0(comm, "_", s)
    inc_by_step[[key]] <- if (s == 0) adj_by_step[[key]] else
      adj_by_step[[key]] - adj_by_step[[paste0(comm, "_", s - 1)]]
  }
}

exposure_from_W <- function(Wrow, theta_vec) {
  valid <- Wrow > 0 & !is.na(theta_vec)
  if (!any(valid)) return(NA_real_)
  sum(Wrow[valid] * theta_vec[valid]) / sum(Wrow[valid])
}

# --- panel: exposición por legislador x (comisión, paso) ---
LAMBDA <- 0.5
comm_steps <- unique(ip_aligned[, c("commission", "step", "step_label", "emirt_period", "emirt_date")])
theta_by_period <- split(ip_full, ip_full$period)

panel <- list()
for (r in seq_len(nrow(comm_steps))) {
  comm <- comm_steps$commission[r]; step <- comm_steps$step[r]
  period <- comm_steps$emirt_period[r]
  W <- adj_by_step[[paste0(comm, "_", step)]]
  ipp <- theta_by_period[[as.character(period)]]
  theta_vec <- setNames(ipp$theta, ipp$legislator)[all_legislators]

  # decaimiento: suma de incrementos ponderados por lambda^(step - s)
  Wdec <- matrix(0, n_leg, n_leg, dimnames = dimnames(W))
  for (s in 0:step) Wdec <- Wdec + LAMBDA^(step - s) * inc_by_step[[paste0(comm, "_", s)]]
  Wlast <- inc_by_step[[paste0(comm, "_", step)]]

  own <- theta_vec
  panel[[length(panel) + 1]] <- data.frame(
    legislator = all_legislators,
    commission = comm, step = step,
    emirt_period = period, emirt_date = comm_steps$emirt_date[r],
    theta = own,
    net_exposure = vapply(all_legislators, function(l) exposure_from_W(W[l, ], theta_vec), numeric(1)),
    net_exposure_decay = vapply(all_legislators, function(l) exposure_from_W(Wdec[l, ], theta_vec), numeric(1)),
    net_exposure_last = vapply(all_legislators, function(l) exposure_from_W(Wlast[l, ], theta_vec), numeric(1)),
    n_partners = vapply(all_legislators, function(l) sum(W[l, ] > 0), numeric(1)),
    stringsAsFactors = FALSE
  )
}
panel_df <- do.call(rbind, panel)
panel_df <- panel_df[order(panel_df$legislator, panel_df$commission, panel_df$step), ]

# --- deltas y rezagos (excluyendo pasos con el mismo período emIRT) ---
panel_df$delta_theta <- NA; panel_df$theta_lag <- NA
panel_df$net_exposure_lag <- NA; panel_df$net_exposure_decay_lag <- NA
panel_df$net_exposure_last_lag <- NA; panel_df$net_exposure_lead <- NA
panel_df$days_elapsed <- NA
dropped_same_period <- 0

for (comm in commissions) {
  for (leg in all_legislators) {
    idx <- which(panel_df$legislator == leg & panel_df$commission == comm)
    if (length(idx) < 2) next
    sub <- panel_df[idx, ]
    for (j in 2:length(idx)) {
      if (sub$emirt_period[j] == sub$emirt_period[j - 1]) { dropped_same_period <- dropped_same_period + 1; next }
      panel_df$delta_theta[idx[j]] <- sub$theta[j] - sub$theta[j - 1]
      panel_df$theta_lag[idx[j]] <- sub$theta[j - 1]
      panel_df$net_exposure_lag[idx[j]] <- sub$net_exposure[j - 1]
      panel_df$net_exposure_decay_lag[idx[j]] <- sub$net_exposure_decay[j - 1]
      panel_df$net_exposure_last_lag[idx[j]] <- sub$net_exposure_last[j - 1]
      if (j < length(idx)) panel_df$net_exposure_lead[idx[j]] <- sub$net_exposure[j + 1]
      panel_df$days_elapsed[idx[j]] <-
        as.numeric(as.Date(sub$emirt_date[j]) - as.Date(sub$emirt_date[j - 1]))
    }
  }
}
cat(sprintf("  Celdas paso-mismo-período excluidas del delta: %d\n", dropped_same_period))

for (col in c("es_abogado", "experiencia_previa_institucional", "es_mujer")) {
  panel_df[[col]] <- profiles[[col]][match(panel_df$legislator, profiles$nombre_armonizado)]
}
write.csv(panel_df, file.path(DATA_PROCESSED, "network_exposure_panel.csv"), row.names = FALSE)

reg_df <- panel_df[!is.na(panel_df$delta_theta) & !is.na(panel_df$net_exposure_lag) &
                     !is.na(panel_df$theta_lag), ]
cat(sprintf("  Muestra de regresión: %d obs, %d legisladores, comisiones: %s\n",
            nrow(reg_df), length(unique(reg_df$legislator)),
            paste(sort(unique(reg_df$commission)), collapse = ", ")))

reg_df$comm_step <- paste(reg_df$commission, reg_df$step, sep = "_")
pdata <- pdata.frame(reg_df, index = c("legislator", "comm_step"), drop.index = FALSE)
results <- list()
tab <- list()
grab <- function(model, label, term = "net_exposure_lag", vcov. = NULL) {
  ct <- if (is.null(vcov.)) coeftest(model) else coeftest(model, vcov. = vcov.)
  i <- grep(term, rownames(ct))[1]
  tab[[length(tab) + 1]] <<- data.frame(model = label, term = rownames(ct)[i],
                                        estimate = ct[i, 1], se = ct[i, 2], p = ct[i, 4])
}

cat("\n[A] OLS agrupado\n")
mod_ols <- lm(delta_theta ~ theta_lag + net_exposure_lag +
                es_abogado + experiencia_previa_institucional + es_mujer, data = reg_df)
print(round(coef(summary(mod_ols)), 4))
grab(mod_ols, "OLS agrupado", vcov. = vcovCL(mod_ols, cluster = reg_df$legislator))
results$mod_ols <- mod_ols

cat("\n[B] Efectos fijos (within)\n")
mod_fe <- plm(delta_theta ~ theta_lag + net_exposure_lag, data = pdata, model = "within")
print(round(coeftest(mod_fe, vcov = vcovHC(mod_fe, type = "HC1", cluster = "group")), 4))
grab(mod_fe, "Efectos fijos", vcov. = vcovHC(mod_fe, type = "HC1", cluster = "group"))
results$mod_fe <- mod_fe

cat("\n[C] Efectos aleatorios + Hausman\n")
mod_re <- tryCatch(plm(delta_theta ~ theta_lag + net_exposure_lag +
                         es_abogado + experiencia_previa_institucional + es_mujer,
                       data = pdata, model = "random"), error = function(e) NULL)
if (!is.null(mod_re)) {
  ht <- phtest(mod_fe, mod_re)
  cat(sprintf("  Hausman: chi2 = %.2f, p = %.2e -> %s\n", ht$statistic, ht$p.value,
              ifelse(ht$p.value < 0.05, "usar FE", "RE aceptable")))
  results$mod_re <- mod_re; results$hausman <- ht
}

cat("\n[D] OLS + FE de comisión\n")
mod_comm <- lm(delta_theta ~ theta_lag + net_exposure_lag + es_abogado +
                 experiencia_previa_institucional + es_mujer + factor(commission), data = reg_df)
grab(mod_comm, "OLS + FE comisión", vcov. = vcovCL(mod_comm, cluster = reg_df$legislator))
results$mod_comm_fe <- mod_comm

cat("\n[E] Falsificación: exposición futura (lead)\n")
lead_df <- reg_df[!is.na(reg_df$net_exposure_lead), ]
mod_lead <- lm(delta_theta ~ theta_lag + net_exposure_lead, data = lead_df)
print(round(coeftest(mod_lead, vcov = vcovCL(mod_lead, cluster = lead_df$legislator)), 4))
grab(mod_lead, "Falsificación (lead)", term = "net_exposure_lead",
     vcov. = vcovCL(mod_lead, cluster = lead_df$legislator))
results$mod_lead <- mod_lead

cat("\n[F] Robustez: exposición con decaimiento (lambda = 0.5)\n")
mod_dec <- plm(delta_theta ~ theta_lag + net_exposure_decay_lag, data = pdata, model = "within")
grab(mod_dec, "FE, exposición decaimiento", term = "net_exposure_decay_lag",
     vcov. = vcovHC(mod_dec, type = "HC1", cluster = "group"))
results$mod_decay <- mod_dec

cat("[G] Robustez: exposición solo-última-onda\n")
last_ok <- !is.na(pdata$net_exposure_last_lag)
mod_last <- tryCatch(plm(delta_theta ~ theta_lag + net_exposure_last_lag,
                         data = pdata[last_ok, ], model = "within"), error = function(e) NULL)
if (!is.null(mod_last)) {
  grab(mod_last, "FE, exposición última onda", term = "net_exposure_last_lag",
       vcov. = vcovHC(mod_last, type = "HC1", cluster = "group"))
  results$mod_last <- mod_last
}

cat("[H] Robustez: delta_theta por día transcurrido\n")
reg_df$delta_theta_daily <- reg_df$delta_theta / pmax(reg_df$days_elapsed, 1)
pdata2 <- pdata.frame(reg_df, index = c("legislator", "comm_step"), drop.index = FALSE)
mod_daily <- plm(delta_theta_daily ~ theta_lag + net_exposure_lag, data = pdata2, model = "within")
grab(mod_daily, "FE, delta por día", vcov. = vcovHC(mod_daily, type = "HC1", cluster = "group"))
results$mod_daily <- mod_daily

saveRDS(results, file.path(DATA_PROCESSED, "panel_regression_results.rds"))
tab_df <- do.call(rbind, tab)
dir.create(RESULTS_TABLES, recursive = TRUE, showWarnings = FALSE)
write.csv(tab_df, file.path(RESULTS_TABLES, "M2_panel.csv"), row.names = FALSE)
cat("\n=== Resumen (coeficiente de exposición por modelo) ===\n")
print(tab_df, row.names = FALSE, digits = 4)
cat("--- Done ---\n")

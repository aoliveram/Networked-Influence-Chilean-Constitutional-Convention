# =============================================================================
# 38-m2-regime.R  (comentario del autor 2026-07-20, punto 2, parte 2)
# Re-estima M2 usando el voto revelado del dynIRT ESTIMADO SOLO CON LA ERA
# DOS TERCIOS (37): las ondas de M2 viven casi todas después del 15-feb-2022,
# así que el theta de régimen homogéneo es el insumo conceptualmente correcto.
# Reconstruye theta por (comisión, onda) mapeando la fecha de la onda al último
# período de la era 2/3 <= esa fecha; recomputa exposición acumulada con las
# redes de onda; ajusta FE + falsificación lead. Compara contra el M2 original.
#
# Output: results/tables/M2_regime.csv
# =============================================================================

cat("=== 38-m2-regime.R ===\n")
suppressPackageStartupMessages({ library(jsonlite); library(plm); library(lmtest) })
set.seed(42)
source("code/paths.R")

panel <- read.csv(file.path(DATA_PROCESSED, "network_exposure_panel.csv"), stringsAsFactors = FALSE)
reg2 <- read.csv(file.path(DATA_PROCESSED, "dynirt_regime_dostercios.csv"), stringsAsFactors = FALSE)
roster <- sort(unique(panel$legislator))
n <- length(roster)
fechas2 <- sort(unique(as.Date(reg2$date)))

# theta era-2/3 por legislador x fecha-de-onda (último período <= fecha)
theta_reg_at <- function(fecha) {
  per <- max(which(fechas2 <= fecha))
  sub <- reg2[reg2$date == as.character(fechas2[per]), ]
  setNames(sub$theta, sub$legislator)[roster]
}

expo <- function(W, th) {
  num <- W %*% ifelse(is.na(th), 0, th); den <- rowSums(W)
  out <- as.numeric(num) / den; out[den == 0] <- NA; out
}

rows <- list()
for (k in 1:7) {
  comm <- sprintf("C%d", k)
  waves <- fromJSON(file.path(DATA_PROCESSED, sprintf("C%d_dynamic_networks.json", k)),
                    simplifyDataFrame = TRUE)
  wave_names <- names(waves)
  sub_p <- panel[panel$commission == comm, c("legislator", "step", "emirt_date")]
  fechas_onda <- sapply(seq_along(wave_names) - 1L, function(st) {
    unique(sub_p$emirt_date[sub_p$step == st])[1]
  })
  for (t in seq_along(wave_names)) {
    fecha <- as.Date(fechas_onda[t])
    if (is.na(fecha) || fecha < min(fechas2)) next     # onda pre-era 2/3
    ed <- waves[[wave_names[t]]]
    W <- matrix(0, n, n, dimnames = list(roster, roster))
    if (length(ed) && nrow(ed)) {
      W[cbind(ed$source, ed$target)] <- ed$weight
      W[cbind(ed$target, ed$source)] <- ed$weight
    }
    th <- theta_reg_at(fecha)
    rows[[length(rows) + 1]] <- data.frame(
      legislator = roster, commission = comm, step = t - 1L,
      fecha = as.character(fecha), theta_reg = th,
      E_reg = expo(W, th), stringsAsFactors = FALSE)
  }
}
df <- do.call(rbind, rows)

# panel: delta y lags dentro de (legislador, comisión), pasos consecutivos
df <- df[order(df$legislator, df$commission, df$step), ]
df$key <- paste(df$legislator, df$commission)
df$theta_lag <- ave(df$theta_reg, df$key, FUN = function(x) c(NA, head(x, -1)))
df$E_lag <- ave(df$E_reg, df$key, FUN = function(x) c(NA, head(x, -1)))
df$E_lead <- ave(df$E_reg, df$key, FUN = function(x) c(tail(x, -1), NA))
df$fecha_lag <- ave(df$fecha, df$key, FUN = function(x) c(NA, head(x, -1)))
df$delta <- df$theta_reg - df$theta_lag
# excluir pasos que comparten período de la era 2/3 (delta mecánico = 0)
per_of <- function(f) sapply(as.Date(f), function(x) max(which(fechas2 <= x)))
ok <- !is.na(df$delta) & !is.na(df$E_lag) &
  per_of(df$fecha) != per_of(ifelse(is.na(df$fecha_lag), df$fecha, df$fecha_lag))
cat(sprintf("  Panel régimen 2/3: %d obs válidas (de %d filas)\n", sum(ok), nrow(df)))

fit_fe <- function(data, rhs, label) {
  pd <- pdata.frame(data, index = "legislator")
  m <- plm(as.formula(paste("delta ~ theta_lag +", rhs)), data = pd, model = "within")
  ct <- coeftest(m, vcov = vcovHC(m, method = "arellano", cluster = "group"))
  data.frame(spec = label, term = rhs, estimate = ct[rhs, 1], se = ct[rhs, 2],
             p = ct[rhs, 4], n = nrow(data), row.names = NULL)
}
d_ok <- df[ok, ]
r1 <- fit_fe(d_ok, "E_lag", "FE theta-régimen 2/3")
d_lead <- df[!is.na(df$delta) & !is.na(df$E_lead) &
               per_of(df$fecha) != per_of(ifelse(is.na(df$fecha_lag), df$fecha, df$fecha_lag)), ]
r2 <- fit_fe(d_lead, "E_lead", "FE lead (falsificación), theta-régimen")

orig <- read.csv(file.path(RESULTS_TABLES, "M2_panel.csv"), stringsAsFactors = FALSE)
tab <- rbind(r1, r2)
write.csv(tab, file.path(RESULTS_TABLES, "M2_regime.csv"), row.names = FALSE)
cat("\n--- M2 con theta de régimen homogéneo (era 2/3) ---\n")
print(tab, row.names = FALSE, digits = 3)
cat("\n--- Referencia (theta original, misma especificación FE) ---\n")
print(orig[orig$model %in% c("Efectos fijos"), ], row.names = FALSE, digits = 3)
cat("--- Done ---\n")

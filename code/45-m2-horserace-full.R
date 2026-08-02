# =============================================================================
# 45-m2-horserace-full.R  (ronda polnet26: "la tabla de resultados completa
# del modelo con lambda_lag y lambda_innov")
# El horse race de 38-m2-norms-window.R reportado COMPLETO para ambos
# termómetros (theta estándar y theta era-2/3): beta de reversión, lambda_lag,
# lambda_innov, EE cluster, p, N y R2 within. Construcción idéntica al 38.
#
# Output: results/tables/M2_horserace_full.csv
# =============================================================================

cat("=== 45-m2-horserace-full.R ===\n")
suppressPackageStartupMessages({ library(jsonlite); library(plm); library(lmtest) })
set.seed(42)
source("code/paths.R")
W0 <- as.Date("2022-02-15"); W1 <- as.Date("2022-05-14")

full_fit <- function(d, label) {
  pd <- pdata.frame(d, index = "legislator")
  aux <- plm(E_lead ~ E_lag, data = pd, model = "within")
  d$innov <- as.numeric(residuals(aux))
  m <- plm(delta ~ theta_lag + E_lag + innov, data = pdata.frame(d, index = "legislator"),
           model = "within")
  ct <- coeftest(m, vcov = vcovHC(m, method = "arellano", cluster = "group"))
  r2w <- unname(summary(m)$r.squared["rsq"])
  data.frame(termometro = label,
             beta_theta = ct["theta_lag", 1], se_beta = ct["theta_lag", 2], p_beta = ct["theta_lag", 4],
             lambda_lag = ct["E_lag", 1], se_lag = ct["E_lag", 2], p_lag = ct["E_lag", 4],
             lambda_innov = ct["innov", 1], se_innov = ct["innov", 2], p_innov = ct["innov", 4],
             n = nrow(d), r2_within = r2w, row.names = NULL)
}

# ---------------- (a) theta estandar ----------------
panel <- read.csv(file.path(DATA_PROCESSED, "network_exposure_panel.csv"), stringsAsFactors = FALSE)
panel$fecha <- as.Date(panel$emirt_date)
sub <- panel[panel$fecha >= W0 & panel$fecha <= W1, ]
s2 <- sub[complete.cases(sub[, c("delta_theta", "theta_lag", "net_exposure_lag", "net_exposure_lead")]), ]
s2$delta <- s2$delta_theta; s2$E_lag <- s2$net_exposure_lag; s2$E_lead <- s2$net_exposure_lead
res <- list(full_fit(s2, "theta estandar"))

# ---------------- (b) theta era-2/3 (identico al 38) ----------------
reg2 <- read.csv(file.path(DATA_PROCESSED, "dynirt_regime_dostercios.csv"), stringsAsFactors = FALSE)
roster <- sort(unique(panel$legislator)); n <- length(roster)
fechas2 <- sort(unique(as.Date(reg2$date)))
theta_reg_at <- function(fecha) {
  per <- max(which(fechas2 <= fecha))
  su <- reg2[reg2$date == as.character(fechas2[per]), ]
  setNames(su$theta, su$legislator)[roster]
}
expo <- function(W, th) { num <- W %*% ifelse(is.na(th), 0, th); den <- rowSums(W)
  out <- as.numeric(num) / den; out[den == 0] <- NA; out }
rows <- list()
for (k in 1:7) {
  comm <- sprintf("C%d", k)
  waves <- fromJSON(file.path(DATA_PROCESSED, sprintf("C%d_dynamic_networks.json", k)), simplifyDataFrame = TRUE)
  wn <- names(waves)
  sub_p <- panel[panel$commission == comm, c("legislator", "step", "emirt_date")]
  fechas_onda <- sapply(seq_along(wn) - 1L, function(st) unique(sub_p$emirt_date[sub_p$step == st])[1])
  for (t in seq_along(wn)) {
    fecha <- as.Date(fechas_onda[t])
    if (is.na(fecha) || fecha < min(fechas2)) next
    ed <- waves[[wn[t]]]
    W <- matrix(0, n, n, dimnames = list(roster, roster))
    if (length(ed) && nrow(ed)) { W[cbind(ed$source, ed$target)] <- ed$weight; W[cbind(ed$target, ed$source)] <- ed$weight }
    th <- theta_reg_at(fecha)
    rows[[length(rows) + 1]] <- data.frame(legislator = roster, commission = comm, step = t - 1L,
                                           fecha = as.character(fecha), theta_reg = th, E_reg = expo(W, th))
  }
}
df <- do.call(rbind, rows)
df <- df[order(df$legislator, df$commission, df$step), ]
df$key <- paste(df$legislator, df$commission)
df$theta_lag <- ave(df$theta_reg, df$key, FUN = function(x) c(NA, head(x, -1)))
df$E_lag <- ave(df$E_reg, df$key, FUN = function(x) c(NA, head(x, -1)))
df$E_lead <- ave(df$E_reg, df$key, FUN = function(x) c(tail(x, -1), NA))
df$fecha_lag <- ave(df$fecha, df$key, FUN = function(x) c(NA, head(x, -1)))
df$delta <- df$theta_reg - df$theta_lag
per_of <- function(f) sapply(as.Date(f), function(x) max(which(fechas2 <= x)))
df <- df[as.Date(df$fecha) >= W0 & as.Date(df$fecha) <= W1, ]
ok <- !is.na(df$delta) & !is.na(df$fecha_lag) & per_of(df$fecha) != per_of(df$fecha_lag) &
  !is.na(df$E_lag) & !is.na(df$E_lead)
d2 <- df[ok, ]
res[[2]] <- full_fit(d2, "theta era-2/3")

tab <- do.call(rbind, res)
write.csv(tab, file.path(RESULTS_TABLES, "M2_horserace_full.csv"), row.names = FALSE)
print(tab, row.names = FALSE, digits = 4)
cat("--- Done ---\n")

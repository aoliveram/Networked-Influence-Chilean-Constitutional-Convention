# =============================================================================
# 52-m2-norms-window.R  (comentario del autor 2026-07-23, punto 2)
# Influencia de posiciones SOLO EN LA VENTANA DE NORMAS (15-feb a 14-may-2022,
# la era en que el Pleno votó normas bajo 2/3 — donde theta más se mueve).
# Tres estimaciones dentro de esa ventana, ambas thetas:
#   (a) FE con exposición rezagada (theta dynIRT original)
#   (b) FE con theta de RÉGIMEN HOMOGÉNEO era-2/3 (code/37) — el insumo
#       conceptualmente correcto para esta ventana
#   (c) las falsificaciones: lead, y el test de INNOVACIÓN (la parte del lead
#       no explicada por el lag) — el estándar de la sección 4.1.
#
# Output: results/tables/M2_norms_window.csv + consola
# =============================================================================

cat("=== 52-m2-norms-window.R ===\n")
suppressPackageStartupMessages({ library(jsonlite); library(plm); library(lmtest) })
set.seed(42)
source("code/paths.R")

W0 <- as.Date("2022-02-15"); W1 <- as.Date("2022-05-14")

fit_fe <- function(df, rhs, label) {
  df <- df[complete.cases(df[, c("delta", "theta_lag", rhs)]), ]
  pd <- pdata.frame(df, index = "legislator")
  m <- plm(as.formula(paste("delta ~ theta_lag +", rhs)), data = pd, model = "within")
  ct <- coeftest(m, vcov = vcovHC(m, method = "arellano", cluster = "group"))
  data.frame(spec = label, term = rhs, estimate = ct[rhs, 1], se = ct[rhs, 2],
             p = ct[rhs, 4], n = nrow(df), row.names = NULL)
}

# ---------------- (a) theta original, ventana normas -------------------------
panel <- read.csv(file.path(DATA_PROCESSED, "network_exposure_panel.csv"), stringsAsFactors = FALSE)
panel$fecha <- as.Date(panel$emirt_date)
sub <- panel[panel$fecha >= W0 & panel$fecha <= W1, ]
sub$delta <- sub$delta_theta
res <- list()
res[[1]] <- fit_fe(transform(sub, delta = delta_theta), "net_exposure_lag",  "normas | theta original | lag")
res[[2]] <- fit_fe(transform(sub, delta = delta_theta), "net_exposure_lead", "normas | theta original | lead (falsificación)")
# innovación del lead dentro de la ventana
s2 <- sub[complete.cases(sub[, c("delta_theta", "theta_lag", "net_exposure_lag", "net_exposure_lead")]), ]
pd <- pdata.frame(s2, index = "legislator")
aux <- plm(net_exposure_lead ~ net_exposure_lag, data = pd, model = "within")
s2$innov <- as.numeric(residuals(aux))
m <- plm(delta_theta ~ theta_lag + net_exposure_lag + innov, data = pdata.frame(s2, index = "legislator"), model = "within")
ct <- coeftest(m, vcov = vcovHC(m, method = "arellano", cluster = "group"))
res[[3]] <- data.frame(spec = "normas | theta original | horse race", term = "net_exposure_lag",
                       estimate = ct["net_exposure_lag", 1], se = ct["net_exposure_lag", 2],
                       p = ct["net_exposure_lag", 4], n = nrow(s2))
res[[4]] <- data.frame(spec = "normas | theta original | horse race", term = "innovación lead",
                       estimate = ct["innov", 1], se = ct["innov", 2], p = ct["innov", 4], n = nrow(s2))

# ---------------- (b) theta era-2/3, ventana normas --------------------------
reg2 <- read.csv(file.path(DATA_PROCESSED, "dynirt_regime_dostercios.csv"), stringsAsFactors = FALSE)
roster <- sort(unique(panel$legislator)); n <- length(roster)
fechas2 <- sort(unique(as.Date(reg2$date)))
theta_reg_at <- function(fecha) {
  per <- max(which(fechas2 <= fecha))
  sub <- reg2[reg2$date == as.character(fechas2[per]), ]
  setNames(sub$theta, sub$legislator)[roster]
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
ok <- !is.na(df$delta) & !is.na(df$fecha_lag) & per_of(df$fecha) != per_of(df$fecha_lag)
d_ok <- df[ok, ]
res[[5]] <- fit_fe(d_ok, "E_lag", "normas | theta era-2/3 | lag")
res[[6]] <- fit_fe(d_ok, "E_lead", "normas | theta era-2/3 | lead (falsificación)")
d2 <- d_ok[complete.cases(d_ok[, c("delta", "theta_lag", "E_lag", "E_lead")]), ]
pd <- pdata.frame(d2, index = "legislator")
aux <- plm(E_lead ~ E_lag, data = pd, model = "within")
d2$innov <- as.numeric(residuals(aux))
m <- plm(delta ~ theta_lag + E_lag + innov, data = pdata.frame(d2, index = "legislator"), model = "within")
ct <- coeftest(m, vcov = vcovHC(m, method = "arellano", cluster = "group"))
res[[7]] <- data.frame(spec = "normas | theta era-2/3 | horse race", term = "E_lag",
                       estimate = ct["E_lag", 1], se = ct["E_lag", 2], p = ct["E_lag", 4], n = nrow(d2))
res[[8]] <- data.frame(spec = "normas | theta era-2/3 | horse race", term = "innovación lead",
                       estimate = ct["innov", 1], se = ct["innov", 2], p = ct["innov", 4], n = nrow(d2))

tab <- do.call(rbind, res)
print(tab, row.names = FALSE, digits = 3)
write.csv(tab, file.path(RESULTS_TABLES, "M2_norms_window.csv"), row.names = FALSE)
cat("--- Done ---\n")

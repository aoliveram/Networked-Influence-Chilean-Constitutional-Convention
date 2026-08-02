# =============================================================================
# 46-m2-era23-family.R  (v2, estructura del autor: M0 basico -> +FE fecha ->
# +innovacion; y la correccion de TIMING: como el dynIRT suaviza a dos lados,
# theta_{t+1} carga votos casi-contemporaneos al outcome -> innovacion tambien
# desde t+2; y simetricamente el lag desde t-2 como robustez)
# Todos con theta de regimen homogeneo (era 2/3):
#   M0: delta ~ theta_lag + E_lag                        (basico)
#   M1: M0 + FE de fecha de onda                         (shocks comunes)
#   M2: M1 + innovacion desde t+1                        (horse race estandar)
#   M2b: M1 + innovacion desde t+2                       (timing estricto)
#   R1: M1 con E_lag desde t-2 + innovacion t+2          (ambos lados estrictos)
#   A1 (anexo): exposicion partida propio/otros bloques + innov + FE fecha
#
# Output: results/tables/M2_era23_family.csv
# =============================================================================

cat("=== 46-m2-era23-family.R ===\n")
suppressPackageStartupMessages({ library(jsonlite); library(plm); library(lmtest) })
set.seed(42)
source("code/paths.R")
W0 <- as.Date("2022-02-15"); W1 <- as.Date("2022-05-14")

panel <- read.csv(file.path(DATA_PROCESSED, "network_exposure_panel.csv"), stringsAsFactors = FALSE)
roster <- sort(unique(panel$legislator)); n <- length(roster)
listas <- read.csv(file.path(DATA_RAW, "electoral_lists.csv"), stringsAsFactors = FALSE)
cg <- listas$conglomerado[match(roster, listas$nombre_armonizado)]
bloque <- ifelse(cg == "Vamos por Chile", "Derecha",
          ifelse(cg %in% c("Lista del Apruebo", "Independientes No Neutrales"), "CentroIzq",
          ifelse(cg %in% c("Apruebo Dignidad", "Lista del Pueblo"), "Izquierda",
          ifelse(cg == "Escaños Reservados PPOO", "PPOO", "Otras"))))
same_bloc <- outer(bloque, bloque, `==`)

reg2 <- read.csv(file.path(DATA_PROCESSED, "dynirt_regime_dostercios.csv"), stringsAsFactors = FALSE)
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
  Wprev <- matrix(0, n, n)
  for (t in seq_along(wn)) {
    fecha <- as.Date(fechas_onda[t])
    ed <- waves[[wn[t]]]
    W <- matrix(0, n, n, dimnames = list(roster, roster))
    if (length(ed) && nrow(ed)) { W[cbind(ed$source, ed$target)] <- ed$weight; W[cbind(ed$target, ed$source)] <- ed$weight }
    dW <- W - Wprev; Wprev <- W
    if (is.na(fecha) || fecha < min(fechas2)) next
    th <- theta_reg_at(fecha)
    rows[[length(rows) + 1]] <- data.frame(
      legislator = roster, commission = comm, step = t - 1L, fecha = as.character(fecha),
      theta_reg = th, E_reg = expo(W, th),
      E_own = expo(W * same_bloc, th), E_cross = expo(W * !same_bloc, th),
      actividad = rowSums(dW))
  }
}
df <- do.call(rbind, rows)
df <- df[order(df$legislator, df$commission, df$step), ]
df$key <- paste(df$legislator, df$commission)
lagv <- function(x) ave(x, df$key, FUN = function(z) c(NA, head(z, -1)))
leadv <- function(x) ave(x, df$key, FUN = function(z) c(tail(z, -1), NA))
df$theta_lag <- lagv(df$theta_reg); df$E_lag <- lagv(df$E_reg); df$E_lead <- leadv(df$E_reg)
df$E_own_lag <- lagv(df$E_own); df$E_cross_lag <- lagv(df$E_cross)
df$act_lag <- lagv(df$actividad)
df$fecha_lag <- ave(df$fecha, df$key, FUN = function(x) c(NA, head(x, -1)))
df$delta <- df$theta_reg - df$theta_lag
per_of <- function(f) sapply(as.Date(f), function(x) max(which(fechas2 <= x)))
df <- df[as.Date(df$fecha) >= W0 & as.Date(df$fecha) <= W1, ]
ok <- !is.na(df$delta) & !is.na(df$fecha_lag) & per_of(df$fecha) != per_of(df$fecha_lag)
d <- df[ok, ]

fitm <- function(d, f, label, terms) {
  d <- d[complete.cases(d[, all.vars(f)]), ]
  pd <- pdata.frame(d, index = "legislator")
  m <- plm(f, data = pd, model = "within")
  ct <- coeftest(m, vcov = vcovHC(m, method = "arellano", cluster = "group"))
  out <- lapply(terms, function(tt) if (tt %in% rownames(ct))
    data.frame(modelo = label, term = tt, estimate = ct[tt, 1], se = ct[tt, 2], p = ct[tt, 4]) else NULL)
  out <- do.call(rbind, out)
  out$n <- nrow(d); out$r2w <- unname(summary(m)$r.squared["rsq"])
  out
}

# leads/lags adicionales para el timing estricto
mkvar <- function(x, k) ave(x, d$key, FUN = function(z) {
  if (k > 0) c(tail(z, -k), rep(NA, k)) else c(rep(NA, -k), head(z, k)) })
d$E_lead2 <- mkvar(d$E_reg, 2)     # exposicion en t+2
d$E_lag2 <- mkvar(d$E_reg, -2)    # exposicion en t-2

innovar <- function(d2, leadvar) {
  pd <- pdata.frame(d2, index = "legislator")
  aux <- plm(as.formula(paste(leadvar, "~ E_lag")), data = pd, model = "within")
  as.numeric(residuals(aux))
}
d2 <- d[complete.cases(d[, c("delta", "theta_lag", "E_lag", "E_lead")]), ]
d2$innov <- innovar(d2, "E_lead")
d3 <- d[complete.cases(d[, c("delta", "theta_lag", "E_lag", "E_lead2")]), ]
d3$innov2 <- innovar(d3, "E_lead2")
d4 <- d[complete.cases(d[, c("delta", "theta_lag", "E_lag2", "E_lead2")]), ]
pd4 <- pdata.frame(d4, index = "legislator")
aux4 <- plm(E_lead2 ~ E_lag2, data = pd4, model = "within")
d4$innov2b <- as.numeric(residuals(aux4))
d5 <- d[complete.cases(d[, c("delta", "theta_lag", "E_own_lag", "E_cross_lag", "E_lead")]), ]
d5$innov <- {
  pd5 <- pdata.frame(d5, index = "legislator")
  aux5 <- plm(E_lead ~ E_own_lag + E_cross_lag, data = pd5, model = "within")
  as.numeric(residuals(aux5)) }

res <- list(
  fitm(d,  delta ~ theta_lag + E_lag, "M0 basico", c("theta_lag", "E_lag")),
  fitm(d,  delta ~ theta_lag + E_lag + factor(fecha), "M1 + FE fecha", c("theta_lag", "E_lag")),
  fitm(d2, delta ~ theta_lag + E_lag + innov + factor(fecha), "M2 + innovacion t+1",
       c("theta_lag", "E_lag", "innov")),
  fitm(d3, delta ~ theta_lag + E_lag + innov2 + factor(fecha), "M2b innovacion t+2",
       c("theta_lag", "E_lag", "innov2")),
  fitm(d4, delta ~ theta_lag + E_lag2 + innov2b + factor(fecha), "R1 lag t-2 + innov t+2",
       c("theta_lag", "E_lag2", "innov2b")),
  fitm(d5, delta ~ theta_lag + E_own_lag + E_cross_lag + innov + factor(fecha),
       "A1 anexo exposicion partida", c("theta_lag", "E_own_lag", "E_cross_lag", "innov")))
tab <- do.call(rbind, res)
write.csv(tab, file.path(RESULTS_TABLES, "M2_era23_family.csv"), row.names = FALSE)
print(tab, row.names = FALSE, digits = 4)
cat("--- Done ---\n")

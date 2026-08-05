# =============================================================================
# 46-m2-era23-family.R  (v3, especificacion FINAL del autor)
# Todo con theta de regimen homogeneo (era 2/3) y SOLO datos de la era.
# El reloj de las exposiciones (argumento del autor): con los votos de t se
# calcula la posicion/exposicion de t+1. Para explicar el voto decidido en t
# habria que usar al menos los votos de t-1 (= exposicion en t); como la
# exposicion contemporanea es contraintuitiva como explicacion, usamos los
# votos de t-2 (= exposicion en t-1). Y para que la innovacion sea
# genuinamente futura, no basta t+1 (construida con votos de t): se usa t+2.
#   M0: delta ~ theta_lag + E_{t-1}                        (basico)
#   M1: M0 + FE de fecha de onda                           (shocks comunes)
#   M2: M1 + innovacion desde t+2                          (el test del arbitro)
#   D025/D050/D075: M1 con exposicion de DECAIMIENTO (era) (memoria realista)
#   C (solo chat): lag = E_t (contemporanea), innov = t+2
#   A1 (anexo): exposicion partida propio/otros bloques
#
# Output: results/tables/M2_era23_family.csv
# =============================================================================

cat("=== 46-m2-era23-family.R (v3) ===\n")
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
  Wd25 <- matrix(0, n, n); Wd50 <- matrix(0, n, n); Wd75 <- matrix(0, n, n)
  for (t in seq_along(wn)) {
    fecha <- as.Date(fechas_onda[t])
    ed <- waves[[wn[t]]]
    W <- matrix(0, n, n, dimnames = list(roster, roster))
    if (length(ed) && nrow(ed)) { W[cbind(ed$source, ed$target)] <- ed$weight; W[cbind(ed$target, ed$source)] <- ed$weight }
    dW <- W - Wprev; Wprev <- W
    Wd25 <- 0.25 * Wd25 + dW; Wd50 <- 0.50 * Wd50 + dW; Wd75 <- 0.75 * Wd75 + dW
    if (is.na(fecha) || fecha < min(fechas2)) next
    th <- theta_reg_at(fecha)
    rows[[length(rows) + 1]] <- data.frame(
      legislator = roster, commission = comm, step = t - 1L, fecha = as.character(fecha),
      theta_reg = th, E_reg = expo(W, th),
      E_d25 = expo(Wd25, th), E_d50 = expo(Wd50, th), E_d75 = expo(Wd75, th),
      E_own = expo(W * same_bloc, th), E_cross = expo(W * !same_bloc, th))
  }
}
df <- do.call(rbind, rows)
df <- df[order(df$legislator, df$commission, df$step), ]
df$key <- paste(df$legislator, df$commission)
shift <- function(x, k) ave(x, df$key, FUN = function(z) {
  if (k > 0) c(tail(z, -k), rep(NA, k)) else if (k < 0) c(rep(NA, -k), head(z, k)) else z })
df$theta_lag <- shift(df$theta_reg, -1)
df$E_lag <- shift(df$E_reg, -1)
df$E_lead2 <- shift(df$E_reg, 2)
df$E_d25_lag <- shift(df$E_d25, -1); df$E_d50_lag <- shift(df$E_d50, -1); df$E_d75_lag <- shift(df$E_d75, -1)
df$Ed25_lead2 <- shift(df$E_d25, 2); df$Ed50_lead2 <- shift(df$E_d50, 2); df$Ed75_lead2 <- shift(df$E_d75, 2)
df$E_own_lag <- shift(df$E_own, -1); df$E_cross_lag <- shift(df$E_cross, -1)
df$fecha_lag <- ave(df$fecha, df$key, FUN = function(x) c(NA, head(x, -1)))
df$delta <- df$theta_reg - df$theta_lag
per_of <- function(f) sapply(as.Date(f), function(x) max(which(fechas2 <= x)))
df <- df[as.Date(df$fecha) >= W0 & as.Date(df$fecha) <= W1, ]
ok <- !is.na(df$delta) & !is.na(df$fecha_lag) & per_of(df$fecha) != per_of(df$fecha_lag)
d <- df[ok, ]

fitm <- function(dd, f, label, terms) {
  dd <- dd[complete.cases(dd[, all.vars(f)]), ]
  pd <- pdata.frame(dd, index = "legislator")
  m <- plm(f, data = pd, model = "within")
  ct <- coeftest(m, vcov = vcovHC(m, method = "arellano", cluster = "group"))
  out <- do.call(rbind, lapply(terms, function(tt) if (tt %in% rownames(ct))
    data.frame(modelo = label, term = tt, estimate = ct[tt, 1], se = ct[tt, 2], p = ct[tt, 4]) else NULL))
  out$n <- nrow(dd); out$r2w <- unname(summary(m)$r.squared["rsq"])
  out
}
innov_of <- function(dd, leadvar, lagvar) {
  pd <- pdata.frame(dd, index = "legislator")
  aux <- plm(as.formula(paste(leadvar, "~", lagvar)), data = pd, model = "within")
  as.numeric(residuals(aux))
}

d2 <- d[complete.cases(d[, c("delta", "theta_lag", "E_lag", "E_lead2")]), ]
d2$innov <- innov_of(d2, "E_lead2", "E_lag")
dC <- d[complete.cases(d[, c("delta", "theta_lag", "E_reg", "E_lead2")]), ]
dC$innovC <- innov_of(dC, "E_lead2", "E_reg")
d5 <- d[complete.cases(d[, c("delta", "theta_lag", "E_own_lag", "E_cross_lag", "E_lead2")]), ]
d5$innov <- {
  pd5 <- pdata.frame(d5, index = "legislator")
  aux5 <- plm(E_lead2 ~ E_own_lag + E_cross_lag, data = pd5, model = "within")
  as.numeric(residuals(aux5)) }

hr_dec <- function(lagvar, leadvar, label) {
  dd <- d[complete.cases(d[, c("delta", "theta_lag", lagvar, leadvar)]), ]
  pd <- pdata.frame(dd, index = "legislator")
  aux <- plm(as.formula(paste(leadvar, "~", lagvar)), data = pd, model = "within")
  dd$innovd <- as.numeric(residuals(aux))
  fitm(dd, as.formula(paste("delta ~ theta_lag +", lagvar, "+ innovd + factor(fecha)")),
       label, c("theta_lag", lagvar, "innovd"))
}

res <- list(
  fitm(d,  delta ~ theta_lag + E_lag, "M0 basico", c("theta_lag", "E_lag")),
  fitm(d,  delta ~ theta_lag + E_lag + factor(fecha), "M1 + FE fecha", c("theta_lag", "E_lag")),
  fitm(d2, delta ~ theta_lag + E_lag + innov + factor(fecha), "M2 + innovacion t+2",
       c("theta_lag", "E_lag", "innov")),
  fitm(d,  delta ~ theta_lag + E_d25_lag + factor(fecha), "D025 decaimiento", c("theta_lag", "E_d25_lag")),
  fitm(d,  delta ~ theta_lag + E_d50_lag + factor(fecha), "D050 decaimiento", c("theta_lag", "E_d50_lag")),
  fitm(d,  delta ~ theta_lag + E_d75_lag + factor(fecha), "D075 decaimiento", c("theta_lag", "E_d75_lag")),
  fitm(dC, delta ~ theta_lag + E_reg + innovC + factor(fecha), "C chat: lag contemporaneo",
       c("theta_lag", "E_reg", "innovC")),
  fitm(d5, delta ~ theta_lag + E_own_lag + E_cross_lag + innov + factor(fecha),
       "A1 anexo exposicion partida", c("theta_lag", "E_own_lag", "E_cross_lag", "innov")),
  hr_dec("E_d25_lag", "Ed25_lead2", "H025 decaimiento+HR"),
  hr_dec("E_d50_lag", "Ed50_lead2", "H050 decaimiento+HR"),
  hr_dec("E_d75_lag", "Ed75_lead2", "H075 decaimiento+HR"))
tab <- do.call(rbind, res)
write.csv(tab, file.path(RESULTS_TABLES, "M2_era23_family.csv"), row.names = FALSE)
print(tab, row.names = FALSE, digits = 4)
cat("--- Done ---\n")

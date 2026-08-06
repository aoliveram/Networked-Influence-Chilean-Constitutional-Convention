# =============================================================================
# 46-m2-era23-family.R  (v4, familia OFICIAL de RQ2a)
# Exposicion con DECAIMIENTO parametrizada por MEMORIA en ondas:
#   m = 1, 2, 3 ondas; lambda_m = 0.1^(1/m) (el 90% del peso cae dentro de
#   las ultimas m ondas). Nada acumulado: la exposicion tiene cota temporal.
# Reloj estricto (argumento del autor): con los votos de t se calcula la
# posicion/exposicion de t+1 -> rezago en t-1 (votos hasta t-2) e innovacion
# desde t+2. La innovacion tambien es decaida: promedio ponderado de las
# exposiciones futuras E^m_u (u >= t+2) con pesos lambda_m^(u-(t+2)) — el
# futuro CERCANO pesa mas (espejo del decaimiento hacia atras).
# Escalera, para cada memoria m:
#   M0: delta ~ theta_lag + E^m_{t-1}
#   M1: M0 + FE de fecha de onda
#   M2: M1 + innovacion decaida desde t+2 (el test del arbitro)
# Todo con theta de regimen homogeneo (era 2/3) y solo datos de la era.
#
# Output: results/tables/M2_era23_family.csv
# =============================================================================

cat("=== 46-m2-era23-family.R (v4: decay-memoria) ===\n")
suppressPackageStartupMessages({ library(jsonlite); library(plm); library(lmtest) })
set.seed(42)
source("code/paths.R")
W0 <- as.Date("2022-02-15"); W1 <- as.Date("2022-05-14")

panel <- read.csv(file.path(DATA_PROCESSED, "network_exposure_panel.csv"), stringsAsFactors = FALSE)
roster <- sort(unique(panel$legislator)); n <- length(roster)
reg2 <- read.csv(file.path(DATA_PROCESSED, "dynirt_regime_dostercios.csv"), stringsAsFactors = FALSE)
fechas2 <- sort(unique(as.Date(reg2$date)))
theta_reg_at <- function(fecha) {
  per <- max(which(fechas2 <= fecha))
  su <- reg2[reg2$date == as.character(fechas2[per]), ]
  setNames(su$theta, su$legislator)[roster]
}
expo <- function(W, th) { num <- W %*% ifelse(is.na(th), 0, th); den <- rowSums(W)
  out <- as.numeric(num) / den; out[den == 0] <- NA; out }

MS <- c(1, 2, 3)
LAM <- setNames(0.1^(1 / MS), paste0("m", MS))

rows <- list()
for (k in 1:7) {
  comm <- sprintf("C%d", k)
  waves <- fromJSON(file.path(DATA_PROCESSED, sprintf("C%d_dynamic_networks.json", k)), simplifyDataFrame = TRUE)
  wn <- names(waves)
  sub_p <- panel[panel$commission == comm, c("legislator", "step", "emirt_date")]
  fechas_onda <- sapply(seq_along(wn) - 1L, function(st) unique(sub_p$emirt_date[sub_p$step == st])[1])
  Wprev <- matrix(0, n, n)
  Wd <- lapply(LAM, function(l) matrix(0, n, n))
  for (t in seq_along(wn)) {
    fecha <- as.Date(fechas_onda[t])
    ed <- waves[[wn[t]]]
    W <- matrix(0, n, n, dimnames = list(roster, roster))
    if (length(ed) && nrow(ed)) { W[cbind(ed$source, ed$target)] <- ed$weight; W[cbind(ed$target, ed$source)] <- ed$weight }
    dW <- W - Wprev; Wprev <- W
    for (j in seq_along(LAM)) Wd[[j]] <- LAM[j] * Wd[[j]] + dW
    if (is.na(fecha) || fecha < min(fechas2)) next
    th <- theta_reg_at(fecha)
    r <- data.frame(legislator = roster, commission = comm, step = t - 1L,
                    fecha = as.character(fecha), theta_reg = th)
    for (j in seq_along(LAM)) r[[paste0("E_", names(LAM)[j])]] <- expo(Wd[[j]], th)
    rows[[length(rows) + 1]] <- r
  }
}
df <- do.call(rbind, rows)
df <- df[order(df$legislator, df$commission, df$step), ]
df$key <- paste(df$legislator, df$commission)
shift <- function(x, k) ave(x, df$key, FUN = function(z) {
  if (k > 0) c(tail(z, -k), rep(NA, k)) else if (k < 0) c(rep(NA, -k), head(z, k)) else z })
df$theta_lag <- shift(df$theta_reg, -1)
df$fecha_lag <- ave(df$fecha, df$key, FUN = function(x) c(NA, head(x, -1)))
df$delta <- df$theta_reg - df$theta_lag
for (mm in names(LAM)) df[[paste0("Elag_", mm)]] <- shift(df[[paste0("E_", mm)]], -1)

# innovacion decaida (cercana): promedio ponderado de E^m_u, u >= t+2
for (mm in names(LAM)) {
  lam <- LAM[mm]
  Ecol <- df[[paste0("E_", mm)]]
  fa <- rep(NA_real_, nrow(df))
  for (idx in split(seq_len(nrow(df)), df$key)) {
    Tn <- length(idx); Ev <- Ecol[idx]
    for (t in seq_len(Tn)) {
      if (t + 2 > Tn) next
      us <- (t + 2):Tn
      ev <- Ev[us]; okv <- !is.na(ev)
      if (!any(okv)) next
      wa <- lam^(us - (t + 2))
      fa[idx[t]] <- sum(wa[okv] * ev[okv]) / sum(wa[okv])
    }
  }
  df[[paste0("Ffut_", mm)]] <- fa
}

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

res <- list()
for (mm in names(LAM)) {
  lagv <- paste0("Elag_", mm); futv <- paste0("Ffut_", mm)
  res[[length(res) + 1]] <- fitm(d, as.formula(paste("delta ~ theta_lag +", lagv)),
                                 paste0("M0 ", mm), c("theta_lag", lagv))
  res[[length(res) + 1]] <- fitm(d, as.formula(paste("delta ~ theta_lag +", lagv, "+ factor(fecha)")),
                                 paste0("M1 ", mm), c("theta_lag", lagv))
  dd <- d[complete.cases(d[, c("delta", "theta_lag", lagv, futv)]), ]
  pd <- pdata.frame(dd, index = "legislator")
  aux <- plm(as.formula(paste(futv, "~", lagv)), data = pd, model = "within")
  dd$innovd <- as.numeric(residuals(aux))
  res[[length(res) + 1]] <- fitm(dd, as.formula(paste("delta ~ theta_lag +", lagv, "+ innovd + factor(fecha)")),
                                 paste0("M2 ", mm), c("theta_lag", lagv, "innovd"))
}
tab <- do.call(rbind, res)
write.csv(tab, file.path(RESULTS_TABLES, "M2_era23_family.csv"), row.names = FALSE)
print(tab, row.names = FALSE, digits = 4)
cat("--- Done ---\n")

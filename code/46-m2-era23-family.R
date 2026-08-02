# =============================================================================
# 46-m2-era23-family.R  (ronda polnet26: elevar la familia era-2/3 como los
# modelos principales de RQ2a — de lo básico al horse race, más un modelo
# extendido con variables de la historia interna de la CC)
# Todos con theta de régimen homogéneo (era 2/3). Modelos:
#   M0: delta ~ theta_lag + E_lag                       (básico)
#   M1: delta ~ theta_lag + E_lead                      (falsificación)
#   M2: delta ~ theta_lag + E_lag + innov               (horse race, = Tabla 9e)
#   M3: M2 + FE de fecha de onda + actividad rezagada   (shocks comunes + carga)
#   M4: theta_lag + E_own_lag + E_cross_lag + innov + FE fecha + actividad
#       (la exposición partida: ¿tira el propio bloque o el resto?)
# Variables nuevas:
#   - FE de fecha (mu_t): absorbe los shocks comunes de la era (plebiscito,
#     acuerdos transversales de abril-mayo).
#   - actividad_lag: co-firmas NUEVAS de i en la onda anterior (rowSums(dW)).
#   - E_own / E_cross: exposición a co-firmantes del MISMO bloque vs de OTROS
#     bloques (los 5 bloques del 44-clogit... hoy 31-clogit-by-bloc.R).
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

# innovacion (misma construccion del 45) sobre la muestra con lead
d2 <- d[complete.cases(d[, c("delta", "theta_lag", "E_lag", "E_lead")]), ]
pd <- pdata.frame(d2, index = "legislator")
aux <- plm(E_lead ~ E_lag, data = pd, model = "within")
d2$innov <- as.numeric(residuals(aux))

res <- list(
  fitm(d,  delta ~ theta_lag + E_lag, "M0 basico", c("theta_lag", "E_lag")),
  fitm(d,  delta ~ theta_lag + E_lead, "M1 falsificacion", c("theta_lag", "E_lead")),
  fitm(d2, delta ~ theta_lag + E_lag + innov, "M2 horse race", c("theta_lag", "E_lag", "innov")),
  fitm(d2, delta ~ theta_lag + E_lag + innov + factor(fecha) + act_lag, "M3 + FE fecha + actividad",
       c("theta_lag", "E_lag", "innov", "act_lag")),
  fitm(d2, delta ~ theta_lag + E_own_lag + E_cross_lag + innov + factor(fecha) + act_lag,
       "M4 exposicion partida", c("theta_lag", "E_own_lag", "E_cross_lag", "innov", "act_lag")))
tab <- do.call(rbind, res)
write.csv(tab, file.path(RESULTS_TABLES, "M2_era23_family.csv"), row.names = FALSE)
print(tab, row.names = FALSE, digits = 4)
cat("--- Done ---\n")

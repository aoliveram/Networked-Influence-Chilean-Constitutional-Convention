# =============================================================================
# 39-early-influence-imputed.R  (comentario del autor 2026-07-20, punto 6)
# EJERCICIO EXPLORATORIO ("solo para saberlo entre nosotros"): ¿hay señal de
# influencia en los PRIMEROS MESES de votación (jul-oct 2021), donde la red de
# co-firma aún no existía? IMPUTAMOS como red de esos meses la primera red
# disponible: la red génesis completa (co-firmas nov-2021..feb-2022), fija.
# Supuesto heroico explícito: los lazos de noviembre ya existían socialmente
# en julio. Si aun ASÍ no aparece influencia, el nulo de M2 queda más cómodo;
# si aparece, NO es evidencia (la red es futura respecto del período), solo
# motivo para buscar una medida temprana de red. NO entra al reporte como
# resultado — es un chequeo interno.
#
# theta: dynIRT de la ERA MAYORÍA (code/37), régimen homogéneo, períodos =
# fechas de votación jul-2021..14-feb-2022.
# Modelo: delta_theta_ip = alpha_i + b theta_{i,p-1} + lambda E_{i,p-1} + e,
# within por convencional, SE cluster convencional; falsificación con E_{p+1}.
#
# Output: solo consola (exploratorio).
# =============================================================================

cat("=== 39-early-influence-imputed.R (EXPLORATORIO) ===\n")
suppressPackageStartupMessages({ library(plm); library(lmtest) })
set.seed(42)
source("code/paths.R")

reg1 <- read.csv(file.path(DATA_PROCESSED, "dynirt_regime_mayoria.csv"), stringsAsFactors = FALSE)
edges <- read.csv(file.path(DATA_PROCESSED, "genesis_network_initiative.csv"), stringsAsFactors = FALSE)
roster <- sort(unique(reg1$legislator))
n <- length(roster)
W <- matrix(0, n, n, dimnames = list(roster, roster))
ok <- edges$source %in% roster & edges$target %in% roster
W[cbind(edges$source[ok], edges$target[ok])] <- edges$weight[ok]
W[cbind(edges$target[ok], edges$source[ok])] <- edges$weight[ok]

fechas <- sort(unique(as.Date(reg1$date)))
TH <- sapply(fechas, function(f) {
  sub <- reg1[reg1$date == as.character(f), ]
  setNames(sub$theta, sub$legislator)[roster]
})                                            # n x P (todos sirven todo el período)
expo <- function(th) { num <- W %*% th; den <- rowSums(W); out <- num / den; out[den == 0] <- NA; as.numeric(out) }
E <- apply(TH, 2, expo)                       # n x P, red FIJA imputada

P <- length(fechas)
long <- do.call(rbind, lapply(2:P, function(p) data.frame(
  legislator = roster, fecha = fechas[p],
  delta = TH[, p] - TH[, p - 1], theta_lag = TH[, p - 1],
  E_lag = E[, p - 1], E_lead = if (p < P) E[, p + 1] else NA_real_)))

fit <- function(df, rhs, label) {
  df <- df[!is.na(df$delta) & !is.na(df$theta_lag) & !is.na(df[[rhs]]), ]
  pd <- pdata.frame(df, index = "legislator")
  m <- plm(as.formula(paste("delta ~ theta_lag +", rhs)), data = pd, model = "within")
  ct <- coeftest(m, vcov = vcovHC(m, method = "arellano", cluster = "group"))
  cat(sprintf("  %-34s %s = %+.4f (EE %.4f, p = %.3f), N = %d, %d fechas\n",
              label, rhs, ct[rhs, 1], ct[rhs, 2], ct[rhs, 4], nrow(df),
              length(unique(df$fecha))))
}

cat("\n--- Ventana PRIMER MES largo (jul-ago 2021, ventana Fábrega) ---\n")
w1 <- long[long$fecha <= as.Date("2021-08-31"), ]
fit(w1, "E_lag", "influencia (red imputada)")
fit(w1, "E_lead", "falsificación (exposición futura)")

cat("\n--- Ventana PRE-RED completa (jul-oct 2021) ---\n")
w2 <- long[long$fecha <= as.Date("2021-10-31"), ]
fit(w2, "E_lag", "influencia (red imputada)")
fit(w2, "E_lead", "falsificación (exposición futura)")

cat("\n--- Referencia: era mayoría completa (jul-21..feb-22, red imputada) ---\n")
fit(long, "E_lag", "influencia (red imputada)")
fit(long, "E_lead", "falsificación (exposición futura)")
cat("--- Done ---\n")

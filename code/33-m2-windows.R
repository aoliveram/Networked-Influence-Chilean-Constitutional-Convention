# =============================================================================
# 33-m2-windows.R  (comentarios del autor 2026-07-18, puntos 11 y 12)
#
# (11) VENTANAS TEMPORALES: ¿la influencia pudo operar temprano (sobre los
#      "nuevos susceptibles") y agotarse? La ventana de Fábrega (jul-ago 2021)
#      no es estimable: la red de co-firma NO EXISTÍA (primeras iniciativas
#      nov-2021; primeros informes génesis ene-feb 2022). Las ventanas
#      factibles cortan la era de colaboración activa en dos:
#        TEMPRANA: ondas con fecha <= 2022-03-31 (los primeros ~2 meses de
#                  colaboración, donde operaría la influencia sobre novatos)
#        TARDÍA:   ondas con fecha >= 2022-04-01
#      En cada ventana: FE + falsificación con lead.
#
# (12) ANCHO DE LA EXPOSICIÓN ("carga virulenta"): la exposición principal es
#      acumulada desde T0; aquí se re-estima con ventanas móviles de k ondas
#      (k = 1, 2, 3, acumulada) y con decaimiento exponencial
#      lambda in {0.25, 0.5, 0.75}:
#        E_it = sum_j w_ij(ventana) * theta_jt / sum_j w_ij(ventana)
#      donde w_ij(k) usa solo la co-firma agregada en las últimas k ondas y
#      w_ij(lambda) = sum_s lambda^(t-s) * Delta w_ij,s.
#
# Modelo en todos los casos (within/FE por delegado, SE cluster delegado):
#   delta_theta_it = alpha_i + b1 theta_{i,t-1} + lambda_exp E_{i,t-1} + e_it
#
# Output: results/tables/M2_windows.csv
# =============================================================================

cat("=== 33-m2-windows.R ===\n")
suppressPackageStartupMessages({ library(jsonlite); library(plm); library(lmtest) })
set.seed(42)
source("code/paths.R")

panel <- read.csv(file.path(DATA_PROCESSED, "network_exposure_panel.csv"), stringsAsFactors = FALSE)
roster <- sort(unique(panel$legislator))
n <- length(roster)

# ---------------- exposiciones por definición de ventana ---------------------
expo <- function(W, th) {
  num <- W %*% ifelse(is.na(th), 0, th); den <- rowSums(W)
  out <- as.numeric(num) / den; out[den == 0] <- NA; out
}

defs <- c("acumulada", "k1", "k2", "k3", "dec025", "dec050", "dec075")
E_rows <- list()
for (k in 1:7) {
  comm <- sprintf("C%d", k)
  waves <- fromJSON(file.path(DATA_PROCESSED, sprintf("C%d_dynamic_networks.json", k)),
                    simplifyDataFrame = TRUE)
  Wc <- lapply(names(waves), function(wn) {
    ed <- waves[[wn]]
    W <- matrix(0, n, n, dimnames = list(roster, roster))
    if (length(ed) && nrow(ed)) {
      W[cbind(ed$source, ed$target)] <- ed$weight
      W[cbind(ed$target, ed$source)] <- ed$weight
    }
    W
  })
  Tn <- length(Wc)
  dW <- lapply(seq_len(Tn), function(t) if (t == 1) Wc[[1]] else Wc[[t]] - Wc[[t - 1]])
  th_by_step <- lapply(seq_len(Tn) - 1L, function(st) {
    sub <- panel[panel$commission == comm & panel$step == st, ]
    setNames(sub$theta, sub$legislator)[roster]
  })
  for (t in seq_len(Tn)) {
    th <- th_by_step[[t]]
    Wk <- list(
      acumulada = Wc[[t]],
      k1 = dW[[t]],
      k2 = if (t >= 2) dW[[t]] + dW[[t - 1]] else dW[[t]],
      k3 = if (t >= 3) dW[[t]] + dW[[t - 1]] + dW[[t - 2]] else Wc[[t]],
      dec025 = Reduce(`+`, lapply(seq_len(t), function(s) 0.25^(t - s) * dW[[s]])),
      dec050 = Reduce(`+`, lapply(seq_len(t), function(s) 0.50^(t - s) * dW[[s]])),
      dec075 = Reduce(`+`, lapply(seq_len(t), function(s) 0.75^(t - s) * dW[[s]]))
    )
    for (d in defs) {
      E_rows[[length(E_rows) + 1]] <- data.frame(
        legislator = roster, commission = comm, step = t - 1L, def = d,
        E = expo(Wk[[d]], th), stringsAsFactors = FALSE)
    }
  }
}
Edf <- do.call(rbind, E_rows)
# lag dentro de comisión: E de la onda t-1 se usa para el delta de la onda t
Edf$step_next <- Edf$step + 1L
panel2 <- merge(panel, Edf[, c("legislator", "commission", "step_next", "def", "E")],
                by.x = c("legislator", "commission", "step"),
                by.y = c("legislator", "commission", "step_next"))
names(panel2)[names(panel2) == "E"] <- "E_lag"

# sanity: la definición acumulada debe reproducir el net_exposure_lag del 03
chk <- panel2[panel2$def == "acumulada" & !is.na(panel2$net_exposure_lag) & !is.na(panel2$E_lag), ]
cat(sprintf("  Sanity acumulada vs 03: cor = %.4f (max|dif| = %.4f) en %d filas\n",
            cor(chk$E_lag, chk$net_exposure_lag), max(abs(chk$E_lag - chk$net_exposure_lag)),
            nrow(chk)))

# ---------------- estimador FE con cluster por delegado ----------------------
fit_fe <- function(df, rhs = "E_lag", label) {
  df <- df[!is.na(df$delta_theta) & !is.na(df$theta_lag) & !is.na(df[[rhs]]), ]
  if (nrow(df) < 200) return(NULL)
  pd <- pdata.frame(df, index = c("legislator"))
  f <- as.formula(paste("delta_theta ~ theta_lag +", rhs))
  m <- plm(f, data = pd, model = "within")
  ct <- coeftest(m, vcov = vcovHC(m, method = "arellano", cluster = "group"))
  data.frame(spec = label, term = rhs, estimate = ct[rhs, 1], se = ct[rhs, 2],
             p = ct[rhs, 4], n = nrow(df), row.names = NULL)
}

panel2$fecha <- as.Date(panel2$emirt_date)
VENTANAS <- list(completa = rep(TRUE, nrow(panel2)),
                 temprana = panel2$fecha <= as.Date("2022-03-31"),
                 tardia = panel2$fecha >= as.Date("2022-04-01"))

res <- list()
for (v in names(VENTANAS)) {
  for (d in defs) {
    sub <- panel2[VENTANAS[[v]] & panel2$def == d, ]
    r <- fit_fe(sub, "E_lag", sprintf("%s | %s", v, d))
    if (!is.null(r)) res[[length(res) + 1]] <- r
  }
  # falsificación lead (exposición acumulada) por ventana
  sub <- panel2[VENTANAS[[v]] & panel2$def == "acumulada", ]
  r <- fit_fe(sub, "net_exposure_lead", sprintf("%s | lead (falsificación)", v))
  if (!is.null(r)) res[[length(res) + 1]] <- r
}
tab <- do.call(rbind, res)
dir.create(RESULTS_TABLES, recursive = TRUE, showWarnings = FALSE)
write.csv(tab, file.path(RESULTS_TABLES, "M2_windows.csv"), row.names = FALSE)
cat("\n--- FE por ventana temporal x definición de exposición ---\n")
print(tab, row.names = FALSE, digits = 3)
cat("--- Done ---\n")

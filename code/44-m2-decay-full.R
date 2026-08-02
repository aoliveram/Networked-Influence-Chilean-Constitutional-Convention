# =============================================================================
# 44-m2-decay-full.R  (comentario del autor post-PoliCICS, punto 6; y ronda
# polnet26: completar la tabla de decaimiento con TODAS las covariables)
# Los tres modelos FE de exposición con decaimiento (lambda_w = 0.25/0.50/0.75,
# ventana completa), reportados completos: beta de reversión (theta_lag),
# lambda de exposición (E_lag), EE cluster, p, N y R2 within.
# Construcción de exposiciones idéntica a code/25-m2-windows.R.
#
# Output: results/tables/M2_decay_full.csv
# =============================================================================

cat("=== 44-m2-decay-full.R ===\n")
suppressPackageStartupMessages({ library(jsonlite); library(plm); library(lmtest) })
set.seed(42)
source("code/paths.R")

panel <- read.csv(file.path(DATA_PROCESSED, "network_exposure_panel.csv"), stringsAsFactors = FALSE)
roster <- sort(unique(panel$legislator)); n <- length(roster)
expo <- function(W, th) {
  num <- W %*% ifelse(is.na(th), 0, th); den <- rowSums(W)
  out <- as.numeric(num) / den; out[den == 0] <- NA; out
}
defs <- c("dec025", "dec050", "dec075")
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
  for (t in seq_len(Tn)) {
    sub <- panel[panel$commission == comm & panel$step == t - 1L, ]
    th <- setNames(sub$theta, sub$legislator)[roster]
    for (lam in c(0.25, 0.50, 0.75)) {
      Wl <- Reduce(`+`, lapply(seq_len(t), function(s) lam^(t - s) * dW[[s]]))
      E_rows[[length(E_rows) + 1]] <- data.frame(
        legislator = roster, commission = comm, step = t - 1L,
        def = sprintf("dec%03d", lam * 100), E = expo(Wl, th), stringsAsFactors = FALSE)
    }
  }
}
Edf <- do.call(rbind, E_rows)
Edf$step_next <- Edf$step + 1L
panel2 <- merge(panel, Edf[, c("legislator", "commission", "step_next", "def", "E")],
                by.x = c("legislator", "commission", "step"),
                by.y = c("legislator", "commission", "step_next"))
names(panel2)[names(panel2) == "E"] <- "E_lag"

res <- list()
for (d in defs) {
  df <- panel2[panel2$def == d, ]
  df <- df[!is.na(df$delta_theta) & !is.na(df$theta_lag) & !is.na(df$E_lag), ]
  pd <- pdata.frame(df, index = c("legislator"))
  m <- plm(delta_theta ~ theta_lag + E_lag, data = pd, model = "within")
  ct <- coeftest(m, vcov = vcovHC(m, method = "arellano", cluster = "group"))
  r2w <- summary(m)$r.squared["rsq"]
  res[[length(res) + 1]] <- data.frame(
    modelo = d,
    beta_theta_lag = ct["theta_lag", 1], se_beta = ct["theta_lag", 2], p_beta = ct["theta_lag", 4],
    lambda_E = ct["E_lag", 1], se_lambda = ct["E_lag", 2], p_lambda = ct["E_lag", 4],
    n = nrow(df), r2_within = unname(r2w), row.names = NULL)
}
tab <- do.call(rbind, res)
write.csv(tab, file.path(RESULTS_TABLES, "M2_decay_full.csv"), row.names = FALSE)
print(tab, row.names = FALSE, digits = 4)
cat("--- Done ---\n")

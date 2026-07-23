# =============================================================================
# 53-norms-influence-robustness.R  (comentario del autor 2026-07-23, punto 1)
# BLINDAJE del hallazgo del 52: con theta ERA-2/3, el lag sobrevive el horse
# race (lag +0.016 p=.007 junto a innovación +0.053 p=.014). Dos robusteces:
#   (A) ERROR DE MEDICIÓN: bootstrap paramétrico del dynIRT era-2/3 (B = 50:
#       re-simular los votos de la era, re-ajustar, reconstruir panel + horse
#       race por réplica) -> ¿el lag sobrevive con EE total (Rubin)?
#   (B) SUB-VENTANAS: el horse race era-2/3 en feb15-mar31 y abr1-may14.
#
# Output: results/tables/M2_norms_robustness.csv + consola
# =============================================================================

cat("=== 53-norms-influence-robustness.R ===\n")
suppressPackageStartupMessages({
  library(emIRT); library(jsonlite); library(plm); library(lmtest); library(parallel)
})
set.seed(42)
source("code/paths.R")
T0 <- Sys.time()
B <- 50
CORTE <- as.Date("2022-02-15"); W1 <- as.Date("2022-05-14")

# ---------------- ajuste era-2/3 (como 37) -----------------------------------
inp <- readRDS(file.path(EMIRT_DIR, "emIRT_data_input.rds"))
meta <- readRDS(file.path(EMIRT_DIR, "emIRT_metadata.rds"))
orig <- readRDS(file.path(EMIRT_DIR, "emIRT_model_output.rds"))
N <- nrow(inp$rc)
vote_date <- meta$unique_dates[inp$bill.session + 1]
cols2 <- which(vote_date >= CORTE)
rc2 <- inp$rc[, cols2, drop = FALSE]
con_var <- apply(rc2, 2, function(v) min(sum(v == 1), sum(v == -1)) >= 2)
cols2 <- cols2[con_var]; rc2 <- rc2[, con_var, drop = FALSE]
fechas2 <- sort(unique(vote_date[cols2]))
Tk <- length(fechas2)
bs2 <- matrix(as.integer(match(vote_date[cols2], fechas2) - 1L), ncol = 1)
priors <- list(x.mu0 = matrix(0, N, 1), x.sigma0 = matrix(1, N, 1),
               beta.mu = matrix(0, 2, 1), beta.sigma = diag(25, 2),
               omega2 = matrix(0.025, N, 1))
priors$x.mu0[meta$idx_derecha] <- 2;  priors$x.sigma0[meta$idx_derecha] <- 0.05
priors$x.mu0[meta$idx_izquierda] <- -2; priors$x.sigma0[meta$idx_izquierda] <- 0.05
per_orig <- match(fechas2, meta$unique_dates)
starts0 <- list(alpha = orig$means$alpha[cols2, , drop = FALSE],
                beta = orig$means$beta[cols2, , drop = FALSE],
                x = orig$means$x[, per_orig, drop = FALSE])
dat2 <- list(rc = rc2, startlegis = matrix(0L, N, 1), endlegis = matrix(Tk - 1L, N, 1),
             bill.session = bs2, T = Tk)
fit2 <- dynIRT(.data = dat2, .starts = starts0, .priors = priors,
               .control = list(threads = 1, verbose = FALSE, thresh = 1e-6, maxit = 500))
cat(sprintf("  Era 2/3 base: %d votaciones, %d períodos\n", ncol(rc2), Tk))

# ---------------- panel + horse race (función) -------------------------------
panel <- read.csv(file.path(DATA_PROCESSED, "network_exposure_panel.csv"), stringsAsFactors = FALSE)
roster <- sort(unique(panel$legislator)); n <- length(roster)
ridx <- match(roster, meta$votantes)
expo <- function(W, th) { num <- W %*% ifelse(is.na(th), 0, th); den <- rowSums(W)
  out <- as.numeric(num) / den; out[den == 0] <- NA; out }
Wlist <- list(); fechas_onda_l <- list()
for (k in 1:7) {
  comm <- sprintf("C%d", k)
  waves <- fromJSON(file.path(DATA_PROCESSED, sprintf("C%d_dynamic_networks.json", k)), simplifyDataFrame = TRUE)
  wn <- names(waves)
  sub_p <- panel[panel$commission == comm, c("legislator", "step", "emirt_date")]
  fechas_onda <- sapply(seq_along(wn) - 1L, function(st) unique(sub_p$emirt_date[sub_p$step == st])[1])
  Wc <- lapply(wn, function(w) {
    ed <- waves[[w]]
    W <- matrix(0, n, n, dimnames = list(roster, roster))
    if (length(ed) && nrow(ed)) { W[cbind(ed$source, ed$target)] <- ed$weight; W[cbind(ed$target, ed$source)] <- ed$weight }
    W
  })
  Wlist[[comm]] <- Wc; fechas_onda_l[[comm]] <- as.Date(fechas_onda)
}
per_of <- function(f) sapply(as.Date(f), function(x) max(which(fechas2 <= x)))

horse_race <- function(theta_mat, w0 = CORTE, w1 = W1) {
  # theta_mat: n x Tk (orden roster, períodos era-2/3)
  rows <- list()
  for (comm in names(Wlist)) {
    fo <- fechas_onda_l[[comm]]
    for (t in seq_along(fo)) {
      fecha <- fo[t]
      if (is.na(fecha) || fecha < min(fechas2)) next
      th <- theta_mat[, max(which(fechas2 <= fecha))]
      rows[[length(rows) + 1]] <- data.frame(legislator = roster, commission = comm, step = t - 1L,
                                             fecha = as.character(fecha), theta_reg = th,
                                             E_reg = expo(Wlist[[comm]][[t]], th))
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
  df <- df[as.Date(df$fecha) >= w0 & as.Date(df$fecha) <= w1, ]
  ok <- !is.na(df$delta) & !is.na(df$fecha_lag) & per_of(df$fecha) != per_of(df$fecha_lag) &
    !is.na(df$E_lag) & !is.na(df$E_lead)
  d <- df[ok, ]
  pd <- pdata.frame(d, index = "legislator")
  aux <- plm(E_lead ~ E_lag, data = pd, model = "within")
  d$innov <- as.numeric(residuals(aux))
  m <- plm(delta ~ theta_lag + E_lag + innov, data = pdata.frame(d, index = "legislator"), model = "within")
  ct <- coeftest(m, vcov = vcovHC(m, method = "arellano", cluster = "group"))
  c(lag = unname(ct["E_lag", 1]), se_lag = unname(ct["E_lag", 2]),
    innov = unname(ct["innov", 1]), se_innov = unname(ct["innov", 2]), n = nrow(d))
}

base <- horse_race(fit2$means$x[ridx, , drop = FALSE])
cat(sprintf("  Base era-2/3: lag %+.4f (EE %.4f) | innov %+.4f (EE %.4f) | n = %d\n",
            base["lag"], base["se_lag"], base["innov"], base["se_innov"], base["n"]))

# ---------------- (A) réplicas de medición -----------------------------------
alpha2 <- fit2$means$alpha[, 1]; beta2 <- fit2$means$beta[, 1]; X2 <- fit2$means$x
per2 <- bs2[, 1] + 1L
PRJ <- pnorm(matrix(alpha2, N, ncol(rc2), byrow = TRUE) +
             matrix(beta2, N, ncol(rc2), byrow = TRUE) * X2[, per2])
OBS <- rc2 != 0
one_rep <- function(b) {
  set.seed(3000 + b)
  rcs <- matrix(0L, N, ncol(rc2)); rcs[OBS] <- ifelse(runif(sum(OBS)) < PRJ[OBS], 1L, -1L)
  keep <- which(apply(rcs, 2, function(v) min(sum(v == 1), sum(v == -1)) >= 2))
  perk <- per2[keep]; fk <- sort(unique(perk))
  dat <- list(rc = rcs[, keep, drop = FALSE], startlegis = matrix(0L, N, 1),
              endlegis = matrix(length(fk) - 1L, N, 1),
              bill.session = matrix(as.integer(match(perk, fk) - 1L), ncol = 1), T = length(fk))
  st <- list(alpha = fit2$means$alpha[keep, , drop = FALSE],
             beta = fit2$means$beta[keep, , drop = FALSE], x = X2[, fk, drop = FALSE])
  f <- dynIRT(.data = dat, .starts = st, .priors = priors,
              .control = list(threads = 1, verbose = FALSE, thresh = 1e-6, maxit = 500))
  th <- matrix(NA_real_, N, Tk); th[, fk] <- f$means$x
  # períodos sin columnas: interpolar con el vecino anterior (LOCF)
  for (j in 2:Tk) if (all(is.na(th[, j]))) th[, j] <- th[, j - 1]
  tryCatch(horse_race(th[ridx, , drop = FALSE]), error = function(e) rep(NA_real_, 5))
}
cat(sprintf("  Réplicas de medición (B = %d, mclapply 8)...\n", B))
reps <- mclapply(1:B, one_rep, mc.cores = 8)
rm_ <- do.call(rbind, reps); rm_ <- rm_[complete.cases(rm_), , drop = FALSE]
lag_b <- rm_[, "lag"]; se_b <- rm_[, "se_lag"]
var_tot <- mean(se_b^2) + (1 + 1/nrow(rm_)) * var(lag_b)
res <- data.frame(
  bloque = c(rep("medición era-2/3", 5)),
  cantidad = c("lag medio entre réplicas", "EE muestral medio", "DE entre réplicas (medición)",
               "EE total (Rubin)", "z del lag con EE total"),
  valor = c(mean(lag_b), sqrt(mean(se_b^2)), sd(lag_b), sqrt(var_tot), base["lag"] / sqrt(var_tot)))

# ---------------- (B) sub-ventanas -------------------------------------------
sw1 <- horse_race(fit2$means$x[ridx, , drop = FALSE], CORTE, as.Date("2022-03-31"))
sw2 <- horse_race(fit2$means$x[ridx, , drop = FALSE], as.Date("2022-04-01"), W1)
res <- rbind(res,
  data.frame(bloque = "sub-ventana feb15-mar31",
             cantidad = c("lag", "EE lag", "innov", "EE innov", "n"),
             valor = unname(sw1)),
  data.frame(bloque = "sub-ventana abr1-may14",
             cantidad = c("lag", "EE lag", "innov", "EE innov", "n"),
             valor = unname(sw2)))
print(res, row.names = FALSE, digits = 3)
write.csv(res, file.path(RESULTS_TABLES, "M2_norms_robustness.csv"), row.names = FALSE)
cat(sprintf("--- Done (%.1f min) ---\n", as.numeric(difftime(Sys.time(), T0, units = "mins"))))

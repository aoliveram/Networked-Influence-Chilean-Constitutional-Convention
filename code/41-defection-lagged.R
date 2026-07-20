# =============================================================================
# 41-defection-lagged.R  (comentario del autor 2026-07-20, punto 8.2)
# Variantes REZAGADAS del modelo de co-defección: en vez de la exposición
# CONTEMPORÁNEA (los míos defeccionan EN esta votación), ¿predice mi defección
# lo que mis co-firmantes hicieron ANTES?
#   (a) E_prev1: defección de mis vecinos en la votación INMEDIATAMENTE anterior
#   (b) E_prevday: tasa de defección de mis vecinos en el DÍA anterior de Pleno
#   (c) horse race: contemporánea + previa juntas
#   (d) E_cum: tasa de defección acumulada de mis vecinos hasta v-1 (propensión)
# Mismo aparato del 19: D_iv vs mayoría del conglomerado, FE persona + votación,
# SE cluster por convencional, ventana era de normas (>= 2022-02-15).
# Nota interpretativa: el rezago corta la simultaneidad mecánica (Manski),
# pero una votación y la anterior pueden ser del MISMO paquete temático.
#
# Output: results/tables/M_defection_lagged.csv
# =============================================================================

cat("=== 41-defection-lagged.R ===\n")
suppressPackageStartupMessages({ library(jsonlite); library(fixest) })
set.seed(42)
source("code/paths.R")

inp <- readRDS(file.path(EMIRT_DIR, "emIRT_data_input.rds"))
meta <- readRDS(file.path(EMIRT_DIR, "emIRT_metadata.rds"))
listas <- read.csv(file.path(DATA_RAW, "electoral_lists.csv"), stringsAsFactors = FALSE)
edges <- read.csv(file.path(DATA_PROCESSED, "genesis_network_initiative.csv"), stringsAsFactors = FALSE)

rc <- inp$rc
votantes <- meta$votantes
vote_date <- meta$unique_dates[inp$bill.session + 1]
congl <- listas$conglomerado[match(votantes, listas$nombre_armonizado)]
BLOQUES <- c("Vamos por Chile", "Apruebo Dignidad", "Lista del Apruebo",
             "Lista del Pueblo", "Escaños Reservados PPOO")
W <- matrix(0, length(votantes), length(votantes), dimnames = list(votantes, votantes))
ok <- edges$source %in% votantes & edges$target %in% votantes
W[cbind(edges$source[ok], edges$target[ok])] <- edges$weight[ok]
W[cbind(edges$target[ok], edges$source[ok])] <- edges$weight[ok]

defection_matrix <- function(rcm) {
  D <- matrix(NA_real_, nrow(rcm), ncol(rcm))
  for (b in BLOQUES) {
    m <- which(congl == b)
    y <- colSums(rcm[m, , drop = FALSE] == 1)
    n <- colSums(rcm[m, , drop = FALSE] == -1)
    modal <- sign(y - n)
    sub <- rcm[m, , drop = FALSE]
    Db <- (sub != 0) * (sub != matrix(modal, length(m), ncol(rcm), byrow = TRUE))
    Db[sub == 0] <- NA
    Db[, modal == 0] <- NA
    D[m, ] <- Db
  }
  D
}
expo_cols <- function(D) {           # exposición contemporánea por columna
  M <- !is.na(D); D0 <- D; D0[!M] <- 0
  E <- (W %*% D0) / (W %*% M); E[(W %*% M) == 0] <- NA
  E
}

D <- defection_matrix(rc)
E <- expo_cols(D)
J <- ncol(rc)

# (a) exposición en la votación anterior (columnas en orden cronológico)
E_prev1 <- cbind(NA_real_, E[, -J, drop = FALSE])

# (b) tasa de defección de vecinos en el día ANTERIOR de Pleno
dias <- sort(unique(vote_date))
M <- !is.na(D); D0 <- D; D0[!M] <- 0
E_prevday <- matrix(NA_real_, nrow(D), J)
num_day <- sapply(dias, function(dd) rowSums(D0[, vote_date == dd, drop = FALSE]))
den_day <- sapply(dias, function(dd) rowSums(M[, vote_date == dd, drop = FALSE]))
for (di in 2:length(dias)) {
  nn <- W %*% num_day[, di - 1]; ddn <- W %*% den_day[, di - 1]
  e <- nn / ddn; e[ddn == 0] <- NA
  E_prevday[, vote_date == dias[di]] <- e[, rep(1, sum(vote_date == dias[di]))]
}

# (d) tasa acumulada de defección de vecinos hasta v-1
cnum <- t(apply(D0, 1, cumsum)); cden <- t(apply(M, 1, cumsum))
E_cum <- matrix(NA_real_, nrow(D), J)
if (J > 1) {
  nn <- W %*% cnum[, -J, drop = FALSE]; ddn <- W %*% cden[, -J, drop = FALSE]
  e <- nn / ddn; e[ddn == 0] <- NA
  E_cum[, -1] <- e
}

cols_main <- which(vote_date >= as.Date("2022-02-15"))
build <- function(vars, cols) {
  base <- c(list(D = D), vars)
  idxok <- Reduce(`&`, lapply(base, function(Mx) !is.na(Mx[, cols, drop = FALSE])))
  idx <- which(idxok, arr.ind = TRUE)
  out <- data.frame(nombre = votantes[idx[, 1]], vote_id = cols[idx[, 2]])
  for (nm in names(base)) out[[nm]] <- base[[nm]][, cols, drop = FALSE][idx]
  out
}
fit1 <- function(long, rhs, label) {
  m <- feglm(as.formula(paste("D ~", rhs, "| nombre + vote_id")),
             data = long, family = binomial())
  ct <- summary(m, cluster = ~nombre)$coeftable
  out <- data.frame(model = label, term = rownames(ct), estimate = ct[, 1],
                    se = ct[, 2], p = ct[, 4], n = nobs(m), row.names = NULL)
  print(out, row.names = FALSE, digits = 3)
  out
}

cat("\n--- (a) exposición en la votación anterior ---\n")
r_a <- fit1(build(list(E_prev1 = E_prev1), cols_main), "E_prev1", "prev-votación")
cat("\n--- (b) exposición del día anterior de Pleno ---\n")
r_b <- fit1(build(list(E_prevday = E_prevday), cols_main), "E_prevday", "prev-día")
cat("\n--- (c) horse race: contemporánea + previa ---\n")
r_c <- fit1(build(list(E = E, E_prev1 = E_prev1), cols_main), "E + E_prev1", "horse race")
cat("\n--- (d) propensión acumulada de vecinos hasta v-1 ---\n")
r_d <- fit1(build(list(E_cum = E_cum), cols_main), "E_cum", "propensión acumulada")
cat("\n--- referencia: contemporánea sola (19) ---\n")
r_e <- fit1(build(list(E = E), cols_main), "E", "contemporánea (ref)")

tab <- rbind(r_a, r_b, r_c, r_d, r_e)
dir.create(RESULTS_TABLES, recursive = TRUE, showWarnings = FALSE)
write.csv(tab, file.path(RESULTS_TABLES, "M_defection_lagged.csv"), row.names = FALSE)
cat("--- Done ---\n")

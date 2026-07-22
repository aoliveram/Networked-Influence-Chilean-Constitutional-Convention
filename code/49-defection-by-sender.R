# =============================================================================
# 49-defection-by-sender.R  (comentario del autor 2026-07-22, punto 5)
# ¿QUIÉN transmite la defección? La exposición del 19 trata a todos los
# vecinos igual; aquí se parte por ATRIBUTO DEL EMISOR: para el atributo a,
#   E^{a}_iv  = exposición a defectores vecinos CON el atributo
#   E^{no-a}_iv = exposición a defectores vecinos SIN el atributo
# y el modelo estima ambos canales a la vez:
#   Pr(D_iv) = Lambda(eta_i + mu_v + phi_a E^a + phi_na E^{no-a}).
# Si los abogados (o educados, o experimentados) fueran emisores especialmente
# influyentes, phi_a > phi_na. Cautelas heredadas del 19 (Manski/homofilia):
# esto es clustering direccional, no influencia causal; el contraste entre
# canales es más informativo que los niveles.
# Ventana: era de normas. EE cluster por convencional. fixest con 3 hilos
# (regla de CPU: nocturno corriendo en 8P).
#
# Output: results/tables/M_defection_by_sender.csv
# =============================================================================

cat("=== 49-defection-by-sender.R ===\n")
suppressPackageStartupMessages({ library(jsonlite); library(fixest) })
setFixest_nthreads(3)
set.seed(42)
source("code/paths.R")

inp <- readRDS(file.path(EMIRT_DIR, "emIRT_data_input.rds"))
meta <- readRDS(file.path(EMIRT_DIR, "emIRT_metadata.rds"))
listas <- read.csv(file.path(DATA_RAW, "electoral_lists.csv"), stringsAsFactors = FALSE)
edges <- read.csv(file.path(DATA_PROCESSED, "genesis_network_initiative.csv"), stringsAsFactors = FALSE)
profiles <- fromJSON(PROFILES)

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
expo_with <- function(Wm, D) {
  M <- !is.na(D); D0 <- D; D0[!M] <- 0
  E <- (Wm %*% D0) / (Wm %*% M); E[(Wm %*% M) == 0] <- NA
  E
}

D <- defection_matrix(rc)
cols_main <- which(vote_date >= as.Date("2022-02-15"))

abog <- profiles$es_abogado[match(votantes, profiles$nombre_armonizado)] == 1
educ <- profiles$grado_academico_nivel[match(votantes, profiles$nombre_armonizado)] >= 2
exper <- profiles$experiencia_previa_institucional[match(votantes, profiles$nombre_armonizado)] == 1
ATTRS <- list("abogado" = abog, "posgrado (magister/doctorado)" = educ,
              "experiencia institucional" = exper)

res <- list()
for (nm in names(ATTRS)) {
  a <- ATTRS[[nm]]
  W_a <- W * rep(as.integer(a), each = nrow(W))       # columnas j con atributo
  W_na <- W * rep(as.integer(!a), each = nrow(W))
  E_a <- expo_with(W_a, D); E_na <- expo_with(W_na, D)
  idx <- which(!is.na(D[, cols_main, drop = FALSE]) &
                 !is.na(E_a[, cols_main, drop = FALSE]) &
                 !is.na(E_na[, cols_main, drop = FALSE]), arr.ind = TRUE)
  long <- data.frame(nombre = votantes[idx[, 1]], vote_id = cols_main[idx[, 2]],
                     D = D[, cols_main, drop = FALSE][idx],
                     E_a = E_a[, cols_main, drop = FALSE][idx],
                     E_na = E_na[, cols_main, drop = FALSE][idx])
  m <- feglm(D ~ E_a + E_na | nombre + vote_id, data = long, family = binomial())
  ct <- summary(m, cluster = ~nombre)$coeftable
  Vc <- vcov(m, cluster = ~nombre)
  dif <- ct["E_a", 1] - ct["E_na", 1]
  se_dif <- sqrt(Vc["E_a", "E_a"] + Vc["E_na", "E_na"] - 2 * Vc["E_a", "E_na"])
  res[[nm]] <- data.frame(
    atributo = nm, n = nobs(m), n_emisores = sum(a),
    phi_con = ct["E_a", 1], se_con = ct["E_a", 2],
    phi_sin = ct["E_na", 1], se_sin = ct["E_na", 2],
    dif = dif, se_dif = se_dif, p_dif = 2 * pnorm(-abs(dif / se_dif)))
  cat(sprintf("  %-28s phi_con = %+6.2f (%.2f) | phi_sin = %+6.2f (%.2f) | dif = %+5.2f (p = %.3f) | emisores = %d\n",
              nm, ct["E_a", 1], ct["E_a", 2], ct["E_na", 1], ct["E_na", 2],
              dif, 2 * pnorm(-abs(dif / se_dif)), sum(a)))
}
tab <- do.call(rbind, res)
dir.create(RESULTS_TABLES, recursive = TRUE, showWarnings = FALSE)
write.csv(tab, file.path(RESULTS_TABLES, "M_defection_by_sender.csv"), row.names = FALSE)
cat("--- Done ---\n")

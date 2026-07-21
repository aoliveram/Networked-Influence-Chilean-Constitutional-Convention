# =============================================================================
# 28-rhem-run.R  (IV.P2 / RHEM-intro §9: EL RUN REAL)
#
# RHEM no dirigido sobre las iniciativas <=16 (D8) del registro de la
# PLATAFORMA (initiative_registry.csv, refresco code/27 -> 00): 947 eventos,
# todos con fecha_iso (131 imputadas). Los eventos sin comisión asignada
# (~120) entran igual: prop_comision = 0 en su estrato (constante, no
# informa el clogit) y su matching blando degrada a uniforme.
#
# Diseño (RHEM-intro §9, aprobado):
#   - Eventos: iniciativas fechadas; tiempo = días desde 2021-11-01.
#     Empates de día: los eventos del mismo día NO se ven entre sí
#     (historia estricta: pasado = días anteriores).
#   - Estadísticas de historia (motor propio vectorizado; verificado contra
#     amorem::hyperedge_subrep en memoria infinita):
#       subrep_1/2/3 en DOS memorias: infinita (w=1) y corta
#       (w = exp(-Δ·ln2/15), semivida 15 días).
#     Truco de cómputo: para el candidato h y el evento pasado e,
#     el nº de subconjuntos de tamaño r de h contenidos en S_e es
#     choose(|S_e ∩ h|, r) -> todo se reduce a |S_e ∩ h| (producto matricial).
#   - Exógenas de composición: disp_theta1, disp_theta2 (media de |dif| por
#     pares), prop_lista (pares mismo conglomerado), prop_comision.
#   - Caso-control: m = 50 por evento (25 uniformes + 25 con matching blando:
#     sorteados entre los miembros de la comisión del texto).
#   - Features estandarizadas por dataset; ajuste amorem::rem(clogit) con
#     fallback a survival::clogit(method = "efron").
#   - Estabilidad: B = 10 re-muestreos de controles (semillas 42+b).
#   - mclapply(8) sobre estratos para el armado de features.
#
# Outputs: results/tables/RHEM_results.csv   (por re-muestreo x memoria x término)
#          results/tables/RHEM_summary.csv   (agregado: media, DE, rango)
#          data/processed/rhem_case_control_b1.csv (dataset del re-muestreo 1)
# =============================================================================

cat("=== 28-rhem-run.R ===\n")
suppressPackageStartupMessages({
  library(jsonlite); library(amorem); library(survival); library(parallel)
})
set.seed(42)
source("code/paths.R")
T0 <- Sys.time()
N_CORES <- max(1, min(8, detectCores() - 2))
M_CONTROLES <- 50
B_RESAMPLES <- 10
HALF_LIFE <- 15

profiles <- fromJSON(PROFILES)
roster <- sort(fromJSON(MEMBERS))
ip2d <- read.csv(file.path(DATA_PROCESSED, "ideal_points_2d_firstmonth.csv"), stringsAsFactors = FALSE)
listas <- read.csv(file.path(DATA_RAW, "electoral_lists.csv"), stringsAsFactors = FALSE)
memb <- read.csv(file.path(DATA_RAW, "commission_membership.csv"), stringsAsFactors = FALSE)
registry <- read.csv(file.path(DATA_PROCESSED, "initiative_registry.csv"), stringsAsFactors = FALSE)
registry <- registry[registry$n_firmantes >= 2 & registry$n_firmantes <= 16, ]

# fechas: vienen en el propio registro (fecha_iso de la plataforma, 100%)
registry$dia <- as.numeric(as.Date(registry$fecha) - as.Date("2021-11-01"))
registry <- registry[order(registry$dia, registry$initiative_id), ]
E <- nrow(registry)
cat(sprintf("  Eventos: %d (todos fechados; %d con fecha imputada) | %s a %s | %d días distintos\n",
            E, sum(registry$fecha_imputada), min(registry$fecha), max(registry$fecha),
            length(unique(registry$dia))))

# ------------------- atributos y matriz de membresía -------------------------
n <- length(roster)
theta1 <- setNames(ip2d$theta1_fm[match(roster, ip2d$nombre_armonizado)], roster)
theta2 <- setNames(ip2d$theta2_fm[match(roster, ip2d$nombre_armonizado)], roster)
congl <- listas$conglomerado[match(roster, listas$nombre_armonizado)]
congl[congl == "REVISAR (lista local sin conglomerado)"] <- "Otras listas locales"
congl <- setNames(congl, roster)
comis <- setNames(memb$commission[match(roster, memb$nombre_armonizado)], roster)
abog <- setNames(profiles$es_abogado[match(roster, profiles$nombre_armonizado)] == 1, roster)
exper <- setNames(profiles$experiencia_previa_institucional[match(roster, profiles$nombre_armonizado)] == 1, roster)
distr <- setNames(profiles$distrito[match(roster, profiles$nombre_armonizado)], roster)
mujer <- setNames(profiles$es_mujer[match(roster, profiles$nombre_armonizado)] == 1, roster)
grado <- setNames(profiles$grado_academico_nivel[match(roster, profiles$nombre_armonizado)], roster)

S_list <- lapply(registry$firmantes, function(s) {
  x <- strsplit(s, "; ", fixed = TRUE)[[1]]; x[x %in% roster]
})
M <- matrix(0L, E, n, dimnames = list(NULL, roster))
for (k in seq_len(E)) M[k, S_list[[k]]] <- 1L

# ------------------- motor de features (propio, vectorizado) -----------------
# Para el estrato k: candidatos = caso + m controles (matriz C de tamaños
# (1+m) x n). kmat = M[pasado,] %*% t(C): |S_e ∩ h| para cada (evento pasado,
# candidato). subrep_r = w' choose(kmat, r) / choose(|h|, r).
features_stratum <- function(k, C_mask) {
  past <- which(registry$dia < registry$dia[k])
  s <- rowSums(C_mask)
  out <- matrix(0, nrow(C_mask), 6,
                dimnames = list(NULL, c("sr1_inf", "sr2_inf", "sr3_inf",
                                        "sr1_15", "sr2_15", "sr3_15")))
  if (length(past) > 0) {
    kmat <- M[past, , drop = FALSE] %*% t(C_mask)          # pasado x candidatos
    w30 <- exp(-(registry$dia[k] - registry$dia[past]) * log(2) / HALF_LIFE)
    for (r in 1:3) {
      ch <- choose(kmat, r)                                 # pasado x candidatos
      out[, r] <- colSums(ch) / choose(s, r)
      out[, r + 3] <- colSums(ch * w30) / choose(s, r)
    }
  }
  # exógenas de composición (incl. H1b: pares ambos-abogados / ambos-experiencia)
  exo <- t(apply(C_mask, 1, function(row) {
    cand <- roster[row == 1]
    p <- combn(length(cand), 2)
    th1 <- theta1[cand]; th2 <- theta2[cand]; cg <- congl[cand]
    ab <- abog[cand]; ex <- exper[cand]; dd <- distr[cand]
    mu <- mujer[cand]; gr <- grado[cand]
    c(disp_theta1 = mean(abs(th1[p[1, ]] - th1[p[2, ]])),
      disp_theta2 = mean(abs(th2[p[1, ]] - th2[p[2, ]])),
      prop_lista = mean(cg[p[1, ]] == cg[p[2, ]]),
      prop_comision = mean(comis[cand] == registry$commission[k]),
      pares_abogado = mean(ab[p[1, ]] & ab[p[2, ]]),
      pares_exper = mean(ex[p[1, ]] & ex[p[2, ]]),
      pares_distrito = mean(dd[p[1, ]] == dd[p[2, ]]),
      pares_mujer = mean(mu[p[1, ]] & mu[p[2, ]]),
      disp_grado = mean(abs(gr[p[1, ]] - gr[p[2, ]])))
  }))
  cbind(out, exo)
}

# ------------------- sanity checks -------------------------------------------
# (a) primer evento: historia vacía
stopifnot(all(features_stratum(1, M[1, , drop = FALSE])[, 1:6] == 0))
# (b) memoria infinita == amorem::hyperedge_subrep (en eventos de día único)
hl <- hyperedge_log(I = S_list, J = replicate(E, character(0), simplify = FALSE),
                    time = registry$dia)
dias_unicos <- which(!(registry$dia %in% registry$dia[duplicated(registry$dia)]))
for (k in sample(dias_unicos[dias_unicos > 5], 4)) {
  propio <- features_stratum(k, M[k, , drop = FALSE])
  for (r in 1:2) {
    pkg <- hyperedge_subrep(hl, I = S_list[[k]], t = registry$dia[k], rho = r)
    stopifnot(abs(propio[1, r] - pkg) < 1e-8)
  }
}
cat("  Sanity: motor propio == amorem::hyperedge_subrep (memoria infinita) OK\n")

# ------------------- caso-control + ajuste por re-muestreo -------------------
EXO <- c("disp_theta1", "disp_theta2", "prop_lista", "prop_comision",
         "pares_abogado", "pares_exper", "pares_distrito", "pares_mujer", "disp_grado")
FEATS <- c("sr1_inf", "sr2_inf", "sr3_inf", "sr1_15", "sr2_15", "sr3_15", EXO)
# PRINCIPAL (decisión del autor 2026-07-20): solo sub.rep(2) — el par es la
# unidad mínima de una relación; sr1 (individual, no relacional) y sr3
# (recicla la info de los pares) entran solo en la robustez conjunta, donde
# la colinealidad (r = 0.78/0.84) produce signos no interpretables por separado.
F_INF <- c("sr2_inf", EXO)
F_30 <- c("sr2_15", EXO)
F_INF_TRIO <- c("sr1_inf", "sr2_inf", "sr3_inf", EXO)
F_30_TRIO <- c("sr1_15", "sr2_15", "sr3_15", EXO)

build_resample <- function(b) {
  strata_rows <- mclapply(seq_len(E), function(k) {
    set.seed(42 + b * 10000 + k)
    s <- length(S_list[[k]])
    com_members <- roster[comis == registry$commission[k]]
    ctrl <- c(replicate(M_CONTROLES / 2, sample(roster, s), simplify = FALSE),
              replicate(M_CONTROLES / 2,
                        if (length(com_members) >= s) sample(com_members, s)
                        else sample(roster, s), simplify = FALSE))
    C_mask <- matrix(0L, 1 + M_CONTROLES, n, dimnames = list(NULL, roster))
    C_mask[1, S_list[[k]]] <- 1L
    for (j in seq_along(ctrl)) C_mask[1 + j, ctrl[[j]]] <- 1L
    cbind(stratum = k, case = c(1L, rep(0L, M_CONTROLES)),
          features_stratum(k, C_mask))
  }, mc.cores = N_CORES)
  df <- as.data.frame(do.call(rbind, strata_rows))
  df[FEATS] <- scale(df[FEATS])                       # estandarización por dataset
  df
}

fit_one <- function(df, feats, label, b) {
  f <- as.formula(paste("case ~", paste(feats, collapse = " + ")))
  fit <- tryCatch(rem(f, data = df, method = "clogit", stratum = "stratum"),
                  error = function(e) NULL)
  conv <- !is.null(fit)
  if (!conv) {   # fallback: clogit Efron directo
    fit <- clogit(update(f, . ~ . + strata(stratum)), data = df, method = "efron")
  }
  sm <- summary(fit)$coefficients
  data.frame(b = b, memoria = label, term = rownames(sm),
             estimate = sm[, "coef"], se = sm[, "se(coef)"],
             p = sm[, "Pr(>|z|)"], backend = ifelse(conv, "rem", "clogit_efron"),
             row.names = NULL)
}

cat(sprintf("\n  Corriendo B = %d re-muestreos (m = %d: %d uniformes + %d comisión; %d cores)...\n",
            B_RESAMPLES, M_CONTROLES, M_CONTROLES / 2, M_CONTROLES / 2, N_CORES))
res <- list()
for (b in seq_len(B_RESAMPLES)) {
  tb <- Sys.time()
  df <- build_resample(b)
  if (b == 1) write.csv(df, file.path(DATA_PROCESSED, "rhem_case_control_b1.csv"),
                        row.names = FALSE)
  res[[length(res) + 1]] <- fit_one(df, F_INF, "infinita", b)
  res[[length(res) + 1]] <- fit_one(df, F_30, "semivida 15d", b)
  res[[length(res) + 1]] <- fit_one(df, F_INF_TRIO, "infinita (trio, robustez)", b)
  res[[length(res) + 1]] <- fit_one(df, F_30_TRIO, "semivida 15d (trio, robustez)", b)
  cat(sprintf("    b=%d listo (%.1f s)\n", b,
              as.numeric(difftime(Sys.time(), tb, units = "secs"))))
}
tab <- do.call(rbind, res)
dir.create(RESULTS_TABLES, recursive = TRUE, showWarnings = FALSE)
write.csv(tab, file.path(RESULTS_TABLES, "RHEM_results.csv"), row.names = FALSE)

# ------------------- resumen de estabilidad ----------------------------------
agg <- do.call(rbind, lapply(split(tab, list(tab$memoria, tab$term), drop = TRUE),
               function(g) {
  data.frame(memoria = g$memoria[1], term = g$term[1],
             est_media = mean(g$estimate), est_de = sd(g$estimate),
             est_min = min(g$estimate), est_max = max(g$estimate),
             se_media = mean(g$se), p_mediana = median(g$p))
}))
agg <- agg[order(agg$memoria, -abs(agg$est_media)), ]
write.csv(agg, file.path(RESULTS_TABLES, "RHEM_summary.csv"), row.names = FALSE)
cat("\n--- Resumen (coeficientes estandarizados; media sobre 10 re-muestreos) ---\n")
print(agg, row.names = FALSE, digits = 3)
cat(sprintf("  Backends usados: %s\n",
            paste(names(table(tab$backend)), table(tab$backend), collapse = "; ")))

# ------------------- lecturas auxiliares (sobre b = 1) -----------------------
# (i) colinealidad entre las subrep (clave para leer el signo de sr1|sr2);
# (ii) efectos de cada subrep A SOLAS vs juntas (partialling);
# (iii) comparación de memorias por logLik en el mismo dataset.
df1 <- read.csv(file.path(DATA_PROCESSED, "rhem_case_control_b1.csv"))
cat("\n--- Correlaciones entre estadísticas de historia (b = 1) ---\n")
print(round(cor(df1[, c("sr1_inf", "sr2_inf", "sr3_inf")]), 3))

fit_ll <- function(feats) {
  f <- as.formula(paste("case ~", paste(feats, collapse = " + "), "+ strata(stratum)"))
  clogit(f, data = df1, method = "efron")
}
cat("\n--- Correlaciones completas sr1/sr2/sr3 (b = 1) ---\n")
solo <- list(
  "sr1 sola" = c("sr1_inf", EXO),
  "sr2 sola" = c("sr2_inf", EXO),
  "sr3 sola" = c("sr3_inf", EXO),
  "las tres" = F_INF
)
cat("\n--- Cada subrep a solas vs juntas (memoria infinita, b = 1) ---\n")
aux <- do.call(rbind, lapply(names(solo), function(lb) {
  m <- fit_ll(solo[[lb]])
  sm <- summary(m)$coefficients
  sr <- grep("^sr", rownames(sm), value = TRUE)
  data.frame(modelo = lb, term = sr, estimate = sm[sr, "coef"],
             p = sm[sr, "Pr(>|z|)"], logLik = as.numeric(logLik(m)), row.names = NULL)
}))
print(aux, row.names = FALSE, digits = 3)

ll_inf <- logLik(fit_ll(F_INF)); ll_30 <- logLik(fit_ll(F_30))
cat(sprintf("\n--- Memorias (b = 1, mismo dataset): logLik infinita = %.1f | semivida 15d = %.1f -> %s ---\n",
            as.numeric(ll_inf), as.numeric(ll_30),
            ifelse(ll_inf > ll_30, "gana infinita", "gana 15d")))
write.csv(aux, file.path(RESULTS_TABLES, "RHEM_aux.csv"), row.names = FALSE)
cat(sprintf("\n--- Done (%.1f min) ---\n", as.numeric(difftime(Sys.time(), T0, units = "mins"))))

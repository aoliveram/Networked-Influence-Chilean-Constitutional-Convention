# =============================================================================
# 15-conditional-logit.R  (respuesta a revisión: D1 principal, D2.1, Q5.2-3)
# M1 re-estimado en la unidad real de observación: la decisión de firma.
#
# Logit condicional (McFadden): para cada iniciativa a (menú) y convencional i,
#   U_ia = beta' x_ia + alpha_a + eps_ia,
# con alpha_a absorbido por strata(iniciativa). Covariables x_ia = distancias/
# afinidades de i respecto de la coalición firmante S_a (leave-one-out para
# firmantes). Incluye:
#   - d_theta1, d_theta2: |theta_i - media(theta_G)| (2D primer mes, pre-red)
#   - lambda_lista por conglomerado: 1{congl_i == congl modal de G == c}
#     (test D2: ¿las listas ad hoc coordinan como los pactos tradicionales?)
#   - misma_comision: estructura de oportunidad (D10)
#   - afinidades profesionales: es_X_i * proporción de X en G (abogado,
#     experiencia, mujer) y |grado_i - media(grado_G)|
#   - Q5: PPOO + PPOO:d_theta1 + PPOO:d_theta2 (¿los PPOO cruzan distancias
#     en theta1 que otros no cruzan, porque su coordenada operativa es theta2?)
#
# Outputs: data/processed/choice_dataset.csv,
#          results/tables/M1_clogit.csv, results/tables/M1_lambda_lista.csv
# =============================================================================

cat("=== 15-conditional-logit.R ===\n")
suppressPackageStartupMessages({ library(jsonlite); library(survival) })
set.seed(42)
source("code/paths.R")
T0 <- Sys.time()

profiles <- fromJSON(PROFILES)
roster <- sort(fromJSON(MEMBERS))
ip2d <- read.csv(file.path(DATA_PROCESSED, "ideal_points_2d_firstmonth.csv"), stringsAsFactors = FALSE)
listas <- read.csv(file.path(DATA_RAW, "electoral_lists.csv"), stringsAsFactors = FALSE)
memb <- read.csv(file.path(DATA_RAW, "commission_membership.csv"), stringsAsFactors = FALSE)
registry <- read.csv(file.path(DATA_PROCESSED, "initiative_registry.csv"), stringsAsFactors = FALSE)
registry <- registry[registry$n_firmantes <= 16, ]   # decisión D8 (2026-07-11)

n <- length(roster)
theta1 <- ip2d$theta1_fm[match(roster, ip2d$nombre_armonizado)]
theta2 <- ip2d$theta2_fm[match(roster, ip2d$nombre_armonizado)]
grado <- profiles$grado_academico_nivel[match(roster, profiles$nombre_armonizado)]
abog <- profiles$es_abogado[match(roster, profiles$nombre_armonizado)]
exper <- profiles$experiencia_previa_institucional[match(roster, profiles$nombre_armonizado)]
mujer <- profiles$es_mujer[match(roster, profiles$nombre_armonizado)]
comis <- memb$commission[match(roster, memb$nombre_armonizado)]
distr <- profiles$distrito[match(roster, profiles$nombre_armonizado)]
distr_i <- match(distr, sort(unique(distr)))          # 39 categorías (28-29
DISTR_NLEV <- length(unique(distr))                   # distritos + 10 pueblos)
congl <- listas$conglomerado[match(roster, listas$nombre_armonizado)]
congl[congl == "REVISAR (lista local sin conglomerado)"] <- "Otras listas locales"
CONGL_LEV <- c("Vamos por Chile", "Apruebo Dignidad", "Lista del Apruebo",
               "Lista del Pueblo", "Escaños Reservados PPOO",
               "Independientes No Neutrales", "Otras listas locales")
congl_i <- match(congl, CONGL_LEV)
ppoo <- as.integer(congl == "Escaños Reservados PPOO")
stopifnot(!anyNA(theta1), !anyNA(congl_i), !anyNA(comis))

# ------------------- construir el dataset de elección ------------------------
cat(sprintf("  Menú: %d iniciativas x %d convencionales = %d decisiones\n",
            nrow(registry), n, nrow(registry) * n))
rows <- vector("list", nrow(registry))
for (k in seq_len(nrow(registry))) {
  S <- match(strsplit(registry$firmantes[k], "; ", fixed = TRUE)[[1]], roster)
  S <- S[!is.na(S)]
  nS <- length(S)
  if (nS < 2) next
  signed <- integer(n); signed[S] <- 1L
  # sumas de la coalición; leave-one-out para firmantes
  nG <- nS - signed
  s_t1 <- sum(theta1[S]) - signed * theta1
  s_t2 <- sum(theta2[S]) - signed * theta2
  s_gr <- sum(grado[S]) - signed * grado
  s_ab <- sum(abog[S]) - signed * abog
  s_ex <- sum(exper[S]) - signed * exper
  s_mu <- sum(mujer[S]) - signed * mujer
  # conglomerado modal de G (conteo de S menos el propio si firma)
  cnt_S <- tabulate(congl_i[S], nbins = length(CONGL_LEV))
  cnt_G <- matrix(cnt_S, nrow = n, ncol = length(CONGL_LEV), byrow = TRUE)
  cnt_G[cbind(seq_len(n), congl_i)] <- cnt_G[cbind(seq_len(n), congl_i)] - signed
  modal_G <- max.col(cnt_G, ties.method = "first")
  # afinidad de distrito: proporción de G del mismo distrito/pueblo que i
  cnt_d <- tabulate(distr_i[S], nbins = DISTR_NLEV)
  distrito_aff <- (cnt_d[distr_i] - signed) / nG
  rows[[k]] <- data.frame(
    initiative_id = paste(registry$commission[k], registry$initiative_id[k], sep = ":"),
    commission = registry$commission[k],
    nombre_armonizado = roster,
    signed = signed,
    d_theta1 = abs(theta1 - s_t1 / nG),
    d_theta2 = abs(theta2 - s_t2 / nG),
    d_grado = abs(grado - s_gr / nG),
    misma_comision = as.integer(comis == registry$commission[k]),
    misma_lista_modal = as.integer(congl_i == modal_G),
    congl_modal = CONGL_LEV[modal_G],
    abogado_aff = abog * (s_ab / nG),
    exper_aff = exper * (s_ex / nG),
    mujer_aff = mujer * (s_mu / nG),
    distrito_aff = distrito_aff,
    ppoo = ppoo,
    stringsAsFactors = FALSE
  )
}
choice <- do.call(rbind, rows)
cat(sprintf("  Dataset: %d filas | firmas positivas: %d (%.1f%%)\n",
            nrow(choice), sum(choice$signed), 100 * mean(choice$signed)))
write.csv(choice, file.path(DATA_PROCESSED, "choice_dataset.csv"), row.names = FALSE)

# lambda por conglomerado: 1{congl_i == modal de G == c}
for (c_lab in CONGL_LEV) {
  cn <- paste0("lam_", gsub("[^A-Za-z]", "", abbreviate(c_lab, 8)))
  choice[[cn]] <- as.integer(choice$misma_lista_modal == 1 & choice$congl_modal == c_lab)
}
lam_cols <- grep("^lam_", names(choice), value = TRUE)
cat("  Términos lambda:", paste(lam_cols, collapse = ", "), "\n")

tidy_clogit <- function(fit, label) {
  sm <- summary(fit)$coefficients
  data.frame(model = label, term = rownames(sm), estimate = sm[, "coef"],
             or = exp(sm[, "coef"]), se_robust = sm[, "robust se"],
             z = sm[, "z"], p = sm[, "Pr(>|z|)"], row.names = NULL)
}

# ------------------- Modelo 1: principal (D1 + D2.1) ------------------------
cat("\n--- Logit condicional principal (strata = iniciativa; SE cluster convencional) ---\n")
f1 <- as.formula(paste(
  "signed ~ d_theta1 + d_theta2 + misma_comision +",
  paste(lam_cols, collapse = " + "),
  "+ abogado_aff + exper_aff + mujer_aff + distrito_aff + d_grado",
  "+ strata(initiative_id) + cluster(nombre_armonizado)"))
t1 <- Sys.time()
m1 <- clogit(f1, data = choice, method = "efron")
cat(sprintf("  [%.1f s]\n", as.numeric(difftime(Sys.time(), t1, units = "secs"))))
tab1 <- tidy_clogit(m1, "clogit principal")
print(tab1, row.names = FALSE, digits = 3)

# ------------------- Modelo 2: Q5 (interacciones PPOO) ----------------------
cat("\n--- Logit condicional + interacciones PPOO (Q5.2-3) ---\n")
f2 <- update(f1, . ~ . + ppoo + ppoo:d_theta1 + ppoo:d_theta2)
t2 <- Sys.time()
m2 <- clogit(f2, data = choice, method = "efron")
cat(sprintf("  [%.1f s]\n", as.numeric(difftime(Sys.time(), t2, units = "secs"))))
tab2 <- tidy_clogit(m2, "clogit + PPOO")
print(tab2, row.names = FALSE, digits = 3)

dir.create(RESULTS_TABLES, recursive = TRUE, showWarnings = FALSE)
write.csv(rbind(tab1, tab2), file.path(RESULTS_TABLES, "M1_clogit.csv"), row.names = FALSE)

# tabla comparada de lambdas (D2: ¿listas ad hoc ~ pactos tradicionales?)
lam_tab <- tab1[tab1$term %in% lam_cols, ]
lam_tab$conglomerado <- CONGL_LEV[match(lam_tab$term, paste0("lam_", gsub("[^A-Za-z]", "", abbreviate(CONGL_LEV, 8))))]
lam_tab$ci_lo <- lam_tab$estimate - 1.96 * lam_tab$se_robust
lam_tab$ci_hi <- lam_tab$estimate + 1.96 * lam_tab$se_robust
write.csv(lam_tab[, c("conglomerado", "estimate", "or", "se_robust", "p", "ci_lo", "ci_hi")],
          file.path(RESULTS_TABLES, "M1_lambda_lista.csv"), row.names = FALSE)
cat("\n--- Lambda por conglomerado (IC 95%) ---\n")
print(lam_tab[, c("conglomerado", "estimate", "se_robust", "ci_lo", "ci_hi")],
      row.names = FALSE, digits = 3)

cat(sprintf("--- Done (%.1f min) ---\n", as.numeric(difftime(Sys.time(), T0, units = "mins"))))

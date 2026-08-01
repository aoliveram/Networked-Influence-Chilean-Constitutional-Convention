# =============================================================================
# 50-bipartite-mple-boot.R  (comentario del autor 2026-07-22, punto 2:
# ETAPA 1 del plan MPLE + bootstrap — piloto sobre C3, la comisión más chica)
#
# Qué mide este piloto:
#   (a) ¿el MPLE traga los términos estructurales? Especificaciones:
#       S0 = extendida + rangos continuos (la de la Tabla 8)
#       S1 = S0 + gwb1degree(0.5, fixed)     [actividad de firmantes]
#       S2 = S1 + gwdsp(0.5, fixed)          [socios compartidos, si existe p/ bipartito]
#   (b) EE por BOOTSTRAP DE INICIATIVAS (B = 200: re-sortear las iniciativas
#       de la comisión con reemplazo, reconstruir la red, re-MPLE) vs los EE
#       logísticos ingenuos -> factor de inflación.
#   (c) tiempo por fit -> extrapolación a la suite x7 y a B mayores.
#
# Output: results/tables/M1_bipartite_boot_pilot.csv + consola
# =============================================================================

cat("=== 50-bipartite-mple-boot.R (ETAPA 1: C3) ===\n")
suppressPackageStartupMessages({ library(jsonlite); library(statnet); library(parallel) })
set.seed(42)
source("code/paths.R")
T0 <- Sys.time()
B <- 200
K <- 3

profiles <- fromJSON(PROFILES); roster <- sort(fromJSON(MEMBERS))
ip2d <- read.csv(file.path(DATA_PROCESSED, "ideal_points_2d_firstmonth.csv"), stringsAsFactors = FALSE)
listas <- read.csv(file.path(DATA_RAW, "electoral_lists.csv"), stringsAsFactors = FALSE)
memb <- read.csv(file.path(DATA_RAW, "commission_membership.csv"), stringsAsFactors = FALSE)
registry <- read.csv(file.path(DATA_PROCESSED, "initiative_registry.csv"), stringsAsFactors = FALSE)
regk <- registry[registry$n_firmantes >= 2 & registry$n_firmantes <= 16 &
                   registry$commission == sprintf("C%d", K), ]
n1 <- length(roster); nE <- nrow(regk)
cat(sprintf("  C%d: %d iniciativas\n", K, nE))

theta1 <- ip2d$theta1_fm[match(roster, ip2d$nombre_armonizado)]
theta1_q <- as.character(cut(theta1, quantile(theta1, probs = seq(0, 1, .2)), include.lowest = TRUE, labels = paste0("Q",1:5)))
congl <- listas$conglomerado[match(roster, listas$nombre_armonizado)]
congl[congl == "REVISAR (lista local sin conglomerado)"] <- "Otras"
comis_v <- memb$commission[match(roster, memb$nombre_armonizado)]
edad_v <- profiles$edad_al_asumir[match(roster, profiles$nombre_armonizado)]
grado_v <- profiles$grado_academico_nivel[match(roster, profiles$nombre_armonizado)]
edad_q <- as.character(cut(edad_v, quantile(edad_v, probs = seq(0, 1, .2)), include.lowest = TRUE, labels = paste0("E",1:5)))
ATR <- list(conglomerado = congl, theta1_q = theta1_q,
            es_abogado = profiles$es_abogado[match(roster, profiles$nombre_armonizado)],
            experiencia = profiles$experiencia_previa_institucional[match(roster, profiles$nombre_armonizado)],
            es_mujer = profiles$es_mujer[match(roster, profiles$nombre_armonizado)],
            distrito = profiles$distrito[match(roster, profiles$nombre_armonizado)],
            grado = as.character(grado_v), edad_q = edad_q,
            theta1_c = theta1, edad_c = edad_v / 10, grado_c = as.numeric(grado_v),
            miembro = as.integer(comis_v == sprintf("C%d", K)))

build_net <- function(reg) {
  n2 <- nrow(reg)
  net <- network::network.initialize(n1 + n2, directed = FALSE, bipartite = n1)
  for (a in names(ATR)) {
    v <- ATR[[a]]
    pad <- if (is.character(v)) "modo2" else if (a %in% c("theta1_c","edad_c","grado_c")) 0 else -1L
    network::set.vertex.attribute(net, a, c(v, rep(pad, n2)))
  }
  tails <- integer(0); heads <- integer(0)
  for (j in seq_len(n2)) {
    S <- match(strsplit(reg$firmantes[j], "; ", fixed = TRUE)[[1]], roster); S <- S[!is.na(S)]
    tails <- c(tails, S); heads <- c(heads, rep(n1 + j, length(S)))
  }
  network::add.edges(net, tail = tails, head = heads)
}

RHS0 <- paste("~ edges + b1cov(\"miembro\") + b1nodematch(\"conglomerado\") +",
              "b1nodematch(\"theta1_q\") + b1nodematch(\"distrito\") +",
              "b1nodematch(\"es_abogado\") + b1nodematch(\"experiencia\") +",
              "b1nodematch(\"es_mujer\") + b1nodematch(\"grado\") + b1nodematch(\"edad_q\") +",
              "b2covrange(\"theta1_c\") + b2covrange(\"edad_c\") + b2covrange(\"grado_c\")")
SPECS <- list(S0 = RHS0,
              S1 = paste(RHS0, "+ gwb1degree(0.5, fixed = TRUE)"),
              S2 = paste(RHS0, "+ gwb1degree(0.5, fixed = TRUE) + gwdsp(0.5, fixed = TRUE)"))

fit_mple <- function(reg, rhs) {
  net <- build_net(reg)
  fit <- ergm(as.formula(paste("net", rhs)), estimate = "MPLE",
              control = control.ergm(seed = 42))
  coef(fit)
}

# ---------- (a) ¿traga MPLE cada espec? + tiempo por fit ---------------------
res_specs <- list()
for (nm in names(SPECS)) {
  t0 <- Sys.time()
  out <- tryCatch(fit_mple(regk, SPECS[[nm]]), error = function(e) conditionMessage(e))
  secs <- as.numeric(difftime(Sys.time(), t0, units = "secs"))
  if (is.character(out)) {
    cat(sprintf("  %s: ERROR (%s) [%.1f s]\n", nm, substr(out, 1, 60), secs))
  } else {
    cat(sprintf("  %s: OK, %d términos, %.1f s por fit\n", nm, length(out), secs))
    res_specs[[nm]] <- out
  }
}
SPEC_BOOT <- if ("S2" %in% names(res_specs)) "S2" else if ("S1" %in% names(res_specs)) "S1" else "S0"
cat(sprintf("  -> bootstrap sobre la espec %s\n", SPEC_BOOT))

# ---------- (b) bootstrap de iniciativas -------------------------------------
punto <- res_specs[[SPEC_BOOT]]
# EE logísticos ingenuos de la misma espec (para el factor de inflación)
net0 <- build_net(regk)
fit0 <- ergm(as.formula(paste("net0", SPECS[[SPEC_BOOT]])), estimate = "MPLE",
             control = control.ergm(seed = 42))
se_naive <- sqrt(diag(vcov(fit0)))

t0 <- Sys.time()
boot <- mclapply(1:B, function(b) {
  set.seed(5000 + b)
  regb <- regk[sample(nE, nE, replace = TRUE), ]
  tryCatch(fit_mple(regb, SPECS[[SPEC_BOOT]]), error = function(e) NULL)
}, mc.cores = 8)
boot <- boot[!sapply(boot, is.null)]
mins_boot <- as.numeric(difftime(Sys.time(), t0, units = "mins"))
bm <- do.call(rbind, boot)
se_boot <- apply(bm, 2, sd)
tab <- data.frame(term = names(punto), estimate = punto,
                  se_naive = se_naive[names(punto)], se_boot = se_boot[names(punto)],
                  inflacion = (se_boot / se_naive)[names(punto)],
                  sig_boot = abs(punto) > 1.96 * se_boot[names(punto)], row.names = NULL)
cat(sprintf("\n--- Bootstrap B = %d (validos %d) en %.1f min ---\n", B, length(boot), mins_boot))
print(tab, row.names = FALSE, digits = 2)
write.csv(tab, file.path(RESULTS_TABLES, "M1_bipartite_boot_pilot.csv"), row.names = FALSE)
cat(sprintf("\n  Extrapolación suite x7 (mismo B): ~%.0f min | B=500: ~%.0f min\n",
            7 * mins_boot * mean(table(registry$commission[registry$n_firmantes >= 2 & registry$n_firmantes <= 16 & nzchar(registry$commission)])) / nE,
            7 * mins_boot * 2.5 * mean(table(registry$commission[registry$n_firmantes >= 2 & registry$n_firmantes <= 16 & nzchar(registry$commission)])) / nE))
cat(sprintf("--- Done (%.1f min) ---\n", as.numeric(difftime(Sys.time(), T0, units = "mins"))))

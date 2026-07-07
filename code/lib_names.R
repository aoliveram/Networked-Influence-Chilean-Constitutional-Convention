# lib_names.R — Espejo R del módulo de normalización de nombres (P4).
# Requiere haber hecho source("code/paths.R") antes (usa MEMBERS).
# Los scripts R trabajan casi siempre con outputs ya canónicos del paso Python;
# este módulo cubre validación y joins contra el roster.

library(jsonlite)

NAME_CORRECTIONS <- c(
  "Muñoz, Pedro"        = "Munoz, Pedro",
  "Sepúlveda, Barbara"  = "Sepulveda, Barbara",
  "Sepúlveda, Carolina" = "Sepulveda, Carolina",
  "Vargas, Margaritfa"  = "Vargas, Margarita",
  "Chinga"              = "Chinga, Eric",
  "Dayyana, Gonzalez"   = "Gonzalez, Dayana",
  "Lidia, Gonzalez"     = "Gonzalez, Lidia"
)

CANONICAL <- fromJSON(MEMBERS)
stopifnot(length(CANONICAL) == 154)

norm_txt <- function(s) {
  s <- iconv(s, from = "UTF-8", to = "ASCII//TRANSLIT")
  tolower(trimws(gsub("\\s+", " ", s)))
}

.by_norm <- setNames(CANONICAL, norm_txt(CANONICAL))

normalize_author <- function(raw) {
  if (is.null(raw) || is.na(raw)) return(NA_character_)
  s <- trimws(gsub("\\s+", " ", raw))
  if (nchar(s) <= 1 || grepl("S/I", s, fixed = TRUE)) return(NA_character_)
  if (grepl("^[0-9]+(-[0-9]+)?$|iniciativa(s)? popular(es)?", s, ignore.case = TRUE)) return(NA_character_)
  if (s %in% names(NAME_CORRECTIONS)) s <- NAME_CORRECTIONS[[s]]
  hit <- .by_norm[norm_txt(s)]
  if (!is.na(hit)) return(unname(hit))
  NA_character_
}

validate_names <- function(x) {
  # devuelve los elementos de x que NO resuelven a un canónico
  x[is.na(vapply(x, normalize_author, character(1)))]
}

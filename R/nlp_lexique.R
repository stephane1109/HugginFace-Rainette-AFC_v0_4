# Module NLP - lemmatisation via lexique externe (lexique_fr.csv)
# Ce module charge un lexique 3 colonnes soit au format lexique_fr
# (ortho, Lexique4__Lemme, Lexique4__Cgram), soit au format IRaMuTeQ
# (c_mot, c_lemme, c_morpho).
# et applique une lemmatisation explicite sans fallback silencieux vers spaCy.

charger_lexique_fr <- function(chemin = "lexique_fr.csv") {
  fichier <- tryCatch(normalizePath(chemin, mustWork = TRUE), error = function(e) NA_character_)
  if (is.na(fichier) || !file.exists(fichier)) {
    stop(
      paste0(
        "Lexique (fr) introuvable : fichier '", chemin,
        "' absent. Ajoute lexique_fr.csv à la racine du projet (format lexique_fr ou IRaMuTeQ)."
      )
    )
  }

  lire_tsv <- function() {
    read.delim(
      fichier,
      sep = "\t",
      header = TRUE,
      stringsAsFactors = FALSE,
      fileEncoding = "UTF-8",
      check.names = FALSE
    )
  }

  lire_csv <- function() {
    read.csv(
      fichier,
      sep = ",",
      header = TRUE,
      stringsAsFactors = FALSE,
      fileEncoding = "UTF-8",
      check.names = FALSE
    )
  }

  lire_csv_point_virgule <- function() {
    read.csv(
      fichier,
      sep = ";",
      header = TRUE,
      stringsAsFactors = FALSE,
      fileEncoding = "UTF-8",
      check.names = FALSE
    )
  }

  lexique <- tryCatch(lire_tsv(), error = function(e) NULL)
  if (is.null(lexique) || ncol(lexique) <= 1) {
    lexique <- tryCatch(lire_csv(), error = function(e) NULL)
  }
  if (is.null(lexique) || ncol(lexique) <= 1) {
    lexique <- tryCatch(lire_csv_point_virgule(), error = function(e) NULL)
  }

  if (is.null(lexique) || nrow(lexique) == 0) {
    stop("Lexique (fr) invalide : fichier vide ou illisible.")
  }

  names(lexique) <- trimws(sub("^\ufeff", "", names(lexique)))

  # Compatibilité IRaMuTeQ : c_mot, c_lemme, c_morpho
  noms_orig <- names(lexique)
  noms_low <- tolower(noms_orig)
  idx_mot <- match("c_mot", noms_low)
  idx_lemme <- match("c_lemme", noms_low)
  idx_morpho <- match("c_morpho", noms_low)
  if (all(!is.na(c(idx_mot, idx_lemme, idx_morpho)))) {
    names(lexique)[idx_mot] <- "ortho"
    names(lexique)[idx_lemme] <- "Lexique4__Lemme"
    names(lexique)[idx_morpho] <- "Lexique4__Cgram"
  }

  colonnes_attendues <- c("ortho", "Lexique4__Lemme", "Lexique4__Cgram")
  manquantes <- setdiff(colonnes_attendues, names(lexique))
  if (length(manquantes) > 0) {
    stop(
      paste0(
        "Lexique (fr) mal configuré : colonnes manquantes [",
        paste(manquantes, collapse = ", "),
        "]. Formats acceptés : ",
        "lexique_fr (ortho, Lexique4__Lemme, Lexique4__Cgram) ou ",
        "IRaMuTeQ (c_mot, c_lemme, c_morpho)."
      )
    )
  }

  lexique$ortho <- tolower(trimws(as.character(lexique$ortho)))
  lexique$Lexique4__Lemme <- tolower(trimws(as.character(lexique$Lexique4__Lemme)))
  lexique$Lexique4__Cgram <- toupper(trimws(as.character(lexique$Lexique4__Cgram)))

  lexique <- lexique[
    nzchar(lexique$ortho) &
      nzchar(lexique$Lexique4__Lemme) &
      nzchar(lexique$Lexique4__Cgram),
    ,
    drop = FALSE
  ]

  if (nrow(lexique) == 0) {
    stop("Lexique (fr) mal configuré : aucune entrée exploitable après nettoyage.")
  }

  lexique
}

lemmatiser_textes_lexique <- function(textes, lexique, rv = NULL) {
  tok <- quanteda::tokens(
    textes,
    remove_punct = FALSE,
    remove_numbers = FALSE
  )

  map_forme_lemme <- tapply(
    lexique$Lexique4__Lemme,
    lexique$ortho,
    function(x) unique(x)[1]
  )

  liste_tok <- as.list(tok)
  textes_lem <- vapply(liste_tok, function(v) {
    if (length(v) == 0) return("")
    v_low <- tolower(as.character(v))
    lem <- unname(map_forme_lemme[v_low])
    lem[is.na(lem) | !nzchar(lem)] <- v_low[is.na(lem) | !nzchar(lem)]
    paste(lem, collapse = " ")
  }, FUN.VALUE = character(1))

  if (!is.null(rv)) {
    ajouter_log(rv, "Lexique (fr) : lemmatisation forme->lemme appliquée sans spaCy.")
  }

  textes_lem
}

filtrer_textes_lexique_par_cgram <- function(textes, lexique, cgram_a_conserver, rv = NULL) {
  cgram_keep <- unique(toupper(trimws(as.character(cgram_a_conserver))))
  cgram_keep <- cgram_keep[nzchar(cgram_keep)]
  if (length(cgram_keep) == 0) return(textes)

  formes_keep <- unique(lexique$ortho[lexique$Lexique4__Cgram %in% cgram_keep])
  formes_keep <- formes_keep[nzchar(formes_keep)]

  tok <- quanteda::tokens(
    textes,
    remove_punct = FALSE,
    remove_numbers = FALSE
  )

  liste_tok <- as.list(tok)
  total_tokens <- 0L
  total_conserves <- 0L

  textes_filtres <- vapply(liste_tok, function(v) {
    if (length(v) == 0) return("")
    v_low <- tolower(trimws(as.character(v)))
    total_tokens <<- total_tokens + length(v_low)
    garder <- v_low %in% formes_keep
    total_conserves <<- total_conserves + sum(garder)
    paste(v_low[garder], collapse = " ")
  }, FUN.VALUE = character(1))

  if (!is.null(rv)) {
    ajouter_log(
      rv,
      paste0(
        "Lexique (fr) : filtrage Cgram [", paste(cgram_keep, collapse = ", "),
        "] => ", total_conserves, "/", total_tokens, " token(s) conservé(s)."
      )
    )
  }

  names(textes_filtres) <- names(textes)
  textes_filtres
}

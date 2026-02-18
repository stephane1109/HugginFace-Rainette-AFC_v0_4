# Module NLP - lemmatisation via lexique externe (OpenLexicon.csv)
# Ce module charge un lexique 3 colonnes (ortho, Lexique4__Lemme, Lexique4__Cgram)
# et applique une lemmatisation explicite sans fallback silencieux vers spaCy.

charger_lexique_fr <- function(chemin = "OpenLexicon.csv") {
  fichier <- tryCatch(normalizePath(chemin, mustWork = TRUE), error = function(e) NA_character_)
  if (is.na(fichier) || !file.exists(fichier)) {
    stop(
      paste0(
        "Lexique (fr) introuvable : fichier '", chemin,
        "' absent. Ajoute OpenLexicon.csv à la racine du projet."
      )
    )
  }

  lire_lignes <- function() {
    con <- file(fichier, open = "rb")
    on.exit(close(con), add = TRUE)
    lignes <- readLines(con, warn = FALSE)
    lignes <- enc2utf8(lignes)
    lignes
  }

  parser_lexique <- function(lignes, sep) {
    tc <- textConnection(lignes)
    on.exit(close(tc), add = TRUE)
    read.table(
      tc,
      sep = sep,
      header = TRUE,
      stringsAsFactors = FALSE,
      check.names = FALSE,
      quote = "\"",
      comment.char = "",
      fill = TRUE
    )
  }

  lignes <- lire_lignes()
  if (length(lignes) == 0) {
    stop("Lexique (fr) invalide : fichier vide ou illisible.")
  }

  en_tete <- lignes[[1]]
  nb_tabs <- lengths(regmatches(en_tete, gregexpr("\t", en_tete, perl = TRUE)))
  nb_virgules <- lengths(regmatches(en_tete, gregexpr(",", en_tete, perl = TRUE)))
  sep <- if (isTRUE(nb_tabs >= nb_virgules)) "\t" else ","

  lexique <- tryCatch(
    parser_lexique(lignes, sep = sep),
    error = function(e) {
      stop(paste0("Lexique (fr) invalide : impossible de parser le fichier (", e$message, ")."))
    }
  )

  if (is.null(lexique) || nrow(lexique) == 0) {
    stop("Lexique (fr) invalide : fichier vide ou illisible.")
  }

  colonnes_attendues <- c("ortho", "Lexique4__Lemme", "Lexique4__Cgram")
  manquantes <- setdiff(colonnes_attendues, names(lexique))
  if (length(manquantes) > 0) {
    stop(
      paste0(
        "Lexique (fr) mal configuré : colonnes manquantes [",
        paste(manquantes, collapse = ", "),
        "]. Colonnes attendues : ",
        paste(colonnes_attendues, collapse = ", "),
        "."
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

lemmatiser_tokens_spacy_avec_lexique <- function(tokens_df, lexique, rv = NULL) {
  if (is.null(tokens_df) || nrow(tokens_df) == 0) {
    return(list(textes = setNames(character(0), character(0)), tokens_df = tokens_df, n_sans_lemme = 0L))
  }

  df_tok <- tokens_df
  df_tok$doc_id <- as.character(df_tok$doc_id)
  df_tok$token <- tolower(trimws(as.character(df_tok$token)))
  df_tok$pos <- toupper(trimws(as.character(df_tok$pos)))

  cle_lex <- paste(lexique$ortho, lexique$Lexique4__Cgram, sep = "\t")
  map_cle_lemme <- tapply(
    lexique$Lexique4__Lemme,
    cle_lex,
    function(x) unique(x)[1]
  )

  cle_tok <- paste(df_tok$token, df_tok$pos, sep = "\t")
  lem <- unname(map_cle_lemme[cle_tok])

  sans_lemme <- is.na(lem) | !nzchar(lem)
  lem[sans_lemme] <- df_tok$token[sans_lemme]

  df_tok$lemma_lexique <- lem

  agg <- split(df_tok$lemma_lexique, df_tok$doc_id)
  textes <- vapply(agg, function(v) paste(v, collapse = " "), FUN.VALUE = character(1))

  if (!is.null(rv)) {
    ajouter_log(
      rv,
      paste0(
        "Lexique (fr) + POS spaCy : ",
        sum(sans_lemme),
        " token(s) sans entrée lexicale (conservés en forme de surface)."
      )
    )
  }

  list(textes = textes, tokens_df = df_tok, n_sans_lemme = as.integer(sum(sans_lemme)))
}

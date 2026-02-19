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

mapper_pos_spacy_vers_lexique <- function(pos_spacy) {
  pos <- toupper(trimws(as.character(pos_spacy)))

  map <- list(
    ADJ = c("ADJ", "ADJ:NUM", "ADJ:POS", "ADJ:IND", "ADJ:INT", "ADJ:DEM"),
    ADP = c("PRE"),
    ADV = c("ADV"),
    AUX = c("AUX", "VER"),
    CCONJ = c("CON"),
    DET = c("ART:DEF", "ART:IND", "ADJ:POS", "ADJ:DEM", "ADJ:IND", "ADJ:INT"),
    INTJ = c("ONO"),
    NOUN = c("NOM"),
    NUM = c("ADJ:NUM"),
    PRON = c("PRO:PER", "PRO:POS", "PRO:DEM", "PRO:IND", "PRO:REL", "PRO:INT"),
    PROPN = c("NOM"),
    SCONJ = c("CON"),
    VERB = c("VER", "AUX")
  )

  out <- lapply(pos, function(tag) {
    candidats <- map[[tag]]
    if (is.null(candidats) || length(candidats) == 0) return(tag)
    unique(candidats)
  })

  names(out) <- pos
  out
}

filtrer_tokens_lexique_par_cgram <- function(tokens_df, lexique, cgram_a_conserver, rv = NULL) {
  if (is.null(tokens_df) || nrow(tokens_df) == 0) return(tokens_df)

  cgram_keep <- unique(toupper(trimws(as.character(cgram_a_conserver))))
  cgram_keep <- cgram_keep[nzchar(cgram_keep)]
  if (length(cgram_keep) == 0) return(tokens_df)

  formes_keep <- unique(lexique$ortho[lexique$Lexique4__Cgram %in% cgram_keep])
  formes_keep <- formes_keep[nzchar(formes_keep)]

  tok <- tokens_df
  tok$token <- tolower(trimws(as.character(tok$token)))
  garder <- tok$token %in% formes_keep

  if (!is.null(rv)) {
    ajouter_log(
      rv,
      paste0(
        "Lexique (fr) : filtrage Cgram [", paste(cgram_keep, collapse = ", "),
        "] => ", sum(garder), "/", nrow(tok), " token(s) conservé(s)."
      )
    )
  }

  tok[garder, , drop = FALSE]
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

  pos_mappes <- mapper_pos_spacy_vers_lexique(df_tok$pos)
  lem <- vapply(seq_len(nrow(df_tok)), function(i) {
    candidats <- pos_mappes[[i]]
    cles <- paste(df_tok$token[i], candidats, sep = "\t")
    trouves <- unname(map_cle_lemme[cles])
    trouves <- trouves[!is.na(trouves) & nzchar(trouves)]
    if (length(trouves) == 0) return(NA_character_)
    trouves[1]
  }, FUN.VALUE = character(1))

  sans_lemme <- is.na(lem) | !nzchar(lem)
  lem[sans_lemme] <- df_tok$token[sans_lemme]

  df_tok$lemma_lexique <- lem

  agg <- split(df_tok$lemma_lexique, df_tok$doc_id)
  textes <- vapply(agg, function(v) paste(v, collapse = " "), FUN.VALUE = character(1))

  if (!is.null(rv)) {
    ajouter_log(
      rv,
      paste0(
        "Lexique (fr) : ",
        sum(sans_lemme),
        " token(s) sans entrée lexicale (conservés en forme de surface)."
      )
    )
  }

  list(textes = textes, tokens_df = df_tok, n_sans_lemme = as.integer(sum(sans_lemme)))
}

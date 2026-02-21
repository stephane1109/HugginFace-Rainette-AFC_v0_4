# concordancier.R

horodater <- function() format(Sys.time(), "%Y-%m-%d %H:%M:%S")

ajouter_log <- function(rv, texte) {
  rv$logs <- paste(rv$logs, paste0("[", horodater(), "] ", texte), sep = "\n")
}

trans_nfd <- function(x) {
  if (requireNamespace("stringi", quietly = TRUE)) return(stringi::stri_trans_nfd(x))
  x
}

trans_nfc <- function(x) {
  if (requireNamespace("stringi", quietly = TRUE)) return(stringi::stri_trans_nfc(x))
  x
}

echapper_regex <- function(x) {
  gsub("([\\^\\$\\*\\+\\?\\(\\)\\[\\]\\{\\}\\.\\|\\\\\\-])", "\\\\\\1", x)
}

normaliser_id_classe <- function(x) {
  x_chr <- as.character(x)
  x_chr <- trimws(x_chr)

  x_num <- suppressWarnings(as.numeric(x_chr))
  need_extract <- is.na(x_num) & nzchar(x_chr)

  if (any(need_extract)) {
    extrait <- sub("^.*?(\\d+).*$", "\\1", x_chr[need_extract])
    extrait[!grepl("\\d", x_chr[need_extract])] <- NA_character_
    x_num[need_extract] <- suppressWarnings(as.numeric(extrait))
  }

  x_num
}

expandir_variantes_termes <- function(termes) {
  termes <- as.character(termes)
  termes <- termes[!is.na(termes) & nzchar(termes)]
  if (length(termes) == 0) return(character(0))

  v <- unique(c(
    termes,
    gsub("_", " ", termes, fixed = TRUE),
    gsub(" ", "_", termes, fixed = TRUE)
  ))

  v[!is.na(v) & nzchar(v)]
}

construire_regex_terme_nfd <- function(terme) {
  if (is.na(terme) || !nzchar(terme)) return("")

  t <- tolower(trans_nfd(terme))
  chars <- strsplit(t, "", fixed = TRUE)[[1]]
  if (length(chars) == 0) return("")

  pieces <- vapply(chars, function(ch) {
    if (grepl("\\p{M}", ch, perl = TRUE)) return("")
    if (ch %in% c(" ", "\t", "\n", "\r")) return("\\s+")
    if (ch %in% c("'", "’")) return("['’]")

    if (grepl("\\p{L}", ch, perl = TRUE)) {
      return(paste0(echapper_regex(ch), "\\p{M}*"))
    }

    echapper_regex(ch)
  }, FUN.VALUE = character(1))

  paste0(pieces, collapse = "")
}

preparer_motifs_surlignage_nfd <- function(terms, taille_lot = 200) {
  terms <- unique(terms)
  terms <- terms[!is.na(terms) & nzchar(terms)]
  if (length(terms) == 0) return(list())

  terms <- terms[order(nchar(terms), decreasing = TRUE)]
  patterns <- vapply(terms, construire_regex_terme_nfd, FUN.VALUE = character(1))
  patterns <- patterns[nzchar(patterns)]
  if (length(patterns) == 0) return(list())

  lots <- split(patterns, ceiling(seq_along(patterns) / taille_lot))

  lapply(lots, function(lot) {
    paste0(
      "(*UCP)(?i)(?<![\\p{L}\\p{M}])(",
      paste0(lot, collapse = "|"),
      ")(?![\\p{L}\\p{M}])"
    )
  })
}

surligner_vecteur_html_unicode <- function(segments, motifs, start_tag, end_tag) {
  if (length(segments) == 0 || length(motifs) == 0) return(segments)

  out <- segments
  for (i in seq_along(out)) {
    s <- out[i]
    if (is.na(s) || !nzchar(s)) next

    s_nfd <- trans_nfd(s)

    for (pat in motifs) {
      s_nfd <- tryCatch(
        gsub(pat, paste0(start_tag, "\\1", end_tag), s_nfd, perl = TRUE),
        error = function(e) s_nfd
      )
    }

    out[i] <- trans_nfc(s_nfd)
  }
  out
}

detecter_segments_contenant_termes_unicode <- function(textes_index, termes) {
  if (length(textes_index) == 0) return(logical(0))

  termes <- unique(termes)
  termes <- termes[!is.na(termes) & nzchar(termes)]
  if (length(termes) == 0) return(rep(TRUE, length(textes_index)))

  motifs <- preparer_motifs_surlignage_nfd(termes, taille_lot = 200)
  if (length(motifs) == 0) return(rep(TRUE, length(textes_index)))

  present <- rep(FALSE, length(textes_index))
  textes_nfd <- trans_nfd(textes_index)

  for (pat in motifs) {
    ok <- tryCatch(grepl(pat, textes_nfd, perl = TRUE), error = function(e) rep(FALSE, length(textes_nfd)))
    present <- present | ok
  }

  present
}

generer_concordancier_html <- function(
  chemin_sortie,
  segments_by_class,
  res_stats_df,
  max_p,
  textes_indexation,
  spacy_tokens_df,
  explor_assets = NULL,
  avancer = NULL,
  rv = NULL,
  ...
) {
  if (!is.null(rv)) ajouter_log(rv, "Concordancier : génération HTML (filtré + surlignage Unicode).")

  con <- file(chemin_sortie, open = "wt", encoding = "UTF-8")
  on.exit(try(close(con), silent = TRUE), add = TRUE)

  writeLines("<html><head><meta charset='utf-8'/>", con)
  writeLines("<style>body{font-family:Arial,sans-serif;} span.highlight{background-color:yellow;}</style>", con)
  writeLines("</head><body>", con)

  writeLines("<h1>Concordancier Rainette</h1>", con)

  writeLines("<h2>Segments par classe</h2>", con)
  writeLines("<h3>Segments par classe (filtrés sur présence de termes significatifs)</h3>", con)

  noms_classes <- names(segments_by_class)
  n_classes <- length(noms_classes)
  if (n_classes == 0) n_classes <- 1

  for (i in seq_along(noms_classes)) {
    cl <- noms_classes[i]

    if (!is.null(avancer)) avancer(0.75 + (i / n_classes) * 0.08, paste0("HTML : classe ", cl))
    writeLines(paste0("<h2>Classe ", cl, "</h2>"), con)

    cl_num <- normaliser_id_classe(cl)

    classes_stats <- normaliser_id_classe(res_stats_df$Classe)
    idx_cl <- !is.na(classes_stats) & !is.na(cl_num) & classes_stats == cl_num

    df_cl <- res_stats_df[idx_cl, , drop = FALSE]

    p_col <- if ("p" %in% names(df_cl)) "p" else if ("p_value" %in% names(df_cl)) "p_value" else NULL
    idx_p <- rep(TRUE, nrow(df_cl))
    if (!is.null(p_col)) idx_p <- !is.na(df_cl[[p_col]]) & df_cl[[p_col]] <= max_p

    idx_pos <- rep(TRUE, nrow(df_cl))
    if ("chi2" %in% names(df_cl)) {
      idx_pos <- !is.na(df_cl$chi2) & df_cl$chi2 > 0
    } else if ("lr" %in% names(df_cl)) {
      idx_pos <- !is.na(df_cl$lr) & df_cl$lr > 0
    }

    termes_cl <- df_cl$Terme[idx_p & idx_pos]
    if (length(termes_cl) == 0) {
      termes_cl <- df_cl$Terme[idx_p]
    }
    if (length(termes_cl) == 0) {
      termes_cl <- df_cl$Terme
    }

    termes_cl <- unique(termes_cl)
    termes_cl <- termes_cl[!is.na(termes_cl) & nzchar(termes_cl)]

    segments <- segments_by_class[[cl]]
    ids_cl <- names(segments)

    if (length(ids_cl) == 0) {
      writeLines("<p><em>Aucun segment.</em></p>", con)
      next
    }

    # Important : le filtrage doit se faire sur les textes d'indexation (lemmatisés / normalisés)
    # pour rester cohérent avec les termes issus des stats Rainette.
    textes_filtrage <- unname(segments)
    if (!is.null(textes_indexation) && length(textes_indexation) > 0) {
      tx <- textes_indexation[ids_cl]
      ok_tx <- !is.na(tx) & nzchar(tx)
      if (any(ok_tx)) {
        textes_filtrage[ok_tx] <- tx[ok_tx]
      }
    }

    tokens_surface <- character(0)
    if (!is.null(spacy_tokens_df) && nrow(spacy_tokens_df) > 0 && length(ids_cl) > 0) {
      df_tok <- spacy_tokens_df
      df_tok$doc_id <- as.character(df_tok$doc_id)
      df_tok <- df_tok[df_tok$doc_id %in% ids_cl, , drop = FALSE]
      if (nrow(df_tok) > 0) {
        tokens_surface <- unique(df_tok$token[df_tok$lemma %in% termes_cl | df_tok$token %in% termes_cl])
        tokens_surface <- tokens_surface[!is.na(tokens_surface) & nzchar(tokens_surface)]
      }
    }

    termes_a_surligner <- unique(c(tokens_surface, termes_cl))
    termes_a_surligner <- expandir_variantes_termes(termes_a_surligner)

    keep <- detecter_segments_contenant_termes_unicode(textes_filtrage, termes_a_surligner)
    segments_keep <- segments[keep]

    writeLines(paste0("<p><em>Segments conservés : ", length(segments_keep), " / ", length(segments), "</em></p>"), con)

    if (length(termes_a_surligner) == 0 || length(segments_keep) == 0) {
      writeLines("<p><em>Aucun segment ne contient de terme significatif pour cette classe avec les paramètres courants.</em></p>", con)
      next
    }

    motifs <- preparer_motifs_surlignage_nfd(termes_a_surligner, taille_lot = 160)

    segments_hl <- surligner_vecteur_html_unicode(
      unname(segments_keep),
      motifs,
      "<span class='highlight'>",
      "</span>"
    )

    # Si les termes sont surtout visibles dans l'espace d'indexation (lemmes),
    # le surlignage peut être vide sur le texte brut. Fallback d'affichage :
    # surligner la version indexée pour conserver un concordancier lisible.
    has_hl <- any(grepl("<span class='highlight'>", segments_hl, fixed = TRUE))
    if (!has_hl) {
      textes_keep_idx <- textes_filtrage[keep]
      segments_hl_idx <- surligner_vecteur_html_unicode(
        unname(textes_keep_idx),
        motifs,
        "<span class='highlight'>",
        "</span>"
      )
      has_hl_idx <- any(grepl("<span class='highlight'>", segments_hl_idx, fixed = TRUE))
      if (has_hl_idx) segments_hl <- segments_hl_idx
    }

    for (seg in segments_hl) writeLines(paste0("<p>", seg, "</p>"), con)
  }

  writeLines("</body></html>", con)
  close(con)

  if (!is.null(rv)) ajouter_log(rv, paste0("Concordancier : HTML écrit dans : ", chemin_sortie))
  chemin_sortie
}

# concordancier_spacy.R

horodater_spacy <- function() format(Sys.time(), "%Y-%m-%d %H:%M:%S")

ajouter_log_spacy <- function(rv, texte) {
  rv$logs <- paste(rv$logs, paste0("[", horodater_spacy(), "] ", texte), sep = "\n")
}

trans_nfd_spacy <- function(x) {
  if (requireNamespace("stringi", quietly = TRUE)) return(stringi::stri_trans_nfd(x))
  x
}

trans_nfc_spacy <- function(x) {
  if (requireNamespace("stringi", quietly = TRUE)) return(stringi::stri_trans_nfc(x))
  x
}

echapper_regex_spacy <- function(x) {
  gsub("([\\^\\$\\*\\+\\?\\(\\)\\[\\]\\{\\}\\.\\|\\\\\\-])", "\\\\\\1", x)
}

echapper_html_spacy <- function(x) {
  x <- gsub("&", "&amp;", x, fixed = TRUE)
  x <- gsub("<", "&lt;", x, fixed = TRUE)
  x <- gsub(">", "&gt;", x, fixed = TRUE)
  x
}

expandir_variantes_termes_spacy <- function(termes) {
  termes <- unique(as.character(termes))
  termes <- trimws(termes)
  termes <- termes[!is.na(termes) & nzchar(termes)]
  if (length(termes) == 0) return(character(0))

  variantes <- unique(c(
    termes,
    gsub("’", "'", termes, fixed = TRUE),
    gsub("'", "’", termes, fixed = TRUE)
  ))

  variantes[!is.na(variantes) & nzchar(variantes)]
}

echapper_segments_en_preservant_surlignage_spacy <- function(segments, start_tag, end_tag) {
  if (length(segments) == 0) return(segments)

  start_placeholder <- "__RAINETTE_HL_START__"
  end_placeholder <- "__RAINETTE_HL_END__"

  x <- gsub(start_tag, start_placeholder, segments, fixed = TRUE)
  x <- gsub(end_tag, end_placeholder, x, fixed = TRUE)
  x <- echapper_html_spacy(x)
  x <- gsub(start_placeholder, start_tag, x, fixed = TRUE)
  x <- gsub(end_placeholder, end_tag, x, fixed = TRUE)
  x <- gsub("&lt;span class='highlight'&gt;", start_tag, x, fixed = TRUE)
  x <- gsub("&lt;span class=\"highlight\"&gt;", start_tag, x, fixed = TRUE)
  x <- gsub("&lt;/span&gt;", end_tag, x, fixed = TRUE)
  x
}

normaliser_id_classe_spacy <- function(x) {
  x_chr <- as.character(x)
  x_chr <- trimws(x_chr)
  x_num <- suppressWarnings(as.numeric(x_chr))

  need_extract <- is.na(x_num) & !is.na(x_chr) & nzchar(x_chr)
  if (any(need_extract)) {
    extrait <- sub("^.*?(\\d+).*$", "\\1", x_chr[need_extract])
    extrait[!grepl("\\d", x_chr[need_extract])] <- NA_character_
    x_num[need_extract] <- suppressWarnings(as.numeric(extrait))
  }

  x_num
}

construire_regex_terme_nfd_spacy <- function(terme) {
  if (is.na(terme) || !nzchar(terme)) return("")

  t <- tolower(trans_nfd_spacy(terme))
  chars <- strsplit(t, "", fixed = TRUE)[[1]]
  if (length(chars) == 0) return("")

  pieces <- vapply(chars, function(ch) {
    if (grepl("\\p{M}", ch, perl = TRUE)) return("")
    if (ch %in% c(" ", "\t", "\n", "\r")) return("\\s+")
    if (ch %in% c("'", "’")) return("['’]")

    if (grepl("\\p{L}", ch, perl = TRUE)) {
      return(paste0(echapper_regex_spacy(ch), "\\p{M}*"))
    }

    echapper_regex_spacy(ch)
  }, FUN.VALUE = character(1))

  paste0(pieces, collapse = "")
}

motif_unicode_est_valide_spacy <- function(pat) {
  if (is.na(pat) || !nzchar(pat)) return(FALSE)
  ok <- tryCatch({
    grepl(pat, "", perl = TRUE)
    TRUE
  }, error = function(e) FALSE)
  ok
}

construire_motif_terme_valide_spacy <- function(pat_terme) {
  candidats <- c(
    paste0("(*UTF)(*UCP)(?i)(?<![\\p{L}\\p{M}])(", pat_terme, ")(?![\\p{L}\\p{M}])"),
    paste0("(*UCP)(?i)(?<![\\p{L}\\p{M}])(", pat_terme, ")(?![\\p{L}\\p{M}])"),
    paste0("(?i)(", pat_terme, ")")
  )

  for (cand in candidats) {
    if (motif_unicode_est_valide_spacy(cand)) return(cand)
  }
  ""
}

preparer_motifs_surlignage_nfd_spacy <- function(terms, taille_lot = 200) {
  terms <- unique(terms)
  terms <- terms[!is.na(terms) & nzchar(terms)]
  if (length(terms) == 0) return(list())

  terms <- terms[order(nchar(terms), decreasing = TRUE)]
  patterns <- vapply(terms, construire_regex_terme_nfd_spacy, FUN.VALUE = character(1))
  patterns <- patterns[nzchar(patterns)]
  if (length(patterns) == 0) return(list())

  motifs <- vapply(patterns, construire_motif_terme_valide_spacy, FUN.VALUE = character(1))
  motifs <- motifs[nzchar(motifs)]
  as.list(unique(motifs))
}

surligner_vecteur_html_unicode_spacy <- function(segments, motifs, start_tag, end_tag, on_error = NULL) {
  if (length(segments) == 0 || length(motifs) == 0) return(segments)

  out <- segments
  for (i in seq_along(out)) {
    s <- out[i]
    if (is.na(s) || !nzchar(s)) next

    s_nfd <- trans_nfd_spacy(s)

    for (pat in motifs) {
      s_nfd <- tryCatch(
        gsub(pat, paste0(start_tag, "\\1", end_tag), s_nfd, perl = TRUE),
        error = function(e) {
          if (is.function(on_error)) on_error(e, pat)
          s_nfd
        }
      )
    }

    out[i] <- trans_nfc_spacy(s_nfd)
  }
  out
}

detecter_segments_contenant_termes_unicode_spacy <- function(textes_index, termes) {
  if (length(textes_index) == 0) return(logical(0))

  termes <- unique(termes)
  termes <- termes[!is.na(termes) & nzchar(termes)]
  if (length(termes) == 0) return(rep(TRUE, length(textes_index)))

  motifs <- preparer_motifs_surlignage_nfd_spacy(termes, taille_lot = 200)
  if (length(motifs) == 0) return(rep(TRUE, length(textes_index)))

  present <- rep(FALSE, length(textes_index))
  textes_nfd <- trans_nfd_spacy(textes_index)

  for (pat in motifs) {
    ok <- tryCatch(grepl(pat, textes_nfd, perl = TRUE), error = function(e) rep(FALSE, length(textes_nfd)))
    present <- present | ok
  }

  present
}

# concordancier_spacy.R

generer_concordancier_spacy_html <- function(
  chemin_sortie,
  segments_by_class,
  res_stats_df,
  max_p,
  textes_indexation,
  spacy_tokens_df,
  avancer = NULL,
  rv = NULL,
  ...
) {
  source_dictionnaire <- "spacy"
  if (!is.null(rv)) ajouter_log_spacy(rv, "Concordancier spaCy : génération HTML (filtré + surlignage Unicode).")

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

    cl_num <- normaliser_id_classe_spacy(cl)
    classes_stats <- normaliser_id_classe_spacy(res_stats_df$Classe)
    idx_cl <- !is.na(classes_stats) & !is.na(cl_num) & classes_stats == cl_num

    if (!"p_value" %in% names(res_stats_df)) {
      if (!is.null(rv)) ajouter_log_spacy(rv, "Concordancier spaCy : colonne p_value absente dans res_stats_df.")
      writeLines("<p><em>Erreur : colonne p_value absente dans les statistiques.</em></p>", con)
      next
    }

    idx_sig <- idx_cl & !is.na(res_stats_df$p_value) & res_stats_df$p_value <= max_p
    termes_cl <- unique(res_stats_df$Terme[idx_sig])
    termes_cl <- termes_cl[!is.na(termes_cl) & nzchar(termes_cl)]

    if (length(termes_cl) == 0) {
      idx_top <- idx_cl & !is.na(res_stats_df$Terme) & nzchar(as.character(res_stats_df$Terme))
      if (any(idx_top)) {
        df_top <- res_stats_df[idx_top, , drop = FALSE]
        if ("chi2" %in% names(df_top)) df_top <- df_top[order(-df_top$chi2), , drop = FALSE]
        termes_cl <- unique(head(as.character(df_top$Terme), 20))
        termes_cl <- termes_cl[!is.na(termes_cl) & nzchar(termes_cl)]
        if (!is.null(rv)) {
          ajouter_log_spacy(rv, paste0(
            "Concordancier spaCy : classe ", cl,
            " sans terme p<=", max_p,
            ", fallback sur top chi2 (", length(termes_cl), " termes)."
          ))
        }
      }

      if (length(termes_cl) == 0) {
        idx_top_global <- !is.na(res_stats_df$Terme) & nzchar(as.character(res_stats_df$Terme))
        if (any(idx_top_global)) {
          df_top_global <- res_stats_df[idx_top_global, , drop = FALSE]
          if ("chi2" %in% names(df_top_global)) {
            df_top_global <- df_top_global[order(-df_top_global$chi2), , drop = FALSE]
          }
          termes_cl <- unique(head(as.character(df_top_global$Terme), 20))
          termes_cl <- termes_cl[!is.na(termes_cl) & nzchar(termes_cl)]
        }
      }
    }

    segments <- segments_by_class[[cl]]
    ids_cl <- names(segments)
    if (length(ids_cl) == 0) {
      writeLines("<p><em>Aucun segment.</em></p>", con)
      next
    }

    textes_filtrage <- unname(segments)
    if (!is.null(textes_indexation) && length(textes_indexation) > 0) {
      tx <- textes_indexation[ids_cl]
      ok_tx <- !is.na(tx) & nzchar(tx)
      if (any(ok_tx)) textes_filtrage[ok_tx] <- tx[ok_tx]
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

    termes_a_surligner <- expandir_variantes_termes_spacy(unique(c(tokens_surface, termes_cl)))
    keep <- detecter_segments_contenant_termes_unicode_spacy(textes_filtrage, termes_a_surligner)
    keep[is.na(keep)] <- FALSE
    segments_keep <- segments[keep]

    if (length(segments) > 0 && sum(keep, na.rm = TRUE) == 0) {
      segments_keep <- segments
      if (!is.null(rv)) {
        ajouter_log_spacy(rv, paste0(
          "Concordancier spaCy : classe ", cl,
          " sans segment après filtrage, fallback sur tous les segments de la classe."
        ))
      }
    }

    if (length(segments_keep) == 0 && length(segments) > 0) {
      segments_keep <- segments
      if (!is.null(rv)) {
        ajouter_log_spacy(rv, paste0(
          "Concordancier spaCy : garde-fou activé pour la classe ", cl,
          " : affichage de tous les segments."
        ))
      }
    }

    if (length(segments_keep) == 0 && length(segments) > 0) {
      segments_keep <- segments
    }

    if (!is.null(rv)) {
      ajouter_log_spacy(rv, paste0(
        "Concordancier spaCy : classe ", cl,
        " | segments=", length(segments),
        " | keep=", length(segments_keep)
      ))
    }

    writeLines(paste0("<p><em>Segments conservés : ", length(segments_keep), " / ", length(segments), "</em></p>"), con)
    if (length(segments_keep) == 0) {
      writeLines("<p><em>Aucun segment ne contient de terme significatif pour cette classe avec les paramètres courants.</em></p>", con)
      next
    }

    if (length(termes_a_surligner) == 0) {
      for (seg in echapper_segments_en_preservant_surlignage_spacy(unname(segments_keep), "<span class='highlight'>", "</span>")) writeLines(paste0("<p>", seg, "</p>"), con)
      next
    }

    motifs <- preparer_motifs_surlignage_nfd_spacy(termes_a_surligner, taille_lot = 160)
    log_regex <- function(e, pat) {
      if (!is.null(rv)) {
        ajouter_log_spacy(rv, paste0("Concordancier spaCy : erreur regex sur motif [", pat, "] - ", conditionMessage(e)))
      }
    }

    segments_hl <- surligner_vecteur_html_unicode_spacy(
      unname(segments_keep),
      motifs,
      "<span class='highlight'>",
      "</span>",
      on_error = log_regex
    )

    has_hl <- any(grepl("<span class='highlight'>", segments_hl, fixed = TRUE))
    if (!has_hl) {
      textes_keep_idx <- textes_filtrage[keep]
      segments_hl_idx <- surligner_vecteur_html_unicode_spacy(
        unname(textes_keep_idx),
        motifs,
        "<span class='highlight'>",
        "</span>",
        on_error = log_regex
      )
      has_hl_idx <- any(grepl("<span class='highlight'>", segments_hl_idx, fixed = TRUE))
      if (has_hl_idx) segments_hl <- segments_hl_idx
    }

    if (length(segments_hl) == 0 && length(segments_keep) > 0) segments_hl <- unname(segments_keep)

    for (seg in echapper_segments_en_preservant_surlignage_spacy(segments_hl, "<span class='highlight'>", "</span>")) writeLines(paste0("<p>", seg, "</p>"), con)
  }

  writeLines("</body></html>", con)
  close(con)
  if (!is.null(rv)) ajouter_log_spacy(rv, paste0("Concordancier spaCy : HTML écrit dans : ", chemin_sortie))
  chemin_sortie
}

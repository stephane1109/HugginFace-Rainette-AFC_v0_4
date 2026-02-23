# concordancier_lexique.R

generer_concordancier_lexique_html <- function(
  chemin_sortie,
  segments_by_class,
  res_stats_df,
  max_p,
  textes_indexation,
  spacy_tokens_df = NULL,
  avancer = NULL,
  rv = NULL,
  ...
) {
  source_dictionnaire <- "lexique_fr"
  if (!is.null(rv)) ajouter_log(rv, "Concordancier lexique_fr : génération HTML.")

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

    if (!"p_value" %in% names(res_stats_df)) {
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

    termes_a_surligner <- expandir_variantes_termes(termes_cl)
    keep <- detecter_segments_contenant_termes_unicode(textes_filtrage, termes_a_surligner)
    keep[is.na(keep)] <- FALSE
    segments_keep <- segments[keep]
    if (length(segments_keep) == 0 && length(segments) > 0) segments_keep <- segments

    writeLines(paste0("<p><em>Segments conservés : ", length(segments_keep), " / ", length(segments), "</em></p>"), con)
    if (length(segments_keep) == 0) {
      writeLines("<p><em>Aucun segment ne contient de terme significatif pour cette classe avec les paramètres courants.</em></p>", con)
      next
    }

    if (length(termes_a_surligner) == 0) {
      for (seg in echapper_segments_en_preservant_surlignage(unname(segments_keep), "<span class='highlight'>", "</span>")) writeLines(paste0("<p>", seg, "</p>"), con)
      next
    }

    motifs <- preparer_motifs_surlignage_nfd(termes_a_surligner, taille_lot = 160)

    log_regex <- function(e, pat) {
      if (!is.null(rv)) {
        ajouter_log(rv, paste0("Concordancier : erreur regex sur motif [", pat, "] - ", conditionMessage(e)))
      }
    }

    segments_hl <- surligner_vecteur_html_unicode(
      unname(segments_keep),
      motifs,
      "<span class='highlight'>",
      "</span>",
      on_error = log_regex
    )

    has_hl <- any(grepl("<span class='highlight'>", segments_hl, fixed = TRUE))
    if (!has_hl) {
      textes_keep_idx <- textes_filtrage[keep]
      if (length(textes_keep_idx) == length(segments_keep)) {
        segments_hl_idx <- surligner_vecteur_html_unicode(
          unname(textes_keep_idx),
          motifs,
          "<span class='highlight'>",
          "</span>",
          on_error = log_regex
        )
        if (any(grepl("<span class='highlight'>", segments_hl_idx, fixed = TRUE))) {
          segments_hl <- segments_hl_idx
        }
      }
    }

    if (length(segments_hl) == 0 && length(segments_keep) > 0) segments_hl <- unname(segments_keep)

    for (seg in echapper_segments_en_preservant_surlignage(segments_hl, "<span class='highlight'>", "</span>")) writeLines(paste0("<p>", seg, "</p>"), con)
  }

  writeLines("</body></html>", con)
  close(con)
  invisible(chemin_sortie)
}

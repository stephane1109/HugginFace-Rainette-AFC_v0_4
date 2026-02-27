# Rôle du fichier: visualisation_chd.R centralise la visualisation CHD pour le mode IRaMuTeQ-like.

tracer_chd_iramuteq <- function(res_stats_df,
                                classe,
                                mesure = "frequency",
                                type = "bar",
                                n_terms = 20,
                                show_negative = FALSE) {
  if (is.null(res_stats_df) || nrow(res_stats_df) == 0) {
    plot.new(); text(0.5, 0.5, "CHD IRaMuTeQ-like : statistiques indisponibles.", cex = 1)
    return(invisible(NULL))
  }

  df <- res_stats_df
  if (!("Classe" %in% names(df)) || !("Terme" %in% names(df))) {
    plot.new(); text(0.5, 0.5, "CHD IRaMuTeQ-like : colonnes manquantes.", cex = 1)
    return(invisible(NULL))
  }

  cl <- suppressWarnings(as.numeric(classe))
  if (!is.finite(cl) || is.na(cl)) {
    classes <- sort(unique(suppressWarnings(as.numeric(df$Classe))))
    classes <- classes[is.finite(classes)]
    if (length(classes) == 0) {
      plot.new(); text(0.5, 0.5, "Aucune classe disponible.", cex = 1)
      return(invisible(NULL))
    }
    cl <- classes[1]
  }

  sous_df <- df[suppressWarnings(as.numeric(df$Classe)) == cl, , drop = FALSE]
  if (nrow(sous_df) == 0) {
    plot.new(); text(0.5, 0.5, paste0("Classe ", cl, " vide."), cex = 1)
    return(invisible(NULL))
  }

  if (!mesure %in% names(sous_df)) {
    plot.new(); text(0.5, 0.5, paste0("Mesure ", mesure, " indisponible."), cex = 1)
    return(invisible(NULL))
  }

  vals <- suppressWarnings(as.numeric(sous_df[[mesure]]))
  termes <- as.character(sous_df$Terme)
  ok <- is.finite(vals) & !is.na(vals) & !is.na(termes) & nzchar(termes)
  vals <- vals[ok]
  termes <- termes[ok]

  if (length(vals) == 0) {
    plot.new(); text(0.5, 0.5, "Aucune valeur traçable.", cex = 1)
    return(invisible(NULL))
  }

  ord <- if (isTRUE(show_negative)) order(vals, decreasing = TRUE) else order(abs(vals), decreasing = TRUE)
  vals <- vals[ord]
  termes <- termes[ord]

  n_terms <- max(1L, as.integer(n_terms))
  n_terms <- min(n_terms, length(vals))
  vals <- vals[seq_len(n_terms)]
  termes <- termes[seq_len(n_terms)]

  if (identical(as.character(type), "cloud")) {
    freqs <- abs(vals)
    names(freqs) <- termes
    wordcloud::wordcloud(
      words = names(freqs),
      freq = freqs,
      min.freq = min(freqs),
      max.words = length(freqs),
      random.order = FALSE,
      colors = RColorBrewer::brewer.pal(8, "Set2")
    )
  } else {
    op <- par(no.readonly = TRUE)
    on.exit(par(op), add = TRUE)
    par(mar = c(8, 4, 3, 1))
    barplot(
      vals,
      names.arg = termes,
      las = 2,
      cex.names = 0.8,
      col = "#5B8FF9",
      border = NA,
      main = paste0("Classe ", cl, " - ", mesure),
      ylab = mesure
    )
  }

  invisible(NULL)
}

# Rôle du fichier: générer des nuages de mots dédiés au mode IRaMuTeQ-like.

generer_wordclouds_iramuteq <- function(res_stats_df,
                                        classes_uniques,
                                        wordcloud_dir,
                                        top_n = 20L,
                                        filtrer_pvalue = FALSE,
                                        max_p = 1) {
  if (is.null(res_stats_df) || !is.data.frame(res_stats_df) || nrow(res_stats_df) == 0) {
    return(invisible(NULL))
  }

  dir.create(wordcloud_dir, showWarnings = FALSE, recursive = TRUE)

  top_n <- suppressWarnings(as.integer(top_n))
  if (!is.finite(top_n) || is.na(top_n)) top_n <- 20L
  top_n <- max(5L, top_n)

  for (cl in classes_uniques) {
    df_stats_cl <- subset(res_stats_df, Classe == cl)
    if (nrow(df_stats_cl) == 0) next

    if (isTRUE(filtrer_pvalue) && is.finite(max_p) && !is.na(max_p)) {
      df_stats_cl <- df_stats_cl[df_stats_cl$p <= max_p, , drop = FALSE]
    }
    if (nrow(df_stats_cl) == 0) next

    df_stats_cl <- df_stats_cl[is.finite(df_stats_cl$chi2) & !is.na(df_stats_cl$chi2), , drop = FALSE]
    if (nrow(df_stats_cl) == 0) next

    df_stats_cl <- df_stats_cl[order(-df_stats_cl$chi2), , drop = FALSE]
    df_stats_cl <- head(df_stats_cl, top_n)

    wc_png <- file.path(wordcloud_dir, paste0("cluster_", cl, "_wordcloud.png"))
    try({
      png(wc_png, width = 800, height = 600)
      suppressWarnings(wordcloud::wordcloud(
        words = df_stats_cl$Terme,
        freq = pmax(df_stats_cl$chi2, 0),
        scale = c(8, 0.8),
        min.freq = 0,
        random.order = FALSE,
        max.words = nrow(df_stats_cl),
        colors = RColorBrewer::brewer.pal(8, "Dark2")
      ))
      dev.off()
    }, silent = TRUE)
  }

  invisible(NULL)
}

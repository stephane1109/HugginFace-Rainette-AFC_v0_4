# Rôle du fichier: stats_chd.R centralise la table des statistiques CHD pour le mode IRaMuTeQ-like.

extraire_stats_chd_classe <- function(res_stats_df, classe, n_max = 50) {
  if (is.null(res_stats_df) || nrow(res_stats_df) == 0) {
    return(data.frame(Message = "Statistiques indisponibles.", stringsAsFactors = FALSE))
  }

  cl <- suppressWarnings(as.numeric(classe))
  df <- res_stats_df
  if (is.finite(cl) && !is.na(cl) && "Classe" %in% names(df)) {
    df <- df[suppressWarnings(as.numeric(df$Classe)) == cl, , drop = FALSE]
  }

  colonnes_possibles <- intersect(
    c("Terme", "chi2", "lr", "frequency", "docprop", "p", "p_value_filter"),
    names(df)
  )
  df <- df[, colonnes_possibles, drop = FALSE]

  if ("chi2" %in% names(df)) df <- df[order(-suppressWarnings(as.numeric(df$chi2))), , drop = FALSE]
  utils::head(df, n_max)
}

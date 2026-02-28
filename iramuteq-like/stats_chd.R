# Rôle du fichier: stats_chd.R centralise la table des statistiques CHD pour le mode IRaMuTeQ-like.

formatter_6_decimales_chd <- function(x) {
  ifelse(is.na(x), NA_character_, formatC(as.numeric(x), format = "f", digits = 6))
}

extraire_stats_chd_classe <- function(res_stats_df, classe, n_max = 50, show_negative = FALSE) {
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

  if ("chi2" %in% names(df)) {
    chi2_vals <- suppressWarnings(as.numeric(df$chi2))
    if (!isTRUE(show_negative)) {
      df <- df[is.finite(chi2_vals) & chi2_vals > 0, , drop = FALSE]
      chi2_vals <- suppressWarnings(as.numeric(df$chi2))
    }
    df <- df[order(-chi2_vals, -suppressWarnings(as.numeric(df$frequency))), , drop = FALSE]
  }
  df <- utils::head(df, n_max)

  colonnes_num <- intersect(c("chi2", "lr", "docprop", "p", "p_value"), names(df))
  for (col in colonnes_num) {
    df[[col]] <- formatter_6_decimales_chd(df[[col]])
  }

  if ("frequency" %in% names(df)) {
    df$frequency <- ifelse(is.na(df$frequency), NA_character_, formatC(as.numeric(df$frequency), format = "f", digits = 6))
  }

  df
}

# stats.R
# Fonctions de calcul de statistiques descriptives du corpus

extraire_textes_iramuteq <- function(textes_bruts) {
  if (is.null(textes_bruts) || !length(textes_bruts)) return(character(0))

  idx_sep <- grepl("^\\*\\*\\*\\*", textes_bruts)
  if (!any(idx_sep)) {
    txt <- trimws(paste(textes_bruts, collapse = " "))
    return(if (nzchar(txt)) txt else character(0))
  }

  blocs <- list()
  debut <- which(idx_sep)
  fin <- c(debut[-1] - 1, length(textes_bruts))

  for (i in seq_along(debut)) {
    contenu <- textes_bruts[(debut[i] + 1):fin[i]]
    contenu <- trimws(paste(contenu, collapse = " "))
    if (nzchar(contenu)) blocs[[length(blocs) + 1]] <- contenu
  }

  unlist(blocs, use.names = FALSE)
}

calculer_stats_corpus <- function(chemin_fichier, corpus_segments = NULL) {
  if (is.null(chemin_fichier) || !file.exists(chemin_fichier)) return(NULL)

  lignes <- readLines(chemin_fichier, warn = FALSE, encoding = "UTF-8")
  textes <- extraire_textes_iramuteq(lignes)

  if (length(textes) == 0) {
    texte_global <- trimws(paste(lignes, collapse = " "))
    textes <- if (nzchar(texte_global)) texte_global else character(0)
  }

  tokens_obj <- quanteda::tokens(
    textes,
    what = "word",
    remove_punct = TRUE,
    remove_numbers = TRUE,
    remove_symbols = TRUE,
    remove_separators = TRUE
  )

  dfm_obj <- quanteda::dfm(tokens_obj)
  freqs <- as.numeric(quanteda::colSums(dfm_obj))
  freqs <- freqs[freqs > 0]

  nb_mots <- sum(freqs)
  nb_hapax <- sum(freqs == 1)
  nb_segments <- if (!is.null(corpus_segments)) quanteda::ndoc(corpus_segments) else NA_integer_

  loi_zipf <- "Indisponible"
  if (length(freqs) >= 2) {
    freq_rank <- sort(freqs, decreasing = TRUE)
    rang <- seq_along(freq_rank)
    fit <- stats::lm(log(freq_rank) ~ log(rang))
    pente <- unname(stats::coef(fit)[2])
    r2 <- summary(fit)$r.squared
    loi_zipf <- paste0("pente=", sprintf("%.3f", pente), " ; R2=", sprintf("%.3f", r2))
  }

  data.frame(
    Metrique = c(
      "Nom du corpus",
      "Nombre de textes",
      "Nombre de mots dans le corpus",
      "Nombre de segments de texte",
      "Nombre d'Hapax",
      "Loi de Zpif (Zipf)"
    ),
    Valeur = c(
      basename(chemin_fichier),
      as.character(length(textes)),
      as.character(nb_mots),
      as.character(nb_segments),
      as.character(nb_hapax),
      loi_zipf
    ),
    stringsAsFactors = FALSE
  )
}

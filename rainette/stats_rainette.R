# Rôle du fichier: stats_rainette.R porte une partie du pipeline d'analyse Rainette.
# stats_rainette.R
# Fonctions de calcul de statistiques descriptives du corpus


.scalar_chr <- function(x, default = "") {
  if (is.null(x) || length(x) == 0) return(default)
  vals <- trimws(as.character(x))
  vals <- vals[!is.na(vals) & nzchar(vals)]
  if (length(vals) == 0) return(default)
  vals[[1]]
}

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

calculer_stats_corpus <- function(chemin_fichier, corpus_segments = NULL, nom_corpus = NULL) {
  if (is.null(chemin_fichier) || !file.exists(chemin_fichier)) return(NULL)

  nom_corpus_affiche <- .scalar_chr(nom_corpus, default = "")
  if (!nzchar(nom_corpus_affiche)) {
    nom_corpus_affiche <- basename(chemin_fichier)
  }

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
  nb_formes <- length(freqs)
  nb_hapax <- sum(freqs == 1)
  nb_segments <- if (!is.null(corpus_segments)) quanteda::ndoc(corpus_segments) else NA_integer_

  loi_zipf <- "—"
  zipf_df <- NULL

  if (length(freqs) >= 2) {
    freq_rank <- sort(freqs, decreasing = TRUE)
    rang <- seq_along(freq_rank)
    fit <- stats::lm(log(freq_rank) ~ log(rang))
    pente <- unname(stats::coef(fit)[2])
    intercept <- unname(stats::coef(fit)[1])
    zipf_df <- data.frame(
      rang = rang,
      frequence = freq_rank,
      log_rang = log(rang),
      log_frequence = log(freq_rank),
      log_pred = intercept + pente * log(rang)
    )
  }

  metriques <- c(
    "Nom du corpus",
    "Nombre de textes",
    "Nombre de mots dans le corpus",
    "Nombre de formes",
    "Nombre de segments de texte",
    "Nombre d'Hapax",
    "Loi de Zpif"
  )

  valeurs <- c(
    .scalar_chr(nom_corpus_affiche, default = basename(chemin_fichier)),
    as.character(length(textes)),
    as.character(nb_mots),
    as.character(nb_formes),
    as.character(nb_segments),
    as.character(nb_hapax),
    .scalar_chr(loi_zipf, default = "—")
  )

  if (length(valeurs) != length(metriques)) {
    valeurs <- utils::head(c(valeurs, rep("", length(metriques))), length(metriques))
  }

  list(
    table = data.frame(
      Metrique = metriques,
      Valeur = valeurs,
      stringsAsFactors = FALSE
    ),
    zipf = zipf_df
  )
}

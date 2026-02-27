# Rôle du fichier: chd_iramuteq.R introduit une base "IRaMuTeQ-like" pour la CHD.
# Ce module prépare les entrées de CHD en respectant les options de nettoyage de l'application,
# expose des utilitaires pour le calcul de mincl (convention IRaMuTeQ texte),
# et fournit un calcul CHD réel en s'appuyant sur les scripts R historiques d'IRaMuTeQ.

# Valeur mincl automatique (mode texte IRaMuTeQ):
# mincl = round(n_uce / ind), avec ind = nbcl * 2 (double) sinon nbcl.
calculer_mincl_auto_iramuteq <- function(n_uce, nbcl, classif_mode = c("double", "simple")) {
  classif_mode <- match.arg(classif_mode)
  n_uce <- as.integer(n_uce)
  nbcl <- as.integer(nbcl)

  if (!is.finite(n_uce) || is.na(n_uce) || n_uce < 1) {
    stop("mincl auto IRaMuTeQ: n_uce invalide.")
  }
  if (!is.finite(nbcl) || is.na(nbcl) || nbcl < 1) {
    stop("mincl auto IRaMuTeQ: nbcl invalide.")
  }

  ind <- if (identical(classif_mode, "double")) nbcl * 2L else nbcl
  mincl <- round(n_uce / ind)
  as.integer(max(1L, mincl))
}

# Normalise une liste d'options de nettoyage selon les clés utilisées dans l'UI.
normaliser_options_nettoyage_iramuteq <- function(options_nettoyage = list()) {
  opts <- list(
    nettoyage_caracteres = isTRUE(options_nettoyage$nettoyage_caracteres),
    forcer_minuscules_avant = isTRUE(options_nettoyage$forcer_minuscules_avant),
    supprimer_chiffres = isTRUE(options_nettoyage$supprimer_chiffres),
    supprimer_apostrophes = isTRUE(options_nettoyage$supprimer_apostrophes),
    supprimer_ponctuation = isTRUE(options_nettoyage$supprimer_ponctuation),
    retirer_stopwords = isTRUE(options_nettoyage$retirer_stopwords)
  )
  opts
}

# Prépare textes/tokens/dfm en tenant compte des options de nettoyage existantes de l'application.
preparer_entrees_chd_iramuteq <- function(
  textes,
  langue = "fr",
  options_nettoyage = list(),
  appliquer_nettoyage_fun = NULL
) {
  if (!is.character(textes)) {
    textes <- as.character(textes)
  }
  textes[is.na(textes)] <- ""

  opts <- normaliser_options_nettoyage_iramuteq(options_nettoyage)

  if (is.null(appliquer_nettoyage_fun)) {
    if (exists("appliquer_nettoyage_et_minuscules", mode = "function", inherits = TRUE)) {
      appliquer_nettoyage_fun <- get("appliquer_nettoyage_et_minuscules", mode = "function", inherits = TRUE)
    } else {
      appliquer_nettoyage_fun <- function(textes,
                                          activer_nettoyage = FALSE,
                                          forcer_minuscules = FALSE,
                                          supprimer_chiffres = FALSE,
                                          supprimer_apostrophes = FALSE) {
        x <- as.character(textes)
        if (isTRUE(forcer_minuscules)) x <- tolower(x)
        x
      }
    }
  }

  textes_prep <- appliquer_nettoyage_fun(
    textes = textes,
    activer_nettoyage = opts$nettoyage_caracteres,
    forcer_minuscules = opts$forcer_minuscules_avant,
    supprimer_chiffres = opts$supprimer_chiffres,
    supprimer_apostrophes = opts$supprimer_apostrophes
  )

  if (!requireNamespace("quanteda", quietly = TRUE)) {
    stop("CHD IRaMuTeQ-like: package quanteda requis pour préparer les entrées.")
  }

  tok <- quanteda::tokens(
    textes_prep,
    remove_punct = opts$supprimer_ponctuation,
    remove_numbers = opts$supprimer_chiffres
  )

  if (opts$retirer_stopwords) {
    sw <- quanteda::stopwords(language = langue)
    tok <- quanteda::tokens_remove(tok, pattern = sw)
  }

  dfm_obj <- quanteda::dfm(tok)
  list(textes = textes_prep, tok = tok, dfm = dfm_obj, options = opts)
}

.trouver_fichier_insensible_casse <- function(dir_path, filename) {
  if (!dir.exists(dir_path)) return(NA_character_)
  files <- list.files(dir_path, full.names = TRUE)
  if (length(files) == 0) return(NA_character_)
  bn <- basename(files)
  idx <- which(tolower(bn) == tolower(filename))
  if (length(idx) == 0) return(NA_character_)
  files[idx[1]]
}

.trouver_rscripts_iramuteq <- function(base_dir = NULL) {
  scripts <- c("anacor.R", "CHD.R", "chdtxt.R")
    candidats <- unique(c(
    base_dir,
    "iramuteq-like",
    "iramuteq-like/Rscripts",
    "iramuteq_clone_v3/Rscripts"
  ))
  candidats <- candidats[!is.na(candidats) & nzchar(candidats)]

  for (cand in candidats) {
    paths <- vapply(scripts, function(sc) .trouver_fichier_insensible_casse(cand, sc), FUN.VALUE = character(1))
    if (all(!is.na(paths))) {
      return(unname(paths))
    }
  }

  stop(
    "CHD IRaMuTeQ-like: scripts R introuvables. Répertoires testés: ",
    paste(candidats, collapse = ", "),
    ". Fichiers attendus: ",
    paste(scripts, collapse = ", "),
    "."
  )
}

.charger_scripts_iramuteq_chd <- function(base_dir = NULL) {
  paths <- .trouver_rscripts_iramuteq(base_dir)
  for (p in paths) {
    source(p, encoding = "UTF-8", local = .GlobalEnv)
  }
  invisible(paths)
}

# Calcul CHD IRaMuTeQ-like (algorithme historique via scripts R IRaMuTeQ).
calculer_chd_iramuteq <- function(
  dfm_obj,
  k = 3,
  mode_patate = FALSE,
  svd_method = c("svdR", "irlba", "svdlibc"),
  libsvdc_path = NULL,
  binariser = TRUE,
  rscripts_dir = NULL
) {
  svd_method <- match.arg(svd_method)

  if (is.null(dfm_obj)) stop("CHD IRaMuTeQ-like: dfm_obj manquant.")
  if (!is.finite(k) || is.na(k) || as.integer(k) < 2) stop("CHD IRaMuTeQ-like: k doit être >= 2.")

  .charger_scripts_iramuteq_chd(rscripts_dir)

  mat <- as.matrix(dfm_obj)
  if (nrow(mat) < 2 || ncol(mat) < 2) {
    stop("CHD IRaMuTeQ-like: matrice trop pauvre (>=2 lignes et >=2 colonnes requises).")
  }

  if (isTRUE(binariser)) {
    mat <- ifelse(mat > 0, 1, 0)
  }

  rownames(mat) <- as.character(seq_len(nrow(mat)))

  nb_tours <- as.integer(k) - 1L
  if (nb_tours < 1) nb_tours <- 1L

  chd <- CHD(
    data.in = mat,
    x = nb_tours,
    mode.patate = isTRUE(mode_patate),
    svd.method = svd_method,
    libsvdc.path = libsvdc_path
  )

  if (is.null(chd$n1) || nrow(chd$n1) != nrow(mat)) {
    stop("CHD IRaMuTeQ-like: sortie CHD invalide.")
  }

  chd
}

# Reconstitue des classes finales depuis la sortie CHD et le principe find.terminales.
reconstruire_classes_terminales_iramuteq <- function(
  chd_obj,
  mincl = 0,
  mincl_mode = c("auto", "manuel"),
  classif_mode = c("simple", "double")
) {
  mincl_mode <- match.arg(mincl_mode)
  classif_mode <- match.arg(classif_mode)

  n1 <- chd_obj$n1
  list_mere <- chd_obj$list_mere
  list_fille <- chd_obj$list_fille

  if (is.null(n1) || is.null(list_mere) || is.null(list_fille)) {
    stop("CHD IRaMuTeQ-like: objet chd incomplet.")
  }

  nbcl <- length(unique(n1[, ncol(n1)]))
  nbcl <- max(2L, as.integer(nbcl))

  if (mincl_mode == "auto") {
    mincl_use <- calculer_mincl_auto_iramuteq(
      n_uce = nrow(n1),
      nbcl = nbcl,
      classif_mode = classif_mode
    )
  } else {
    mincl_use <- as.integer(mincl)
    if (!is.finite(mincl_use) || is.na(mincl_use) || mincl_use < 1) mincl_use <- 1L
  }

  terminales <- find.terminales(n1, list_mere, list_fille, mincl = mincl_use)
  if (is.character(terminales) && length(terminales) == 1 && terminales == "no clusters") {
    stop("CHD IRaMuTeQ-like: aucune classe terminale retenue.")
  }

  feuilles <- unique(as.integer(n1[, ncol(n1)]))
  classes_finales <- rep(0L, nrow(n1))

  for (i in seq_along(terminales)) {
    cl <- as.integer(terminales[[i]])
    if (cl %in% feuilles) {
      classes_finales[which(as.integer(n1[, ncol(n1)]) == cl)] <- i
    } else {
      filles <- getfille(list_fille, cl, NULL)
      filles <- intersect(as.integer(filles), feuilles)
      if (length(filles) > 0) {
        classes_finales[which(as.integer(n1[, ncol(n1)]) %in% filles)] <- i
      }
    }
  }

  list(
    classes = classes_finales,
    terminales = as.integer(terminales),
    mincl = mincl_use
  )
}

# Calcule une table de statistiques par classe dans l'esprit des sorties IRaMuTeQ.
construire_stats_classes_iramuteq <- function(dfm_obj, classes, max_p = 1) {
  if (is.null(dfm_obj)) stop("Stats IRaMuTeQ-like: dfm_obj manquant.")
  if (is.null(classes)) stop("Stats IRaMuTeQ-like: classes manquantes.")

  mat <- as.matrix(dfm_obj)
  if (nrow(mat) != length(classes)) {
    stop("Stats IRaMuTeQ-like: longueur de classes incohérente avec le DFM.")
  }

  classes <- as.integer(classes)
  ok_docs <- !is.na(classes) & classes > 0
  mat <- mat[ok_docs, , drop = FALSE]
  classes <- classes[ok_docs]

  if (nrow(mat) < 2 || ncol(mat) < 1) {
    return(data.frame())
  }

  mat_bin <- ifelse(mat > 0, 1, 0)
  classes_uniques <- sort(unique(classes))

  calc_lr <- function(n11, n12, n21, n22) {
    n <- n11 + n12 + n21 + n22
    r1 <- n11 + n12
    r2 <- n21 + n22
    c1 <- n11 + n21
    c2 <- n12 + n22
    expected <- c(r1 * c1 / n, r1 * c2 / n, r2 * c1 / n, r2 * c2 / n)
    observed <- c(n11, n12, n21, n22)
    idx <- observed > 0 & expected > 0
    if (!any(idx)) return(0)
    2 * sum(observed[idx] * log(observed[idx] / expected[idx]))
  }

  sorties <- vector("list", length(classes_uniques))

  for (i in seq_along(classes_uniques)) {
    cl <- classes_uniques[[i]]
    in_cl <- classes == cl
    n_cl <- sum(in_cl)
    n_non <- sum(!in_cl)

    n11 <- colSums(mat_bin[in_cl, , drop = FALSE])
    n21 <- colSums(mat_bin[!in_cl, , drop = FALSE])
    n12 <- n_cl - n11
    n22 <- n_non - n21

    chi2 <- mapply(function(a, b, c, d) {
      tb <- matrix(c(a, b, c, d), nrow = 2, byrow = TRUE)
      suppressWarnings(as.numeric(stats::chisq.test(tb, correct = TRUE)$statistic))
    }, n11, n12, n21, n22)

    pval <- stats::pchisq(chi2, df = 1, lower.tail = FALSE)
    lr <- mapply(calc_lr, n11, n12, n21, n22)

    df <- data.frame(
      Terme = colnames(mat_bin),
      chi2 = as.numeric(chi2),
      lr = as.numeric(lr),
      frequency = as.numeric(n11),
      docprop = as.numeric(if (n_cl > 0) n11 / n_cl else 0),
      p = as.numeric(pval),
      Classe = as.integer(cl),
      stringsAsFactors = FALSE
    )

    df <- df[is.finite(df$chi2) & !is.na(df$chi2), , drop = FALSE]
    if (is.finite(max_p) && !is.na(max_p) && max_p < 1) {
      df <- df[df$p <= max_p, , drop = FALSE]
    }
    df <- df[order(-df$chi2, -df$frequency), , drop = FALSE]
    sorties[[i]] <- df
  }

  out <- dplyr::bind_rows(sorties)
  if (!nrow(out)) return(out)

  out$Classe_brut <- as.character(out$Classe)
  out$p_value <- out$p
  out$p_value_filter <- ifelse(out$p <= max_p, paste0("≤ ", max_p), paste0("> ", max_p))
  out
}

# Dendrogramme CHD basé sur la structure hiérarchique IRaMuTeQ (list_mere/list_fille).
tracer_dendrogramme_chd_iramuteq <- function(chd_obj, terminales = NULL) {
  if (is.null(chd_obj) || is.null(chd_obj$list_fille) || is.null(chd_obj$n1)) {
    plot.new()
    text(0.5, 0.5, "Dendrogramme CHD indisponible.", cex = 1.1)
    return(invisible(NULL))
  }

  list_fille <- chd_obj$list_fille
  meres <- suppressWarnings(as.integer(names(list_fille)))
  meres <- meres[is.finite(meres)]
  enfants <- unique(as.integer(unlist(list_fille)))
  enfants <- enfants[is.finite(enfants)]

  racines <- setdiff(meres, enfants)
  racine <- if (length(racines)) racines[[1]] else if (length(meres)) meres[[1]] else NA_integer_
  if (!is.finite(racine)) {
    plot.new()
    text(0.5, 0.5, "Structure CHD invalide.", cex = 1.1)
    return(invisible(NULL))
  }

  get_filles <- function(node) {
    key <- as.character(node)
    x <- list_fille[[key]]
    if (is.null(x)) integer(0) else as.integer(x)
  }

  # Feuilles ordonnées par identifiant pour stabiliser l'affichage.
  feuilles <- setdiff(enfants, meres)
  feuilles <- sort(unique(feuilles))
  if (!length(feuilles)) {
    feuilles <- sort(unique(as.integer(chd_obj$n1[, ncol(chd_obj$n1)])))
  }

  x_map <- setNames(seq_along(feuilles), as.character(feuilles))
  positions <- list()

  compute_pos <- function(node, depth = 0L) {
    filles <- get_filles(node)
    if (!length(filles)) {
      x <- unname(x_map[[as.character(node)]])
      if (is.null(x) || !is.finite(x)) x <- length(x_map) + 1
      positions[[as.character(node)]] <<- c(x = x, y = -depth)
      return(c(x = x, y = -depth))
    }

    child_pos <- lapply(filles, function(f) compute_pos(f, depth + 1L))
    xs <- vapply(child_pos, function(v) v[["x"]], numeric(1))
    x <- mean(xs)
    positions[[as.character(node)]] <<- c(x = x, y = -depth)
    c(x = x, y = -depth)
  }

  compute_pos(racine, 0L)

  all_pos <- do.call(rbind, positions)
  plot(
    NA,
    xlim = c(min(all_pos[, "x"]) - 0.5, max(all_pos[, "x"]) + 0.5),
    ylim = c(min(all_pos[, "y"]) - 0.5, 0.5),
    axes = FALSE,
    xlab = "",
    ylab = "",
    main = "Dendrogramme CHD IRaMuTeQ-like"
  )

  for (mere_name in names(list_fille)) {
    mere <- suppressWarnings(as.integer(mere_name))
    if (!is.finite(mere)) next
    p_m <- positions[[as.character(mere)]]
    if (is.null(p_m)) next

    for (f in as.integer(list_fille[[mere_name]])) {
      p_f <- positions[[as.character(f)]]
      if (is.null(p_f)) next
      segments(p_m[["x"]], p_m[["y"]], p_f[["x"]], p_f[["y"]], col = "#2f4f4f", lwd = 1.5)
    }
  }

  noeuds <- as.integer(names(positions))
  col_points <- rep("#5B8FF9", length(noeuds))
  if (!is.null(terminales)) {
    terminales <- as.integer(terminales)
    col_points[noeuds %in% terminales] <- "#d62728"
  }

  points(all_pos[, "x"], all_pos[, "y"], pch = 19, col = col_points, cex = 1)
  text(all_pos[, "x"], all_pos[, "y"] - 0.12, labels = rownames(all_pos), cex = 0.7)

  invisible(NULL)
}

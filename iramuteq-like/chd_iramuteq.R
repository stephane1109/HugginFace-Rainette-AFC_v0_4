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

.normaliser_n1_chd <- function(n1) {
  if (is.null(n1)) return(NULL)
  if (is.data.frame(n1)) n1 <- as.matrix(n1)
  if (is.vector(n1)) {
    n1 <- matrix(as.integer(n1), ncol = 1)
  }
  if (!is.matrix(n1)) return(NULL)
  if (nrow(n1) < 1 || ncol(n1) < 1) return(NULL)
  n1
}

# Calcul CHD IRaMuTeQ-like (algorithme historique via scripts R IRaMuTeQ).
calculer_chd_iramuteq <- function(
  dfm_obj,
  k = 3,
  mode_patate = FALSE,
  svd_method = c("svdR", "irlba", "svdlibc"),
  libsvdc_path = NULL,
  binariser = TRUE,
  rscripts_dir = NULL,
  seed = 123
) {
  svd_method <- match.arg(svd_method)

  if (is.null(dfm_obj)) stop("CHD IRaMuTeQ-like: dfm_obj manquant.")
  if (!is.finite(k) || is.na(k) || as.integer(k) < 2) stop("CHD IRaMuTeQ-like: k doit être >= 2.")

  .charger_scripts_iramuteq_chd(rscripts_dir)

  if (!is.null(seed) && is.finite(seed)) set.seed(as.integer(seed))

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

  n1 <- .normaliser_n1_chd(chd$n1)
  if (is.null(n1) || nrow(n1) != nrow(mat)) {
    stop("CHD IRaMuTeQ-like: sortie CHD invalide.")
  }

  chd$n1 <- n1

  chd
}

# Reconstitue des classes finales depuis la sortie CHD et le principe find.terminales.
reconstruire_classes_terminales_iramuteq <- function(
  chd_obj,
  mincl = 0,
  mincl_mode = c("auto", "manuel"),
  classif_mode = c("simple", "double"),
  nb_classes_cible = NULL,
  respecter_nb_classes = TRUE
) {
  mincl_mode <- match.arg(mincl_mode)
  classif_mode <- match.arg(classif_mode)

  n1 <- .normaliser_n1_chd(chd_obj$n1)
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

  if (isTRUE(respecter_nb_classes) && !is.null(nb_classes_cible) && is.finite(nb_classes_cible)) {
    nb_classes_cible <- as.integer(nb_classes_cible)
    if (nb_classes_cible >= 2 && length(feuilles) == nb_classes_cible && length(unique(terminales)) != nb_classes_cible) {
      terminales <- sort(feuilles)
      mincl_use <- 1L
    }
  }

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

  if (nrow(mat) < 2 || ncol(mat) < 1) return(data.frame())

  # Alignement avec l'approche IRaMuTeQ historique (BuildProf):
  # - contingence documentaire (présence/absence terme)
  # - chi2 signé (sur/sous-représentation)
  # - p-value issue de chisq.test(..., correct = FALSE)
  mat_bin <- ifelse(mat > 0, 1L, 0L)
  total_docs <- nrow(mat_bin)
  docs_par_terme <- colSums(mat_bin)

  calc_chi_sign <- function(a, b, c, d) {
    tb <- matrix(c(a, b, c, d), nrow = 2, byrow = TRUE)
    chi <- suppressWarnings(stats::chisq.test(tb, correct = FALSE))
    stat <- suppressWarnings(as.numeric(chi$statistic))
    pval <- suppressWarnings(as.numeric(chi$p.value))
    exp11 <- suppressWarnings(as.numeric(chi$expected[1, 1]))

    if (!is.finite(stat) || is.na(stat)) stat <- 0
    if (!is.finite(pval) || is.na(pval)) pval <- 1
    if (!is.finite(exp11) || is.na(exp11)) exp11 <- a

    signe <- if (a >= exp11) 1 else -1
    c(chi2 = stat * signe, p = pval)
  }

  classes_uniques <- sort(unique(classes))
  sorties <- vector("list", length(classes_uniques))

  for (i in seq_along(classes_uniques)) {
    cl <- classes_uniques[[i]]
    in_cl <- classes == cl

    docs_cl <- sum(in_cl)
    if (docs_cl < 1) next

    docs_terme_cl <- colSums(mat_bin[in_cl, , drop = FALSE])
    docs_terme_hors <- pmax(0, docs_par_terme - docs_terme_cl)

    n11 <- as.numeric(docs_terme_cl)
    n12 <- as.numeric(docs_terme_hors)
    n21 <- as.numeric(pmax(0, docs_cl - docs_terme_cl))
    n22 <- as.numeric(pmax(0, (total_docs - docs_cl) - docs_terme_hors))

    chi_p <- t(mapply(calc_chi_sign, n11, n12, n21, n22))

    freq_cl <- colSums(mat[in_cl, , drop = FALSE])
    docprop_cl <- if (docs_cl > 0) docs_terme_cl / docs_cl else rep(0, ncol(mat))
    lr <- mapply(function(a, b, c, d) {
      n <- a + b + c + d
      r1 <- a + b
      r2 <- c + d
      c1 <- a + c
      c2 <- b + d
      expected <- c(r1 * c1 / n, r1 * c2 / n, r2 * c1 / n, r2 * c2 / n)
      observed <- c(a, b, c, d)
      idx <- observed > 0 & expected > 0
      if (!any(idx)) return(0)
      2 * sum(observed[idx] * log(observed[idx] / expected[idx]))
    }, n11, n12, n21, n22)

    df <- data.frame(
      Terme = colnames(mat),
      chi2 = as.numeric(chi_p[, "chi2"]),
      lr = as.numeric(lr),
      frequency = as.numeric(freq_cl),
      docprop = as.numeric(docprop_cl),
      eff_st = as.numeric(docs_terme_cl),
      eff_total = as.numeric(docs_par_terme),
      pourcentage = as.numeric(ifelse(docs_par_terme > 0, 100 * docs_terme_cl / docs_par_terme, 0)),
      p = as.numeric(chi_p[, "p"]),
      Classe = as.integer(cl),
      stringsAsFactors = FALSE
    )

    df <- df[is.finite(df$chi2) & !is.na(df$chi2), , drop = FALSE]
    if (is.finite(max_p) && !is.na(max_p) && max_p < 1) {
      df <- df[df$p <= max_p, , drop = FALSE]
    }
    df <- df[order(-df$chi2, -df$frequency, -docs_par_terme[df$Terme]), , drop = FALSE]
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
tracer_dendrogramme_chd_iramuteq <- function(chd_obj, terminales = NULL, classes = NULL) {
  if (is.null(chd_obj)) {
    plot.new()
    text(0.5, 0.5, "Dendrogramme CHD indisponible.", cex = 1.1)
    return(invisible(NULL))
  }

  n1 <- .normaliser_n1_chd(chd_obj$n1)
  if (is.null(chd_obj$list_fille) || is.null(n1)) {
    plot.new()
    text(0.5, 0.5, "Dendrogramme CHD indisponible.", cex = 1.1)
    return(invisible(NULL))
  }

  list_fille <- chd_obj$list_fille
  if (!is.list(list_fille) || length(list_fille) == 0) {
    plot.new()
    text(0.5, 0.5, "Dendrogramme CHD indisponible (list_fille vide).", cex = 1.1)
    return(invisible(NULL))
  }

  noms <- names(list_fille)
  if (is.null(noms) || any(!nzchar(noms))) noms <- as.character(seq_along(list_fille))
  map_filles <- stats::setNames(lapply(list_fille, function(x) as.integer(x)), noms)

  meres <- suppressWarnings(as.integer(names(map_filles)))
  meres <- meres[is.finite(meres)]
  enfants <- unique(as.integer(unlist(map_filles, use.names = FALSE)))
  enfants <- enfants[is.finite(enfants)]

  racines <- setdiff(meres, enfants)
  racine <- if (length(racines)) racines[[1]] else if (length(meres)) meres[[1]] else NA_integer_
  if (!is.finite(racine)) {
    feuilles_n1 <- suppressWarnings(as.integer(n1[, ncol(n1)]))
    feuilles_n1 <- feuilles_n1[is.finite(feuilles_n1)]
    if (length(feuilles_n1)) racine <- min(feuilles_n1, na.rm = TRUE)
  }

  if (!is.finite(racine)) {
    plot.new()
    text(0.5, 0.5, "Structure CHD invalide.", cex = 1.1)
    return(invisible(NULL))
  }

  get_filles <- function(node) {
    key <- as.character(node)
    x <- map_filles[[key]]
    x <- x[is.finite(x)]
    if (is.null(x)) integer(0) else as.integer(x)
  }

  terminales <- suppressWarnings(as.integer(terminales))
  terminales <- terminales[is.finite(terminales)]
  terminales <- unique(terminales)
  utiliser_terminales <- length(terminales) > 0

  # Ordre des feuilles via parcours en profondeur (style phylogramme IRaMuTeQ).
  # Quand les classes terminales sont connues, on fige l'arrêt du parcours sur ces noeuds
  # afin d'afficher le nombre réel de classes retenues (et non toutes les feuilles brutes).
  leaves <- integer(0)
  visited <- integer(0)
  walk_leaves <- function(node) {
    if (node %in% visited) return(invisible(NULL))
    visited <<- c(visited, node)

    if (isTRUE(utiliser_terminales) && node %in% terminales) {
      leaves <<- c(leaves, node)
      return(invisible(NULL))
    }

    filles <- get_filles(node)
    if (!length(filles)) {
      leaves <<- c(leaves, node)
      return(invisible(NULL))
    }
    for (f in filles) walk_leaves(f)
  }
  walk_leaves(racine)

  if (!length(leaves)) {
    leaves <- sort(unique(suppressWarnings(as.integer(n1[, ncol(n1)]))))
    leaves <- leaves[is.finite(leaves)]
  }
  if (!length(leaves)) {
    plot.new()
    text(0.5, 0.5, "Aucune feuille exploitable pour le dendrogramme.", cex = 1.1)
    return(invisible(NULL))
  }

  leaves <- unique(leaves)
  y_map <- stats::setNames(seq_along(leaves), as.character(leaves))
  pos <- list()
  seen <- integer(0)

  # Coordonnées de type phylogramme : x=profondeur, y=ordre des feuilles.
  layout_phylo <- function(node, depth = 0L) {
    if (node %in% seen) return(pos[[as.character(node)]])
    seen <<- c(seen, node)

    if (isTRUE(utiliser_terminales) && node %in% leaves) {
      y <- unname(y_map[[as.character(node)]])
      if (is.null(y) || !is.finite(y)) y <- max(unname(y_map)) + 1
      pos[[as.character(node)]] <<- c(x = depth, y = y)
      return(pos[[as.character(node)]])
    }

    filles <- get_filles(node)
    if (!length(filles)) {
      y <- unname(y_map[[as.character(node)]])
      if (is.null(y) || !is.finite(y)) y <- max(unname(y_map)) + 1
      pos[[as.character(node)]] <<- c(x = depth, y = y)
      return(pos[[as.character(node)]])
    }

    child_pos <- lapply(filles, function(f) layout_phylo(f, depth + 1L))
    ys <- vapply(child_pos, function(v) as.numeric(v[["y"]]), numeric(1))
    pos[[as.character(node)]] <<- c(x = depth, y = mean(ys))
    return(pos[[as.character(node)]])
  }

  layout_phylo(racine, 0L)

  if (!length(pos)) {
    plot.new()
    text(0.5, 0.5, "Dendrogramme CHD indisponible (positions vides).", cex = 1.1)
    return(invisible(NULL))
  }

  all_pos <- do.call(rbind, pos)
  x_max <- max(all_pos[, "x"], na.rm = TRUE)
  y_max <- max(all_pos[, "y"], na.rm = TRUE)

  plot(
    NA,
    xlim = c(-0.2, x_max + 1.4),
    ylim = c(y_max + 0.6, 0.4),
    axes = FALSE,
    xlab = "",
    ylab = "",
    main = "Dendrogramme CHD IRaMuTeQ-like (phylogramme)"
  )

  for (mere_name in names(map_filles)) {
    mere <- suppressWarnings(as.integer(mere_name))
    if (!is.finite(mere)) next
    p_m <- pos[[as.character(mere)]]
    if (is.null(p_m)) next

    filles <- as.integer(map_filles[[mere_name]])
    filles <- filles[is.finite(filles)]
    if (!length(filles)) next

    # Barre verticale au niveau du parent (style phylogramme).
    y_child <- numeric(0)
    for (f in filles) {
      p_f <- pos[[as.character(f)]]
      if (!is.null(p_f)) y_child <- c(y_child, p_f[["y"]])
    }
    if (length(y_child) >= 2) {
      segments(p_m[["x"]], min(y_child), p_m[["x"]], max(y_child), col = "#2f4f4f", lwd = 1.6)
    }

    # Branches horizontales parent -> enfant.
    for (f in filles) {
      p_f <- pos[[as.character(f)]]
      if (is.null(p_f)) next
      segments(p_m[["x"]], p_f[["y"]], p_f[["x"]], p_f[["y"]], col = "#2f4f4f", lwd = 1.6)
    }
  }

  node_ids <- suppressWarnings(as.integer(rownames(all_pos)))
  node_ids[!is.finite(node_ids)] <- NA_integer_
  tip_idx <- which(node_ids %in% leaves)

  tip_cols <- rep("#5B8FF9", nrow(all_pos))
  if (length(terminales)) tip_cols[which(node_ids %in% terminales)] <- "#d62728"

  tip_labels <- paste0("Classe ", rownames(all_pos)[tip_idx])
  if (!is.null(classes)) {
    classes <- suppressWarnings(as.integer(classes))
    classes <- classes[is.finite(classes) & classes > 0]
    if (length(classes)) {
      pct_par_classe <- prop.table(table(classes)) * 100
      if (length(terminales)) {
        for (i in seq_along(terminales)) {
          node <- terminales[[i]]
          idx_node <- which(rownames(all_pos)[tip_idx] == as.character(node))
          if (!length(idx_node)) next
          pct <- unname(pct_par_classe[as.character(i)])
          if (!is.finite(pct) || is.na(pct)) pct <- 0
          tip_labels[idx_node] <- paste0("Classe ", i, " (", format(round(pct, 1), nsmall = 1), " %)")
        }
      }
    }
  }

  if (length(tip_idx)) {
    points(all_pos[tip_idx, "x"], all_pos[tip_idx, "y"], pch = 19, col = tip_cols[tip_idx], cex = 0.95)
    text(
      x = all_pos[tip_idx, "x"] + 0.12,
      y = all_pos[tip_idx, "y"],
      labels = tip_labels,
      adj = c(0, 0.5),
      cex = 0.78
    )
  }

  invisible(NULL)
}

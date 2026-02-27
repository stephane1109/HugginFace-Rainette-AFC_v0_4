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
    "iramuteq-like/Rscripts"
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
  invisibly(paths)
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

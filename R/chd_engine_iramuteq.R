# Rôle du fichier: chd_engine_iramuteq.R encapsule le lancement du moteur CHD IRaMuTeQ-like.
# Ce module sert de point d'entrée dédié pour exécuter la CHD historique et reconstruire
# les classes terminales avec mincl (auto ou manuel).

lancer_moteur_chd_iramuteq <- function(
  dfm_obj,
  k,
  mincl_mode = c("auto", "manuel"),
  mincl = 0,
  classif_mode = c("simple", "double"),
  svd_method = c("svdR", "irlba", "svdlibc"),
  mode_patate = FALSE,
  libsvdc_path = NULL,
  binariser = TRUE,
  rscripts_dir = NULL
) {
  mincl_mode <- match.arg(mincl_mode)
  classif_mode <- match.arg(classif_mode)
  svd_method <- match.arg(svd_method)

  if (!exists("calculer_chd_iramuteq", mode = "function", inherits = TRUE) ||
      !exists("reconstruire_classes_terminales_iramuteq", mode = "function", inherits = TRUE)) {
    chemin_module <- "iramuteq-like/chd_iramuteq.R"
    if (file.exists(chemin_module)) {
      source(chemin_module, encoding = "UTF-8", local = .GlobalEnv)
    }
  }

  if (!exists("calculer_chd_iramuteq", mode = "function", inherits = TRUE)) {
    stop("Moteur CHD IRaMuTeQ-like indisponible: calculer_chd_iramuteq() introuvable.")
  }
  if (!exists("reconstruire_classes_terminales_iramuteq", mode = "function", inherits = TRUE)) {
    stop("Moteur CHD IRaMuTeQ-like indisponible: reconstruire_classes_terminales_iramuteq() introuvable.")
  }

  chd_obj <- calculer_chd_iramuteq(
    dfm_obj = dfm_obj,
    k = k,
    mode_patate = mode_patate,
    svd_method = svd_method,
    libsvdc_path = libsvdc_path,
    binariser = binariser,
    rscripts_dir = rscripts_dir
  )

  classes_obj <- reconstruire_classes_terminales_iramuteq(
    chd_obj = chd_obj,
    mincl = mincl,
    mincl_mode = mincl_mode,
    classif_mode = classif_mode
  )

  list(
    engine = "iramuteq-like",
    chd = chd_obj,
    classes = classes_obj$classes,
    terminales = classes_obj$terminales,
    mincl = classes_obj$mincl
  )
}

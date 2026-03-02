# Rôle du fichier: wrapper de compatibilité pour charger les options IRaMuTeQ-like
# depuis iramuteq-like/ui_options_iramuteq.R.

app_dir <- tryCatch(shiny::getShinyOption("appDir"), error = function(e) NULL)
if (is.null(app_dir) || !nzchar(app_dir)) app_dir <- getwd()

chemin_options_iramuteq <- file.path(app_dir, "iramuteq-like", "ui_options_iramuteq.R")
if (file.exists(chemin_options_iramuteq)) {
  source(chemin_options_iramuteq, encoding = "UTF-8", local = TRUE)
} else {
  stop("Fichier UI IRaMuTeQ-like introuvable: iramuteq-like/ui_options_iramuteq.R")
}

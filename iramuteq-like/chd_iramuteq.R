# Rôle du fichier: wrapper de compatibilité pour le moteur CHD IRaMuTeQ-like.
# La source de vérité est désormais `R/chd_iramuteq.R`.
# Ce fichier conserve l'ancien chemin `iramuteq-like/chd_iramuteq.R` pour éviter
# de casser des scripts externes qui le source() encore directement.

.chd_iramuteq_compat_paths <- function() {
  c(
    file.path("R", "chd_iramuteq.R"),
    file.path("..", "R", "chd_iramuteq.R"),
    file.path(getwd(), "R", "chd_iramuteq.R")
  )
}

.chd_iramuteq_source_canonique <- function() {
  candidats <- unique(.chd_iramuteq_compat_paths())
  candidats <- candidats[!is.na(candidats) & nzchar(candidats)]

  for (cand in candidats) {
    if (file.exists(cand)) {
      source(cand, encoding = "UTF-8", local = TRUE)
      return(invisible(cand))
    }
  }

  stop(
    "Module CHD canonique introuvable: R/chd_iramuteq.R. Chemins testés: ",
    paste(candidats, collapse = ", "),
    "."
  )
}

.chd_iramuteq_source_canonique()

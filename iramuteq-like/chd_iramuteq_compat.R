# Rôle du fichier: wrapper de compatibilité pour le moteur CHD IRaMuTeQ-like.
# La source de vérité est désormais `iramuteq-like/chd_iramuteq.R`.
# Ce fichier porte désormais un nom explicite (`chd_iramuteq_compat.R`)
# pour éviter la confusion avec le module canonique `iramuteq-like/chd_iramuteq.R`.

.chd_iramuteq_compat_paths <- function() {
  c(
    file.path("iramuteq-like", "chd_iramuteq.R"),
    file.path("R", "chd_iramuteq.R"),
    file.path("..", "iramuteq-like", "chd_iramuteq.R"),
    file.path("..", "R", "chd_iramuteq.R"),
    file.path(getwd(), "iramuteq-like", "chd_iramuteq.R"),
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
    "Module CHD canonique introuvable: iramuteq-like/chd_iramuteq.R. Chemins testés: ",
    paste(candidats, collapse = ", "),
    "."
  )
}

.chd_iramuteq_source_canonique()

# Rôle du fichier: bridge de compatibilité (nom explicite, distinct du module canonique).
# La source de vérité du moteur CHD IRaMuTeQ-like est `iramuteq-like/chd_iramuteq.R`.

.chd_iramuteq_bridge_paths <- function() {
  c(
    file.path("iramuteq-like", "chd_iramuteq.R"),
    file.path("..", "iramuteq-like", "chd_iramuteq.R"),
    file.path(getwd(), "iramuteq-like", "chd_iramuteq.R")
  )
}

.chd_iramuteq_source_impl <- function() {
  for (cand in unique(.chd_iramuteq_bridge_paths())) {
    if (file.exists(cand)) {
      source(cand, encoding = "UTF-8", local = TRUE)
      return(invisible(cand))
    }
  }
  stop(
    "Module CHD IRaMuTeQ-like introuvable. Chemins testés: ",
    paste(unique(.chd_iramuteq_bridge_paths()), collapse = ", "),
    "."
  )
}

.chd_iramuteq_source_impl()

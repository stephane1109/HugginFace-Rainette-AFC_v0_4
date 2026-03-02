# Rôle du fichier: utils_general.R porte une partie du pipeline d'analyse Rainette.
# Ce script centralise une responsabilité métier/technique utilisée par l'application.
# Il facilite la maintenance en explicitant le périmètre et les points d'intégration.
compter_tokens <- function(tok) {
  lst <- as.list(tok)
  sum(vapply(lst, length, integer(1)))
}

md5_fichier <- function(chemin) {
  if (is.null(chemin) || !file.exists(chemin)) return(NA_character_)
  as.character(tools::md5sum(chemin))[1]
}

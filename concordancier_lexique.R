# concordancier_lexique.R
# Wrapper dédié au mode dictionnaire Lexique (fr).

generer_concordancier_lexique_html <- function(
  chemin_sortie,
  segments_by_class,
  res_stats_df,
  max_p,
  textes_indexation,
  spacy_tokens_df = NULL,
  avancer = NULL,
  rv = NULL,
  ...
) {
  generer_concordancier_html(
    chemin_sortie = chemin_sortie,
    segments_by_class = segments_by_class,
    res_stats_df = res_stats_df,
    max_p = max_p,
    textes_indexation = textes_indexation,
    spacy_tokens_df = spacy_tokens_df,
    source_dictionnaire = "lexique_fr",
    avancer = avancer,
    rv = rv,
    ...
  )
}

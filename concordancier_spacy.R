# concordancier_spacy.R
# Wrapper dédié au mode dictionnaire spaCy.

generer_concordancier_spacy_html <- function(
  chemin_sortie,
  segments_by_class,
  res_stats_df,
  max_p,
  textes_indexation,
  spacy_tokens_df,
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
    source_dictionnaire = "spacy",
    avancer = avancer,
    rv = rv,
    ...
  )
}

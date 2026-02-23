executer_pipeline_spacy <- function(input, rv, ids_corpus, textes_chd, avancer, charger_module_spacy) {
  filtrage_morpho <- isTRUE(input$filtrage_morpho)
  utiliser_lemmes_spacy <- isTRUE(input$spacy_utiliser_lemmes)

  config_spacy <- configurer_langue_spacy(as.character(input$spacy_langue))
  langue_reference <- config_spacy$code

  pos_a_conserver <- NULL
  pos_spacy_pipeline <- pos_a_conserver

  if (isTRUE(filtrage_morpho)) {
    pos_a_conserver <- input$pos_spacy_a_conserver
    if (is.null(pos_a_conserver) || length(pos_a_conserver) == 0) pos_a_conserver <- c("NOUN", "ADJ")
    pos_spacy_pipeline <- pos_a_conserver
  }

  ajouter_log(
    rv,
    paste0(
      "spaCy (", config_spacy$modele, ", ", config_spacy$libelle, ") | filtrage morpho=", ifelse(filtrage_morpho, "1", "0"),
      ifelse(filtrage_morpho, paste0(" (spaCy POS: ", paste(pos_a_conserver, collapse = ", "), ")"), ""),
      " | lemmes=", ifelse(utiliser_lemmes_spacy, "1", "0"),
      " | source lemmes=spaCy",
      " | stopwords: spaCy"
    )
  )

  rv$lexique_fr_df <- NULL

  avancer(0.28, "spaCy : exécution Python")
  rv$statut <- "spaCy : prétraitement..."

  if (!exists("executer_spacy_filtrage", mode = "function", inherits = TRUE)) {
    ajouter_log(rv, "Diagnostic spaCy: fonction executer_spacy_filtrage absente avant chargement dynamique.")
    charge_spacy <- charger_module_spacy()
    if (!isTRUE(charge_spacy$ok)) {
      stop(paste0("Module spaCy indisponible pour le dictionnaire spaCy (", charge_spacy$raison, ") : ", charge_spacy$chemin))
    }
    ajouter_log(rv, paste0("Diagnostic spaCy: module chargé depuis ", charge_spacy$chemin, "."))
  }

  sp <- executer_spacy_filtrage(
    ids = ids_corpus,
    textes = unname(textes_chd),
    pos_a_conserver = pos_spacy_pipeline,
    utiliser_lemmes = utiliser_lemmes_spacy,
    lower_input = isTRUE(input$forcer_minuscules_avant),
    modele_spacy = config_spacy$modele,
    rv = rv
  )

  textes_spacy <- sp$textes
  names(textes_spacy) <- ids_corpus
  rv$spacy_tokens_df <- sp$tokens_df

  avancer(0.40, "spaCy : tokens + DFM")
  tok_base <- tokens(
    textes_spacy,
    remove_punct = isTRUE(input$supprimer_ponctuation),
    remove_numbers = isTRUE(input$supprimer_chiffres)
  )

  res_dfm <- construire_dfm_avec_fallback_stopwords(
    tok_base = tok_base,
    min_docfreq = input$min_docfreq,
    retirer_stopwords = isTRUE(input$retirer_stopwords),
    langue_spacy = langue_reference,
    rv = rv,
    libelle = "spaCy"
  )

  list(
    tok = res_dfm$tok,
    dfm_obj = res_dfm$dfm,
    langue_reference = langue_reference,
    source_dictionnaire = "spacy"
  )
}

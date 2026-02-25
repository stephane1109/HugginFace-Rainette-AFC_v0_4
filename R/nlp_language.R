# Rôle du fichier: nlp_language.R porte une partie du pipeline d'analyse Rainette.
# Ce script centralise une responsabilité métier/technique utilisée par l'application.
# Il facilite la maintenance en explicitant le périmètre et les points d'intégration.
# Module NLP - langue et dictionnaires spaCy
# Ce fichier regroupe la détection de langue du corpus, la vérification de cohérence
# entre langue estimée et langue sélectionnée, ainsi que la configuration des modèles
# spaCy et le chargement (caché) des stopwords par langue.

estimer_langue_corpus <- function(textes, rv = NULL, max_segments = 200) {
  if (is.null(textes) || length(textes) == 0) return(list(code = NA_character_, scores = c(fr = 0, en = 0, es = 0, it = 0, de = 0, pt = 0, ca = 0, zh = 0, ja = 0)))

  textes <- as.character(textes)
  textes <- textes[nzchar(trimws(textes))]
  if (length(textes) == 0) return(list(code = NA_character_, scores = c(fr = 0, en = 0, es = 0, it = 0, de = 0, pt = 0, ca = 0, zh = 0, ja = 0)))
  if (length(textes) > max_segments) textes <- textes[seq_len(max_segments)]

  tok <- quanteda::tokens(textes, remove_punct = TRUE, remove_numbers = TRUE)
  tok <- quanteda::tokens_tolower(tok)
  all_tokens <- unlist(as.list(tok), use.names = FALSE)
  all_tokens <- trimws(all_tokens)
  all_tokens <- all_tokens[nzchar(all_tokens)]

  if (length(all_tokens) == 0) return(list(code = NA_character_, scores = c(fr = 0, en = 0, es = 0, it = 0, de = 0, pt = 0, ca = 0, zh = 0, ja = 0)))

  scores <- c(
    fr = mean(all_tokens %in% obtenir_stopwords_spacy("fr", rv = rv)),
    en = mean(all_tokens %in% obtenir_stopwords_spacy("en", rv = rv)),
    es = mean(all_tokens %in% obtenir_stopwords_spacy("es", rv = rv)),
    it = mean(all_tokens %in% obtenir_stopwords_spacy("it", rv = rv)),
    de = mean(all_tokens %in% obtenir_stopwords_spacy("de", rv = rv)),
    pt = mean(all_tokens %in% obtenir_stopwords_spacy("pt", rv = rv)),
    ca = mean(all_tokens %in% obtenir_stopwords_spacy("ca", rv = rv)),
    zh = mean(all_tokens %in% obtenir_stopwords_spacy("zh", rv = rv)),
    ja = mean(all_tokens %in% obtenir_stopwords_spacy("ja", rv = rv))
  )

  langue <- names(scores)[which.max(scores)]
  list(code = langue, scores = scores)
}

verifier_coherence_dictionnaire_langue <- function(textes, langue_selectionnee, rv = NULL) {
  est <- estimer_langue_corpus(textes, rv = rv)
  if (is.na(est$code)) return(invisible(est))

  sel <- configurer_langue_spacy(langue_selectionnee)$code
  sc_sel <- as.numeric(est$scores[[sel]])
  sc_best <- as.numeric(max(est$scores))
  marge <- sc_best - sc_sel

  if (!identical(sel, est$code) && sc_best >= 0.02 && marge >= 0.01) {
    cfg_sel <- configurer_langue_spacy(sel)
    cfg_best <- configurer_langue_spacy(est$code)
    stop(
      paste0(
        "Langue incohérente : le corpus ressemble à du ", cfg_best$libelle,
        " mais le dictionnaire spaCy sélectionné est ", cfg_sel$libelle,
        ". Choisis le dictionnaire ", cfg_best$libelle, " avant de lancer l'analyse."
      )
    )
  }

  invisible(est)
}

configurer_langue_spacy <- function(langue) {
  if (is.null(langue) || !nzchar(as.character(langue))) langue <- "fr"
  langue <- trimws(tolower(as.character(langue)))
  if (!langue %in% c("fr", "en", "es", "it", "de", "pt", "ca", "zh", "ja")) langue <- "fr"

  cfg <- switch(
    langue,
    fr = list(code = "fr", libelle = "Français", modele = "fr_core_news_md", stopwords_module = "fr"),
    en = list(code = "en", libelle = "Anglais", modele = "en_core_web_md", stopwords_module = "en"),
    es = list(code = "es", libelle = "Espagnol", modele = "es_core_news_md", stopwords_module = "es"),
    it = list(code = "it", libelle = "Italien", modele = "it_core_news_md", stopwords_module = "it"),
    de = list(code = "de", libelle = "Allemand", modele = "de_core_news_md", stopwords_module = "de"),
    pt = list(code = "pt", libelle = "Portugais", modele = "pt_core_news_md", stopwords_module = "pt"),
    ca = list(code = "ca", libelle = "Catalan", modele = "ca_core_news_md", stopwords_module = "ca"),
    zh = list(code = "zh", libelle = "Chinois", modele = "zh_core_web_md", stopwords_module = "zh"),
    ja = list(code = "ja", libelle = "Japonais", modele = "ja_core_news_md", stopwords_module = "ja"),
    list(code = "fr", libelle = "Français", modele = "fr_core_news_md", stopwords_module = "fr")
  )

  # Permet de surcharger le modèle spaCy via variable d'environnement, par langue.
  # Exemple: RAINETTE_SPACY_MODEL_FR=fr_dep_news_trf
  cle_env <- paste0("RAINETTE_SPACY_MODEL_", toupper(cfg$code))
  modele_override <- trimws(Sys.getenv(cle_env, unset = ""))
  if (nzchar(modele_override)) {
    cfg$modele <- modele_override
  }

  cfg
}

obtenir_stopwords_spacy <- local({
  cache <- new.env(parent = emptyenv())

  function(langue_spacy = "fr", rv = NULL) {
    cfg <- configurer_langue_spacy(langue_spacy)
    code <- cfg$code

    if (exists(code, envir = cache, inherits = FALSE)) {
      return(get(code, envir = cache, inherits = FALSE))
    }

    python_cmd <- Sys.which("python3")
    if (!nzchar(python_cmd)) python_cmd <- Sys.which("python")

    if (nzchar(python_cmd)) {
      py_code <- paste(
        paste0("from spacy.lang.", cfg$stopwords_module, ".stop_words import STOP_WORDS"),
        "for w in sorted(STOP_WORDS):",
        "    print(w)",
        sep = "\n"
      )

      sortie <- tryCatch(
        system2(python_cmd, args = c("-c", shQuote(py_code)), stdout = TRUE, stderr = TRUE),
        error = function(e) character(0)
      )

      if (length(sortie) > 0) {
        stopwords_spacy <- trimws(sortie)
        stopwords_spacy <- stopwords_spacy[nzchar(stopwords_spacy)]
        stopwords_spacy <- stopwords_spacy[!grepl("^Traceback", stopwords_spacy)]

        if (length(stopwords_spacy) > 0) {
          sw <- unique(stopwords_spacy)
          assign(code, sw, envir = cache)
          if (!is.null(rv)) {
            ajouter_log(rv, paste0("Stopwords spaCy chargés (", cfg$libelle, ") : ", length(sw), " termes."))
          }
          return(sw)
        }
      }
    }

    assign(code, character(0), envir = cache)
    if (!is.null(rv)) {
      ajouter_log(rv, paste0("Impossible de charger les stopwords spaCy pour ", cfg$libelle, "."))
    }
    get(code, envir = cache, inherits = FALSE)
  }
})

obtenir_stopwords_quanteda_fr <- local({
  cache <- NULL

  function(rv = NULL) {
    if (!is.null(cache)) return(cache)

    sw <- tryCatch(
      quanteda::stopwords(language = "fr", source = "stopwords-iso"),
      error = function(e) character(0)
    )

    sw <- unique(trimws(tolower(as.character(sw))))
    sw <- sw[nzchar(sw)]

    if (!is.null(rv)) {
      if (length(sw) > 0) {
        ajouter_log(rv, paste0("Stopwords quanteda (fr/stopwords-iso) chargés : ", length(sw), " termes."))
      } else {
        ajouter_log(rv, "Impossible de charger les stopwords quanteda (fr/stopwords-iso).")
      }
    }

    cache <<- sw
    cache
  }
})

obtenir_stopwords_analyse <- function(langue_spacy = "fr", source_dictionnaire = "spacy", lexique_source_stopwords = "quanteda", rv = NULL) {
  source_dictionnaire <- tolower(trimws(as.character(source_dictionnaire)))
  if (!source_dictionnaire %in% c("spacy", "lexique_fr")) source_dictionnaire <- "spacy"

  # Politique explicite:
  # - dictionnaire spaCy   -> stopwords spaCy uniquement
  # - dictionnaire lexique -> stopwords quanteda (fr) uniquement
  if (identical(source_dictionnaire, "lexique_fr")) {
    sw_quanteda <- obtenir_stopwords_quanteda_fr(rv = rv)
    if (length(sw_quanteda) > 0) {
      if (!is.null(rv)) {
        ajouter_log(rv, paste0("lexique_fr: stopwords quanteda (fr) utilisés : ", length(sw_quanteda), " termes."))
      }
      return(sw_quanteda)
    }

    if (!is.null(rv)) {
      ajouter_log(rv, "Aucun stopword quanteda exploitable pour lexique_fr.")
    }
    return(character(0))
  }

  sw_spacy <- obtenir_stopwords_spacy(langue_spacy = langue_spacy, rv = rv)
  sw_spacy <- unique(trimws(as.character(sw_spacy)))
  sw_spacy <- sw_spacy[nzchar(sw_spacy)]

  if (length(sw_spacy) > 0) {
    return(sw_spacy)
  }

  if (!is.null(rv)) {
    ajouter_log(rv, paste0("Aucun stopword spaCy exploitable pour la langue '", langue_spacy, "'."))
  }

  character(0)
}

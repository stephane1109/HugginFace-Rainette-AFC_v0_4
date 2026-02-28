# Rôle du fichier: app.R porte une partie du pipeline d'analyse Rainette.
# Ce script centralise une responsabilité métier/technique utilisée par l'application.

###############################################################################
#                    Script CHD - version beta 0.4 - 18-02-2026               #
#      A partir d'un corpus texte formaté aux exigences IRAMUTEQ              #
#                            Stéphane Meurisse                                #
#                           wwww.codeandcortex.fr                             #          
#                                                                             #
#      1.Réalise la CHD sur le corpus, sans rainette_explor                   #
#      2.Extrait chi2, lr, freq, docprop dans un CSV                          #
#      3.AFC                                                                  #
#      4.Recherche de NER avec Spacy (md)                                     #
#      5.Génère nuages de mots et graphes de cooccurrences par classe         #
#      6.Exporte les segments de texte par classe au format text              #
#      7.Creation d'un concordancier au format html                           #
#      8.Recherche de coocurrences                                            #
###############################################################################

library(shiny)
library(rainette)
library(quanteda)
library(wordcloud)
library(RColorBrewer)
library(igraph)
library(dplyr)
library(htmltools)

options(shiny.maxRequestSize = 300 * 1024^2)
options(shinygadgets.viewer = shiny::browserViewer())
options(bspm.sudo = TRUE)

if (file.exists("help.md")) {
  ui_aide_huggingface <- function() {
    tagList(
      tags$h2("Aide"),
      includeMarkdown("help.md")
    )
  }
} else {
  ui_aide_huggingface <- function() {
    tagList(
      tags$h2("Aide"),
      tags$p("Le fichier help.md est introuvable. Ajoute help.md à la racine du projet.")
    )
  }
}

source("nettoyage.R", encoding = "UTF-8", local = TRUE)
source("concordancier_utils.R", encoding = "UTF-8", local = TRUE)
source("concordancier_spacy.R", encoding = "UTF-8", local = TRUE)
source("concordancier_lexique.R", encoding = "UTF-8", local = TRUE)
source("concordancier_ner.R", encoding = "UTF-8", local = TRUE)
source("afc.R", encoding = "UTF-8", local = TRUE)
source("stats.R", encoding = "UTF-8", local = TRUE)
source("ui.R", encoding = "UTF-8", local = TRUE)

source("R/utils_general.R", encoding = "UTF-8", local = TRUE)
source("R/utils_logging.R", encoding = "UTF-8", local = TRUE)
source("R/utils_text.R", encoding = "UTF-8", local = TRUE)

source("R/afc_helpers.R", encoding = "UTF-8", local = TRUE)

source("R/chd_afc_pipeline.R", encoding = "UTF-8", local = TRUE)
source("R/chd_iramuteq.R", encoding = "UTF-8", local = TRUE)
source("iramuteq-like/visualisation_chd.R", encoding = "UTF-8", local = TRUE)
source("iramuteq-like/stats_chd.R", encoding = "UTF-8", local = TRUE)
source("R/chd_engine_iramuteq.R", encoding = "UTF-8", local = TRUE)
source("R/nlp_language.R", encoding = "UTF-8", local = TRUE)
source("R/nlp_spacy.R", encoding = "UTF-8", local = TRUE)
source("R/nlp_lexique.R", encoding = "UTF-8", local = TRUE)
source("R/pipeline_spacy_analysis.R", encoding = "UTF-8", local = TRUE)
source("R/pipeline_lexique_analysis.R", encoding = "UTF-8", local = TRUE)
source("R/server_outputs_status.R", encoding = "UTF-8", local = TRUE)
source("R/server_events_lancer.R", encoding = "UTF-8", local = TRUE)

server <- function(input, output, session) {

  rv <- reactiveValues(
    logs = "",
    statut = "En attente.",
    progression = 0,

    base_dir = NULL,
    export_dir = NULL,
    segments_file = NULL,
    stats_file = NULL,
    html_file = NULL,
    ner_file = NULL,
    zip_file = NULL,

    res = NULL,
    res_chd = NULL,
    dfm_chd = NULL,
    dfm = NULL,
    filtered_corpus = NULL,
    res_stats_df = NULL,
    clusters = NULL,
    max_n_groups = NULL,
    max_n_groups_chd = NULL,

    res_type = "simple",

    exports_prefix = paste0("exports_", session$token),

    spacy_tokens_df = NULL,
    lexique_fr_df = NULL,
    textes_indexation = NULL,

    ner_df = NULL,
    ner_nb_segments = NA_integer_,

    afc_obj = NULL,
    afc_erreur = NULL,

    afc_vars_obj = NULL,
    afc_vars_erreur = NULL,

    afc_dir = NULL,
    afc_table_mots = NULL,
    afc_table_vars = NULL,
    afc_plot_classes = NULL,
    afc_plot_termes = NULL,
    afc_plot_vars = NULL,

    explor_assets = NULL,
    stats_corpus_df = NULL,
    stats_zipf_df = NULL
  )

  register_outputs_status(input, output, session, rv)

  output$ui_afc_statut <- renderUI({
    if (!is.null(rv$afc_erreur) && nzchar(rv$afc_erreur)) {
      return(tags$p("AFC : erreur (voir ci-dessous)."))
    }
    if (is.null(rv$afc_obj) || is.null(rv$afc_obj$ca)) {
      return(tags$p("AFC non calculée. Lance une analyse pour calculer l'AFC classes × termes."))
    }
    ncl <- nrow(rv$afc_obj$table)
    nt <- ncol(rv$afc_obj$table)
    tags$p(paste0("AFC calculée sur ", ncl, " classes et ", nt, " termes (table Classes × Termes)."))
  })

  output$ui_afc_erreurs <- renderUI({
    messages <- Filter(
      nzchar,
      list(
        rv$afc_erreur,
        rv$afc_vars_erreur
      )
    )

    if (length(messages) == 0) {
      return(NULL)
    }

    tags$div(
      style = "display: flex; flex-direction: column; gap: 8px; margin-bottom: 12px;",
      lapply(messages, function(msg) {
        tags$div(
          style = "border: 1px solid #f5c2c7; background: #f8d7da; color: #842029; border-radius: 4px; padding: 10px; white-space: pre-wrap;",
          msg
        )
      })
    )
  })

  output$ui_spacy_langue_detection <- renderUI({
    if (identical(input$source_dictionnaire, "lexique_fr")) {
      return(NULL)
    }

    if (is.null(rv$filtered_corpus)) {
      return(tags$p("Détection langue : charge et lance une analyse pour afficher une estimation."))
    }

    est <- estimer_langue_corpus(as.character(rv$filtered_corpus))
    if (is.na(est$code)) {
      return(tags$p("Détection langue : estimation indisponible."))
    }

    cfg_est <- configurer_langue_spacy(est$code)
    cfg_sel <- configurer_langue_spacy(if (identical(input$source_dictionnaire, "lexique_fr")) "fr" else input$spacy_langue)
    src_dic <- if (identical(input$source_dictionnaire, "lexique_fr")) "Lexique (fr)" else "spaCy"

    msg <- paste0(
      "Langue estimée du corpus : ", cfg_est$libelle,
      " (scores stopwords FR=", sprintf("%.3f", est$scores[["fr"]]),
      ", EN=", sprintf("%.3f", est$scores[["en"]]),
      ", ES=", sprintf("%.3f", est$scores[["es"]]),
      ", DE=", sprintf("%.3f", est$scores[["de"]]), ")."
    )

    if (!identical(cfg_est$code, cfg_sel$code)) {
      return(tags$div(
        style = "border:1px solid #f5c2c7;background:#f8d7da;color:#842029;padding:10px;border-radius:4px;",
        tags$p(style = "margin:0;", paste0(msg, " Dictionnaire actif : ", src_dic, " (langue ", toupper(cfg_sel$code), ")."))
      ))
    }

    tags$div(
      style = "border:1px solid #badbcc;background:#d1e7dd;color:#0f5132;padding:10px;border-radius:4px;",
      tags$p(style = "margin:0;", paste0(msg, " Dictionnaire actif : ", src_dic, " (langue ", toupper(cfg_sel$code), ")."))
    )
  })

  output$ui_ner_statut <- renderUI({
    if (!isTRUE(input$activer_ner)) {
      return(tags$p("NER désactivé. Coche 'Activer NER (spaCy)' puis relance l'analyse."))
    }

    if (is.null(rv$ner_df)) {
      return(tags$p("NER activé, mais aucun résultat disponible. Relance une analyse complète."))
    }

    nb_ent <- nrow(rv$ner_df)
    nb_seg <- ifelse(is.na(rv$ner_nb_segments), 0, rv$ner_nb_segments)

    source_dico <- "Aucun dictionnaire JSON personnalisé."
    if (!is.null(rv$ner_file) && nzchar(as.character(rv$ner_file)) && file.exists(rv$ner_file)) {
      source_dico <- paste0("Dictionnaire JSON personnalisé importé : ", basename(rv$ner_file), ".")
    } else {
      dico_env <- trimws(Sys.getenv("RAINETTE_NER_JSON", unset = ""))
      if (nzchar(dico_env)) {
        source_dico <- paste0("Dictionnaire JSON personnalisé via variable d'environnement : ", dico_env, ".")
      }
    }

    tags$div(
      tags$p(paste0("NER calculé sur ", nb_seg, " segments. Entités détectées : ", nb_ent, ".")),
      tags$p(source_dico)
    )
  })

  output$ui_ner_lexique_incompatibilite <- renderUI({
    if (!isTRUE(input$activer_ner) || !identical(input$source_dictionnaire, "lexique_fr")) {
      return(NULL)
    }

    tags$div(
      style = "border:1px solid #f5c2c7;background:#f8d7da;color:#842029;padding:10px;border-radius:4px;margin:8px 0;",
      tags$strong("Incompatibilité détectée : "),
      tags$span("le NER n'est pas disponible avec la source de lemmatisation \"Lexique (fr)\". "),
      tags$span("Désactive \"Activer NER (spaCy)\" ou bascule la source vers \"spaCy\" avant de lancer l'analyse.")
    )
  })

  output$ui_corpus_preview <- renderUI({
    fichier <- input$fichier_corpus
    if (is.null(fichier) || is.null(fichier$datapath) || !file.exists(fichier$datapath)) {
      return(tags$p("Aucun corpus importé pour le moment."))
    }

    lignes <- tryCatch(
      readLines(fichier$datapath, encoding = "UTF-8", warn = FALSE),
      error = function(e) NULL
    )

    if (is.null(lignes) || length(lignes) == 0) {
      return(tags$p("Le corpus importé est vide ou illisible."))
    }

    max_lignes <- 250
    extrait <- lignes[seq_len(min(length(lignes), max_lignes))]
    texte <- paste(extrait, collapse = "\n")

    if (length(lignes) > max_lignes) {
      texte <- paste0(
        texte,
        "\n\n… Aperçu limité aux ", max_lignes,
        " premières lignes (", length(lignes), " lignes au total)."
      )
    }

    tags$div(
      tags$p(
        style = "margin-bottom: 8px;",
        paste0("Fichier : ", fichier$name)
      ),
      tags$pre(
        style = "white-space: pre-wrap; max-height: 70vh; overflow-y: auto; border: 1px solid #ddd; padding: 10px; background: #fafafa;",
        texte
      )
    )
  })

  output$table_stats_corpus <- renderTable({
    req(rv$stats_corpus_df)
    rv$stats_corpus_df
  }, striped = TRUE, spacing = "s", rownames = FALSE)


  output$plot_stats_zipf <- renderPlot({
    req(rv$stats_zipf_df)
    df <- rv$stats_zipf_df
    if (is.null(df) || nrow(df) < 2) {
      plot.new()
      text(0.5, 0.5, "Données insuffisantes pour tracer la loi de Zpif.", cex = 1.1)
      return(invisible(NULL))
    }

    x_lim <- range(df$log_rang, na.rm = TRUE)
    y_lim <- range(c(df$log_frequence, df$log_pred), na.rm = TRUE)

    plot(
      x = df$log_rang,
      y = df$log_frequence,
      pch = 16,
      cex = 0.8,
      col = grDevices::adjustcolor("#2C7FB8", alpha.f = 0.7),
      xlab = "log(rang)",
      ylab = "log(fréquence)",
      main = "Loi de Zpif",
      xlim = x_lim,
      ylim = y_lim,
      asp = 1
    )
    grid(col = "#E6E6E6", lty = "dotted")

    ord <- order(df$log_rang)
    lines(df$log_rang[ord], df$log_pred[ord], col = "#D7301F", lwd = 2.5)

    legend(
      "topright",
      legend = c("Données", "Régression log-log"),
      col = c("#2C7FB8", "#D7301F"),
      pch = c(16, NA),
      lty = c(NA, 1),
      lwd = c(NA, 2),
      bty = "n"
    )
  })

  output$table_ner_resume <- renderTable({
    req(rv$ner_df)
    if (nrow(rv$ner_df) == 0) return(data.frame(Message = "Aucune entité détectée.", stringsAsFactors = FALSE))

    as.data.frame(sort(table(rv$ner_df$ent_label), decreasing = TRUE), stringsAsFactors = FALSE) |>
      dplyr::rename(Type = Var1, Effectif = Freq)
  }, rownames = FALSE)

  output$table_ner_details <- renderTable({
    req(rv$ner_df)
    if (nrow(rv$ner_df) == 0) return(data.frame(Message = "Aucune entité détectée.", stringsAsFactors = FALSE))

    df <- rv$ner_df[, intersect(c("Classe", "doc_id", "ent_text", "ent_label", "segment_texte"), names(rv$ner_df)), drop = FALSE]
    head(df, 200)
  }, rownames = FALSE)

  output$plot_ner_wordcloud <- renderPlot({
    req(rv$ner_df)
    if (nrow(rv$ner_df) == 0) {
      plot.new()
      text(0.5, 0.5, "Aucune entité détectée.", cex = 1.1)
      return(invisible(NULL))
    }

    freq <- sort(table(rv$ner_df$ent_text), decreasing = TRUE)
    suppressWarnings(wordcloud(
      words = names(freq),
      freq = as.numeric(freq),
      min.freq = 1,
      max.words = min(150, length(freq)),
      random.order = FALSE,
      colors = brewer.pal(8, "Dark2")
    ))
  })

  output$ui_ner_wordcloud_par_classe <- renderUI({
    req(rv$ner_df)
    if (nrow(rv$ner_df) == 0 || !"Classe" %in% names(rv$ner_df)) return(tags$p("Aucune entité à afficher par classe."))

    classes <- sort(unique(rv$ner_df$Classe))
    if (length(classes) == 0) return(tags$p("Aucune classe disponible pour l'affichage."))

    tagList(lapply(classes, function(cl) {
      nm <- paste0("plot_ner_wordcloud_cl_", cl)
      local({
        cl_local <- cl
        output[[nm]] <- renderPlot({
          df_cl <- rv$ner_df[rv$ner_df$Classe == cl_local, , drop = FALSE]
          if (nrow(df_cl) == 0) {
            plot.new()
            text(0.5, 0.5, paste0("Classe ", cl_local, " : aucune entité."), cex = 1.1)
            return(invisible(NULL))
          }

          freq <- sort(table(df_cl$ent_text), decreasing = TRUE)
          suppressWarnings(wordcloud(
            words = names(freq),
            freq = as.numeric(freq),
            min.freq = 1,
            max.words = min(120, length(freq)),
            random.order = FALSE,
            colors = brewer.pal(8, "Set2")
          ))
        })
      })

      tagList(
        tags$h4(paste0("Classe ", cl)),
        plotOutput(nm, height = "360px")
      )
    }))
  })

  output$ui_chd_statut <- renderUI({
    if (is.null(rv$res)) {
      return(tags$p("CHD non disponible. Lance une analyse."))
    }

    nb_classes <- NA_integer_
    if (!is.null(rv$clusters)) nb_classes <- length(rv$clusters)

    if (identical(rv$res_type, "iramuteq")) {
      return(tags$p(paste0("CHD disponible (moteur IRaMuTeQ-like) - classes détectées : ", nb_classes, ".")))
    }

    if (identical(rv$res_type, "double")) {
      return(tags$p("CHD disponible (classification double rainette2)."))
    }

    tags$p(paste0("CHD disponible (classification simple rainette) - classes détectées : ", nb_classes, "."))
  })

  register_events_lancer(input, output, session, rv)




  observeEvent(input$explor, {
    req(rv$export_dir)

    if (is.null(rv$exports_prefix) || !nzchar(rv$exports_prefix)) {
      showNotification("Préfixe d'export invalide.", type = "error", duration = 8)
      return(invisible(NULL))
    }

    if (!(rv$exports_prefix %in% names(shiny::resourcePaths()))) {
      shiny::addResourcePath(rv$exports_prefix, rv$export_dir)
    }

    tryCatch({
      clusters_choices <- as.character(rv$clusters)
      if (length(clusters_choices) == 0 && !is.null(rv$res_stats_df) && "Classe" %in% names(rv$res_stats_df)) {
        clusters_choices <- unique(as.character(rv$res_stats_df$Classe))
      }
      clusters_choices <- clusters_choices[!is.na(clusters_choices) & nzchar(clusters_choices)]
      if (length(clusters_choices) == 0) clusters_choices <- "1"

      classe_defaut <- clusters_choices[1]
      max_k_plot <- suppressWarnings(as.integer(rv$max_n_groups_chd))
      if (!is.finite(max_k_plot) || is.na(max_k_plot) || max_k_plot < 2) {
        max_k_plot <- max(2L, length(unique(clusters_choices)))
      }

      k_plot_defaut <- suppressWarnings(as.integer(input$k))
      if (!is.finite(k_plot_defaut) || is.na(k_plot_defaut) || k_plot_defaut < 2) {
        k_plot_defaut <- min(max_k_plot, 3L)
      }
      k_plot_defaut <- max(2L, min(max_k_plot, k_plot_defaut))

      n_terms_plot_defaut <- 20L

      concordancier_src <- NULL
      html_file_ok <- !is.null(rv$html_file) && length(rv$html_file) == 1 && !is.na(rv$html_file) && nzchar(rv$html_file)
      if (isTRUE(html_file_ok) && isTRUE(file.exists(rv$html_file))) {
        nom_html <- basename(rv$html_file)
        src_html <- file.path(rv$export_dir, nom_html)
        if (!isTRUE(file.exists(src_html))) {
          ok_copy <- tryCatch(file.copy(rv$html_file, src_html, overwrite = TRUE), error = function(e) FALSE)
          if (!isTRUE(ok_copy)) src_html <- rv$html_file
        }
        concordancier_src <- paste0("/", rv$exports_prefix, "/", basename(src_html))
      }

      removeModal()
      showModal(modalDialog(
        title = "Explore_rainette",
        size = "l",
        easyClose = TRUE,
        footer = modalButton("Fermer"),

        selectInput("classe_viz", "Classe", choices = clusters_choices, selected = classe_defaut),

        tabsetPanel(
          tabPanel(
            "CHD",
            fluidRow(
              column(
                4,
                sliderInput("k_plot", "Nombre de classes (k)", min = 2, max = max_k_plot, value = k_plot_defaut, step = 1),
                selectInput(
                  "measure_plot", "Statistiques",
                  choices = c(
                    "Frequency - Terms" = "frequency",
                    "Keyness - Chi-squared" = "chi2",
                    "Keyness - Likelihood ratio" = "lr",
                    "Frequency - Documents proportion" = "docprop"
                  ),
                  selected = "frequency"
                ),
                selectInput("type_plot", "Type", choices = c("bar", "cloud"), selected = "bar"),
                numericInput("n_terms_plot", "Nombre de termes", value = n_terms_plot_defaut, min = 5, max = 1000, step = 1),
                conditionalPanel(
                  "input.measure_plot != 'docprop'",
                  checkboxInput("same_scales_plot", "Forcer les mêmes échelles", value = TRUE)
                ),
                checkboxInput("show_negative_plot", "Afficher les valeurs négatives", value = FALSE),
                numericInput("text_size_plot", "Taille du texte", value = 12, min = 6, max = 30, step = 1)
              ),
              column(
                8,
                plotOutput("plot_chd", height = "70vh")
              )
            )
          ),
          tabPanel(
            "Concordancier HTML",
            if (is.null(concordancier_src)) {
              tags$div(
                style = "padding: 12px;",
                tags$p("Le fichier du concordancier HTML n'est pas disponible pour cette analyse."),
                tags$p("Relance l'analyse puis vérifie les logs si le problème persiste.")
              )
            } else {
              tags$iframe(
                src = concordancier_src,
                style = "width: 100%; height: 70vh; border: 1px solid #999;"
              )
            }
          ),
          tabPanel("Wordcloud", uiOutput("ui_wordcloud")),
          tabPanel("Cooccurrences", uiOutput("ui_cooc")),
          tabPanel("Statistiques", tableOutput("table_stats_classe"))
        )
      ))
    }, error = function(e) {
      removeModal()
      showNotification(paste0("Impossible d'ouvrir Explore_rainette : ", conditionMessage(e)), type = "error", duration = 10)
      invisible(NULL)
    })
  })

  output$plot_afc_classes <- renderPlot({
    if (!is.null(rv$afc_erreur) && nzchar(rv$afc_erreur)) {
      plot.new()
      text(0.5, 0.5, "AFC indisponible (erreur).", cex = 1.1)
      return(invisible(NULL))
    }
    if (is.null(rv$afc_obj) || is.null(rv$afc_obj$ca)) {
      plot.new()
      text(0.5, 0.5, "AFC non disponible. Lance une analyse.", cex = 1.1)
      return(invisible(NULL))
    }
    tracer_afc_classes_seules(rv$afc_obj, axes = c(1, 2), cex_labels = 1.05)
  })

  observe({
    req(rv$res_stats_df)
    req("Classe" %in% names(rv$res_stats_df))

    classes <- sort(unique(suppressWarnings(as.numeric(rv$res_stats_df$Classe))))
    classes <- classes[is.finite(classes)]
    if (length(classes) == 0) return(invisible(NULL))

    choix <- as.character(classes)
    selected <- input$classe_viz_iramuteq
    if (is.null(selected) || !selected %in% choix) selected <- choix[[1]]

    updateSelectInput(session, "classe_viz_iramuteq", choices = choix, selected = selected)
  })

  output$plot_chd_iramuteq_dendro <- renderPlot({
    if (!identical(rv$res_type, "iramuteq")) {
      plot.new()
      text(0.5, 0.5, "Dendrogramme IRaMuTeQ-like indisponible (mode Rainette actif).", cex = 1.05)
      return(invisible(NULL))
    }

    req(rv$res)
    chd_obj <- rv$res$chd
    terminales <- rv$res$terminales

    tryCatch({
      tracer_dendrogramme_chd_iramuteq(chd_obj = chd_obj, terminales = terminales)
    }, error = function(e) {
      plot.new()
      text(0.5, 0.5, paste0("Erreur dendrogramme IRaMuTeQ-like: ", conditionMessage(e)), cex = 0.95)
      invisible(NULL)
    })
  })

  output$plot_chd_iramuteq <- renderPlot({
    if (!identical(rv$res_type, "iramuteq")) {
      plot.new()
      text(0.5, 0.5, "Résultats CHD IRaMuTeQ-like indisponibles (mode Rainette actif).", cex = 1.05)
      return(invisible(NULL))
    }

    req(rv$res_stats_df)

    mesure_sel <- if (is.null(input$measure_plot_iramuteq)) "frequency" else as.character(input$measure_plot_iramuteq)
    type_sel <- if (is.null(input$type_plot_iramuteq)) "bar" else as.character(input$type_plot_iramuteq)
    n_terms_sel <- if (is.null(input$n_terms_plot_iramuteq)) 20 else as.integer(input$n_terms_plot_iramuteq)

    tracer_chd_iramuteq(
      res_stats_df = rv$res_stats_df,
      classe = input$classe_viz_iramuteq,
      mesure = mesure_sel,
      type = type_sel,
      n_terms = n_terms_sel,
      show_negative = isTRUE(input$show_negative_plot_iramuteq)
    )
  })

  output$ui_tables_stats_chd_iramuteq <- renderUI({
    if (!identical(rv$res_type, "iramuteq")) {
      return(tags$p("Résultats CHD IRaMuTeQ-like indisponibles (mode Rainette actif)."))
    }

    req(rv$res_stats_df)
    req("Classe" %in% names(rv$res_stats_df))

    classes <- sort(unique(suppressWarnings(as.numeric(rv$res_stats_df$Classe))))
    classes <- classes[is.finite(classes)]
    if (length(classes) == 0) {
      return(tags$p("Aucune classe disponible pour les statistiques CHD."))
    }

    panneaux <- lapply(classes, function(cl) {
      output_id <- paste0("table_stats_chd_iramuteq_cl_", cl)

      output[[output_id]] <- renderTable({
        extraire_stats_chd_classe(rv$res_stats_df, classe = cl, n_max = 100)
      }, rownames = FALSE)

      tabPanel(
        title = paste0("Classe ", cl),
        tableOutput(output_id)
      )
    })

    do.call(tabsetPanel, c(id = "tabs_stats_chd_iramuteq", panneaux))
  })

  output$plot_chd <- renderPlot({
    req(!is.null(input$measure_plot), !is.null(input$type_plot), !is.null(input$n_terms_plot))

    if (identical(rv$res_type, "iramuteq")) {
      req(rv$res_stats_df)
      tracer_chd_iramuteq(
        res_stats_df = rv$res_stats_df,
        classe = input$classe_viz,
        mesure = as.character(input$measure_plot),
        type = as.character(input$type_plot),
        n_terms = input$n_terms_plot,
        show_negative = isTRUE(input$show_negative_plot)
      )
      return(invisible(NULL))
    }

    req(rv$res_chd, rv$dfm_chd)
    req(!is.null(input$k_plot))

    same_scales <- isTRUE(input$same_scales_plot)
    show_negative <- isTRUE(input$show_negative_plot)

    rainette_plot(
      rv$res_chd,
      rv$dfm_chd,
      k = input$k_plot,
      type = input$type_plot,
      n_terms = input$n_terms_plot,
      free_scales = !same_scales,
      measure = input$measure_plot,
      show_negative = show_negative,
      text_size = input$text_size_plot
    )
  })

  output$ui_wordcloud <- renderUI({
    req(input$classe_viz, rv$exports_prefix, rv$export_dir)

    src_rel <- file.path("wordclouds", paste0("cluster_", input$classe_viz, "_wordcloud.png"))
    if (!file.exists(file.path(rv$export_dir, src_rel))) {
      return(tags$p("Aucun nuage de mots disponible pour cette classe."))
    }

    tags$div(
      style = "text-align: center;",
      tags$img(
        src = paste0("/", rv$exports_prefix, "/", src_rel),
        style = "max-width: 100%; height: auto; border: 1px solid #999; display: inline-block;"
      )
    )
  })

  output$ui_cooc <- renderUI({
    req(input$classe_viz, rv$exports_prefix, rv$export_dir)

    src_rel <- file.path("cooccurrences", paste0("cluster_", input$classe_viz, "_fcm_network.png"))
    if (!file.exists(file.path(rv$export_dir, src_rel))) {
      return(tags$p("Aucune cooccurrence disponible pour cette classe."))
    }

    tags$img(src = paste0("/", rv$exports_prefix, "/", src_rel), style = "max-width: 100%; height: auto; border: 1px solid #999;")
  })

  output$table_stats_classe <- renderTable({
    req(input$classe_viz, rv$res_stats_df)
    extraire_stats_chd_classe(rv$res_stats_df, classe = input$classe_viz, n_max = 50)
  }, rownames = FALSE)

  output$plot_afc <- renderPlot({
    if (!is.null(rv$afc_erreur) && nzchar(rv$afc_erreur)) {
      plot.new()
      text(0.5, 0.5, "AFC indisponible (erreur).", cex = 1.1)
      return(invisible(NULL))
    }
    if (is.null(rv$afc_obj) || is.null(rv$afc_obj$ca)) {
      plot.new()
      text(0.5, 0.5, "AFC non disponible. Lance une analyse.", cex = 1.1)
      return(invisible(NULL))
    }

    activer_repel <- TRUE
    if (!is.null(input$afc_reduire_chevauchement)) activer_repel <- isTRUE(input$afc_reduire_chevauchement)

    taille_sel <- "frequency"
    if (!is.null(input$afc_taille_mots) && nzchar(as.character(input$afc_taille_mots))) {
      taille_sel <- as.character(input$afc_taille_mots)
    }
    if (!taille_sel %in% c("frequency", "chi2")) taille_sel <- "frequency"

    top_termes <- 120
    if (!is.null(input$afc_top_termes) && is.finite(input$afc_top_termes)) top_termes <- as.integer(input$afc_top_termes)

    tracer_afc_classes_termes(rv$afc_obj, axes = c(1, 2), top_termes = top_termes, taille_sel = taille_sel, activer_repel = activer_repel)
  })

  output$ui_table_afc_mots_par_classe <- renderUI({
    if (is.null(rv$afc_table_mots)) {
      output$table_afc_mots_message <- renderTable({
        data.frame(Message = "AFC mots : non disponible.", stringsAsFactors = FALSE)
      }, rownames = FALSE)
      return(tableOutput("table_afc_mots_message"))
    }

    df <- rv$afc_table_mots
    colonnes <- intersect(c("Terme", "Classe_max", "frequency", "chi2", "p_value", "Segment_texte"), names(df))
    df <- df[, colonnes, drop = FALSE]
    if ("p_value" %in% names(df)) {
      df$p_value <- ifelse(
        is.na(df$p_value),
        NA_character_,
        formatC(df$p_value, format = "f", digits = 6)
      )
    }

    classes <- unique(as.character(df$Classe_max))
    classes <- classes[!is.na(classes) & nzchar(classes)]
    classes <- sort(classes)

    if (length(classes) == 0) {
      output$table_afc_mots_message <- renderTable({
        data.frame(Message = "AFC mots : aucune classe disponible.", stringsAsFactors = FALSE)
      }, rownames = FALSE)
      return(tableOutput("table_afc_mots_message"))
    }

    ui_tables <- lapply(seq_along(classes), function(i) {
      cl <- classes[[i]]
      id <- paste0("table_afc_mots_", i)

      output[[id]] <- renderUI({
        sous_df <- df[df$Classe_max == cl, , drop = FALSE]
        colonnes <- intersect(c("Terme", "frequency", "chi2", "p_value", "Segment_texte"), names(sous_df))
        sous_df <- sous_df[, colonnes, drop = FALSE]

        if ("p_value" %in% names(sous_df)) {
          sous_df$p_value <- ifelse(
            is.na(sous_df$p_value),
            NA_character_,
            formatC(sous_df$p_value, format = "f", digits = 6)
          )
        }

        if ("chi2" %in% names(sous_df)) {
          sous_df <- sous_df[order(-sous_df$chi2), , drop = FALSE]
          sous_df$chi2 <- ifelse(
            is.na(sous_df$chi2),
            NA_character_,
            formatC(sous_df$chi2, format = "f", digits = 6)
          )
        }

        sous_df <- head(sous_df, 100)
        generer_table_html_afc_mots(sous_df)
      })

      tagList(
        tags$h5(cl),
        uiOutput(id)
      )
    })

    do.call(tagList, ui_tables)
  })

  output$plot_afc_vars <- renderPlot({
    if (!is.null(rv$afc_vars_erreur) && nzchar(rv$afc_vars_erreur)) {
      plot.new()
      text(0.5, 0.5, "AFC variables étoilées indisponible (erreur).", cex = 1.1)
      return(invisible(NULL))
    }
    if (is.null(rv$afc_vars_obj) || is.null(rv$afc_vars_obj$ca)) {
      plot.new()
      text(0.5, 0.5, "AFC variables étoilées non disponible. Lance une analyse.", cex = 1.1)
      return(invisible(NULL))
    }

    activer_repel <- TRUE
    if (!is.null(input$afc_reduire_chevauchement)) activer_repel <- isTRUE(input$afc_reduire_chevauchement)

    top_mod <- 120
    if (!is.null(input$afc_top_modalites) && is.finite(input$afc_top_modalites)) top_mod <- as.integer(input$afc_top_modalites)

    tracer_afc_variables_etoilees(rv$afc_vars_obj, axes = c(1, 2), top_modalites = top_mod, activer_repel = activer_repel)
  })

  output$table_afc_vars <- renderTable({
    if (is.null(rv$afc_table_vars)) {
      return(data.frame(Message = "AFC variables étoilées : non disponible.", stringsAsFactors = FALSE))
    }
    df <- rv$afc_table_vars
    colonnes <- intersect(c("Modalite", "Classe_max", "frequency", "chi2", "p_value"), names(df))
    df <- df[, colonnes, drop = FALSE]
    if ("p_value" %in% names(df)) {
      p_values <- df$p_value
      df$p_value <- ifelse(
        is.na(p_values),
        NA_character_,
        ifelse(
          p_values > 0.05,
          sprintf("<span style='color:#d97706;font-weight:600;'>%s</span>", formatC(p_values, format = "f", digits = 6)),
          formatC(p_values, format = "f", digits = 6)
        )
      )
    }
    if ("chi2" %in% names(df)) df <- df[order(-df$chi2), , drop = FALSE]
    if ("chi2" %in% names(df)) {
      df$chi2 <- ifelse(
        is.na(df$chi2),
        NA_character_,
        formatC(df$chi2, format = "f", digits = 6)
      )
    }
    head(df, 200)
  }, rownames = FALSE, sanitize.text.function = function(x) x)

  output$table_afc_eig <- renderTable({
    if (!is.null(rv$afc_erreur) && nzchar(rv$afc_erreur)) {
      return(data.frame(Message = "AFC indisponible (erreur).", stringsAsFactors = FALSE))
    }
    if (is.null(rv$afc_obj) || is.null(rv$afc_obj$ca)) {
      return(data.frame(Message = "AFC non disponible.", stringsAsFactors = FALSE))
    }
    eig <- rv$afc_obj$ca$eig
    if (is.null(eig)) return(data.frame(Message = "Valeurs propres indisponibles.", stringsAsFactors = FALSE))
    df <- as.data.frame(eig)
    df$Dim <- rownames(df)
    rownames(df) <- NULL
    df <- df[, c("Dim", names(df)[1], names(df)[2], names(df)[3]), drop = FALSE]
    names(df) <- c("Dim", "Valeur_propre", "Pourcentage_inertie", "Pourcentage_cumule")
    df
  }, rownames = FALSE)

  output$dl_segments <- downloadHandler(
    filename = function() "segments_par_classe.txt",
    content = function(file) {
      req(rv$segments_file)
      file.copy(rv$segments_file, file, overwrite = TRUE)
    }
  )

  output$dl_stats <- downloadHandler(
    filename = function() "stats_par_classe.csv",
    content = function(file) {
      req(rv$stats_file)
      file.copy(rv$stats_file, file, overwrite = TRUE)
    }
  )

  output$dl_html <- downloadHandler(
    filename = function() "segments_par_classe.html",
    content = function(file) {
      req(rv$html_file)
      file.copy(rv$html_file, file, overwrite = TRUE)
    }
  )

  output$dl_zip <- downloadHandler(
    filename = function() "exports_rainette.zip",
    content = function(file) {
      req(rv$zip_file)
      file.copy(rv$zip_file, file, overwrite = TRUE)
    }
  )

  output$dl_afc_zip <- downloadHandler(
    filename = function() "afc_exports.zip",
    content = function(file) {
      req(rv$afc_dir)
      zip_tmp <- tempfile(fileext = ".zip")
      ancien <- getwd()
      on.exit(setwd(ancien), add = TRUE)
      setwd(dirname(rv$afc_dir))
      if (file.exists(zip_tmp)) unlink(zip_tmp)
      utils::zip(zipfile = zip_tmp, files = basename(rv$afc_dir))
      file.copy(zip_tmp, file, overwrite = TRUE)
    }
  )

}

app <- shinyApp(ui = ui, server = server)
app

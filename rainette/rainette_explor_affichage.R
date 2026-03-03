# Description des fonctions du script `rainette_explor_affichage.R`
# - register_rainette_explor_affichage(input, output, session, rv) :
#   enregistre les observateurs et sorties Shiny de l'onglet « Explore rainette »
#   (navigation, chargement du concordancier, graphiques CHD, nuages de mots,
#   réseaux de cooccurrences et tableau des statistiques par classe).
# - update_explore_controls() [fonction interne] :
#   initialise et synchronise les contrôles d'exploration (classe, k du graphe,
#   nombre de termes) à partir des résultats courants.

register_rainette_explor_affichage <- function(input, output, session, rv) {
  normaliser_id_classe_explore <- function(x) {
    x_chr <- trimws(as.character(x))
    if (!length(x_chr) || is.na(x_chr) || !nzchar(x_chr)) return(NA_integer_)

    x_num <- suppressWarnings(as.integer(x_chr))
    if (!is.na(x_num)) return(x_num)

    extrait <- sub("^.*?(\\d+).*$", "\\1", x_chr)
    if (!grepl("\\d", x_chr)) return(NA_integer_)
    suppressWarnings(as.integer(extrait))
  }

  trouver_export_par_classe <- function(export_dir, sous_dossier, suffixe, classe_input = NULL) {
    dir_path <- file.path(export_dir, sous_dossier)
    if (!dir.exists(dir_path)) return(NULL)

    motif <- paste0("^cluster_(.+)_", suffixe, "\\.png$")
    fichiers <- list.files(dir_path, pattern = motif, full.names = FALSE)
    if (length(fichiers) == 0) return(NULL)

    if (!is.null(classe_input) && nzchar(as.character(classe_input))) {
      candidats <- unique(c(
        as.character(classe_input),
        as.character(normaliser_id_classe_explore(classe_input))
      ))
      candidats <- candidats[!is.na(candidats) & nzchar(candidats)]

      for (cl in candidats) {
        cible <- paste0("cluster_", cl, "_", suffixe, ".png")
        if (cible %in% fichiers) return(file.path(sous_dossier, cible))
      }
    }

    file.path(sous_dossier, sort(fichiers)[[1]])
  }

  update_explore_controls <- function() {
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

    updateSelectInput(session, "classe_viz", choices = clusters_choices, selected = classe_defaut)
    updateSliderInput(session, "k_plot", min = 2, max = max_k_plot, value = k_plot_defaut)
    updateNumericInput(session, "n_terms_plot", value = 20L)
  }

  observeEvent(rv$res, {
    update_explore_controls()
  }, ignoreInit = TRUE)

  observeEvent(rv$res_stats_df, {
    req(!is.null(rv$res_stats_df))
    update_explore_controls()
  }, ignoreInit = TRUE)

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
      update_explore_controls()
      updateTabsetPanel(session, "onglets_principaux", selected = "Explore rainette")
    }, error = function(e) {
      showNotification(paste0("Impossible d'ouvrir Explore_rainette : ", conditionMessage(e)), type = "error", duration = 10)
      invisible(NULL)
    })
  })

  output$ui_concordancier_explore <- renderUI({
    req(rv$export_dir)

    if (is.null(rv$exports_prefix) || !nzchar(rv$exports_prefix)) {
      return(tags$div(
        style = "padding: 12px;",
        tags$p("Préfixe de ressources invalide."),
        tags$p("Relance l'analyse pour régénérer les exports.")
      ))
    }

    if (!(rv$exports_prefix %in% names(shiny::resourcePaths()))) {
      shiny::addResourcePath(rv$exports_prefix, rv$export_dir)
    }

    concordancier_src <- NULL

    candidats_html <- c(
      rv$html_file,
      file.path(rv$export_dir, "segments_par_classe.html"),
      file.path(rv$export_dir, "concordancier.html")
    )

    # Fallback robuste : certains pipelines peuvent produire un nom de fichier
    # légèrement différent. On récupère alors le premier HTML concordancier
    # détecté dans le répertoire d'export.
    candidats_dyn <- list.files(
      rv$export_dir,
      pattern = "(segments.*classe|concord).*\\.html$",
      ignore.case = TRUE,
      full.names = TRUE
    )
    candidats_html <- c(candidats_html, candidats_dyn)

    candidats_html <- unique(candidats_html[!is.na(candidats_html) & nzchar(candidats_html)])
    html_existant <- candidats_html[file.exists(candidats_html)]

    if (length(html_existant) > 0) {
      src_html <- html_existant[[1]]
      nom_html <- basename(src_html)
      src_dans_exports <- file.path(rv$export_dir, nom_html)

      if (!isTRUE(file.exists(src_dans_exports))) {
        ok_copy <- tryCatch(file.copy(src_html, src_dans_exports, overwrite = TRUE), error = function(e) FALSE)
        if (isTRUE(ok_copy)) src_html <- src_dans_exports
      } else {
        src_html <- src_dans_exports
      }

      concordancier_src <- paste0("/", rv$exports_prefix, "/", basename(src_html))
    }

    if (is.null(concordancier_src)) {
      return(tags$div(
        style = "padding: 12px;",
        tags$p("Le fichier du concordancier HTML n'est pas disponible pour cette analyse."),
        tags$p("Relance l'analyse puis vérifie les logs si le problème persiste.")
      ))
    }

    tags$iframe(
      src = concordancier_src,
      style = "width: 100%; height: 70vh; border: 1px solid #999;"
    )
  })

  output$plot_chd <- renderPlot({
    req(!is.null(input$measure_plot), !is.null(input$type_plot), !is.null(input$n_terms_plot))

    if (identical(rv$res_type, "iramuteq")) {
      plot.new()
      text(0.5, 0.5, "Explore rainette indisponible en mode IRaMuTeQ-like.", cex = 1.05)
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

    src_rel <- trouver_export_par_classe(
      export_dir = rv$export_dir,
      sous_dossier = "wordclouds",
      suffixe = "wordcloud",
      classe_input = input$classe_viz
    )
    if (is.null(src_rel) || !file.exists(file.path(rv$export_dir, src_rel))) {
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

    src_rel <- trouver_export_par_classe(
      export_dir = rv$export_dir,
      sous_dossier = "cooccurrences",
      suffixe = "fcm_network",
      classe_input = input$classe_viz
    )
    if (is.null(src_rel) || !file.exists(file.path(rv$export_dir, src_rel))) {
      return(tags$p("Aucune cooccurrence disponible pour cette classe."))
    }

    tags$img(src = paste0("/", rv$exports_prefix, "/", src_rel), style = "max-width: 100%; height: auto; border: 1px solid #999;")
  })

  output$table_stats_classe <- renderTable({
    req(input$classe_viz, rv$res_stats_df)
    extraire_stats_chd_classe(
      rv$res_stats_df,
      classe = input$classe_viz,
      n_max = 50,
      max_p = if (isTRUE(input$filtrer_affichage_pvalue)) input$max_p else 1,
      seuil_p_significativite = input$max_p,
      style = "iramuteq_clone"
    )
  }, rownames = FALSE, sanitize.text.function = function(x) x)
}

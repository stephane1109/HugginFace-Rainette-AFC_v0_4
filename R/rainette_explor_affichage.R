register_rainette_explor_affichage <- function(input, output, session, rv) {
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
}

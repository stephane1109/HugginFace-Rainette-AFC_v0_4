# Rôle du fichier: server_outputs_status.R porte une partie du pipeline d'analyse Rainette.
# Ce script centralise une responsabilité métier/technique utilisée par l'application.
# Il facilite la maintenance en explicitant le périmètre et les points d'intégration.
# Module server - sorties de statut global
# Ce fichier enregistre les sorties Shiny de statut transversal (logs, statut,
# barre de progression, table de classes) pour alléger `app.R` sans changer la logique.

register_outputs_status <- function(input, output, session, rv) {
    output$logs <- renderText(rv$logs)
    output$statut <- renderText(rv$statut)

    output$barre_progression <- renderUI({
      p <- max(0, min(100, rv$progression))
      tags$div(
        style = "width: 100%; border: 1px solid #999; height: 20px; position: relative;",
        tags$div(style = paste0("width: ", p, "%; height: 100%; background-color: #4C9AFF;")),
        tags$div(
          style = "position: absolute; top: 0; left: 0; width: 100%; height: 100%; text-align: center; line-height: 20px; font-size: 12px;",
          paste0(p, "%")
        )
      )
    })

    output$table_classes <- renderTable({
      req(rv$filtered_corpus)
      classes_brutes <- docvars(rv$filtered_corpus)$Classes

      # Harmonisation des identifiants de classes (ex. "Classe 3", "3", facteur)
      # pour éviter des doublons d'affichage artificiels avec IRaMuTeQ-like.
      classes_norm <- suppressWarnings(as.integer(classes_brutes))
      if (all(is.na(classes_norm))) {
        classes_norm <- suppressWarnings(as.integer(gsub("[^0-9-]", "", as.character(classes_brutes))))
      }

      classes_norm <- classes_norm[!is.na(classes_norm) & classes_norm > 0L]
      validate(need(length(classes_norm) > 0, "Aucune classe exploitable."))

      tb <- table(factor(classes_norm, levels = sort(unique(classes_norm))), useNA = "no")
      effectifs <- as.integer(tb)
      pourcentages <- round((effectifs / sum(effectifs)) * 100, 1)
      data.frame(
        Classe = names(tb),
        Effectif = effectifs,
        `Pourcentage (%)` = pourcentages,
        stringsAsFactors = FALSE
      )
    }, rownames = FALSE)
}

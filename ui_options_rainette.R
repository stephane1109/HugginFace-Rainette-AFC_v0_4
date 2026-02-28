# Rôle du fichier: ui_options_rainette.R définit les options UI spécifiques à la méthode Rainette.

library(shiny)

ui_options_rainette <- function() {
  tagList(
    tags$div(class = "sidebar-section-title", "Paramètres CHD (Rainette)"),
    numericInput("k", "k (nombre de classes)", value = 3, min = 2, step = 1),
    numericInput("min_segment_size", "Nombre minimal de termes par segment (min_segment_size)", value = 10, min = 1, step = 1),
    numericInput("min_split_members", "Effectif minimal pour scinder une classe (min_split_members)", value = 10, min = 1, step = 1),
    radioButtons(
      "type_classification",
      "Type de classification",
      choices = c(
        "Classification simple (rainette)" = "simple",
        "Classification double (rainette2)" = "double"
      ),
      selected = "simple",
      inline = FALSE
    ),
    conditionalPanel(
      condition = "input.type_classification == 'double'",
      numericInput("min_segment_size2", "min_segment_size (classification 2)", value = 15, min = 1, step = 1),
      numericInput("max_k_double", "max_k (rainette2)", value = 8, min = 2, step = 1)
    )
  )
}

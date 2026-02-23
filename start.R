# Rôle du fichier: start.R porte une partie du pipeline d'analyse Rainette.
# Ce script centralise une responsabilité métier/technique utilisée par l'application.
# Il facilite la maintenance en explicitant le périmètre et les points d'intégration.
options(
  repos = c(CRAN = "https://cloud.r-project.org"),
  bspm.sudo = TRUE
)

port <- as.integer(Sys.getenv("PORT", "7860"))
app_dir <- "/home/user/app"
app_file <- file.path(app_dir, "app.R")

app_env <- new.env(parent = globalenv())
source_result <- source(app_file, local = app_env, chdir = TRUE)
app_obj <- source_result$value

if (!inherits(app_obj, "shiny.appobj")) {
  if (exists("app", envir = app_env, inherits = FALSE) && inherits(app_env$app, "shiny.appobj")) {
    app_obj <- app_env$app
  } else if (
    exists("ui", envir = app_env, inherits = FALSE) &&
    exists("server", envir = app_env, inherits = FALSE) &&
    is.function(app_env$server)
  ) {
    app_obj <- shiny::shinyApp(ui = app_env$ui, server = app_env$server)
  } else if (
    exists("ui", envir = globalenv(), inherits = FALSE) &&
    exists("server", envir = globalenv(), inherits = FALSE) &&
    is.function(get("server", envir = globalenv(), inherits = FALSE))
  ) {
    app_obj <- shiny::shinyApp(
      ui = get("ui", envir = globalenv(), inherits = FALSE),
      server = get("server", envir = globalenv(), inherits = FALSE)
    )
  } else {
    app_obj <- shiny::shinyAppDir(app_dir)
  }
}

shiny::runApp(app_obj, host = "0.0.0.0", port = port)

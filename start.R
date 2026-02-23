options(repos = c(CRAN = "https://cloud.r-project.org"))

auto_update_rainette <- tolower(Sys.getenv("AUTO_UPDATE_RAINETTE", "true")) %in% c("1", "true", "yes")

if (auto_update_rainette) {
  message("AUTO_UPDATE_RAINETTE=true -> tentative de mise à jour de rainette")
  try(install.packages("rainette", quiet = TRUE), silent = TRUE)
}

port <- as.integer(Sys.getenv("PORT", "7860"))
app_dir <- "/home/user/app"
app_file <- file.path(app_dir, "app.R")

app_env <- new.env(parent = globalenv())
app_obj <- source(app_file, local = app_env, chdir = TRUE)$value

if (!inherits(app_obj, "shiny.appobj")) {
  if (exists("app", envir = app_env, inherits = FALSE) && inherits(app_env$app, "shiny.appobj")) {
    app_obj <- app_env$app
  } else if (
    exists("ui", envir = app_env, inherits = FALSE) &&
    exists("server", envir = app_env, inherits = FALSE) &&
    is.function(app_env$server)
  ) {
    app_obj <- shiny::shinyApp(ui = app_env$ui, server = app_env$server)
  } else {
    stop("app.R doit retourner un objet shiny.appobj ou définir ui + server.")
  }
}

shiny::runApp(app_obj, host = "0.0.0.0", port = port)

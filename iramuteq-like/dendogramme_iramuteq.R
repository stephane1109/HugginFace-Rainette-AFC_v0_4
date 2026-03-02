# Rôle du fichier: point d'entrée UI pour le tracé du dendrogramme IRaMuTeQ-like.

tracer_dendogramme_iramuteq_ui <- function(rv,
                                            top_n_terms = 4,
                                            orientation = "vertical") {
  if (is.null(rv$res) || is.null(rv$res$chd)) {
    plot.new()
    text(0.5, 0.5, "Dendrogramme CHD indisponible.", cex = 1.1)
    return(invisible(NULL))
  }

  tracer_dendrogramme_chd_iramuteq(
    chd_obj = rv$res$chd,
    terminales = rv$res$terminales,
    classes = rv$res$classes,
    res_stats_df = rv$res_stats_df,
    top_n_terms = top_n_terms,
    orientation = orientation
  )

  invisible(NULL)
}

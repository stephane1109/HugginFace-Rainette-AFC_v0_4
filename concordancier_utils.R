# Rôle du fichier: concordancier_utils.R porte une partie du pipeline d'analyse Rainette.
# Ce script centralise une responsabilité métier/technique utilisée par l'application.
# Il facilite la maintenance en explicitant le périmètre et les points d'intégration.
# concordancier_utils.R

horodater <- function() format(Sys.time(), "%Y-%m-%d %H:%M:%S")

ajouter_log <- function(rv, texte) {
  rv$logs <- paste(rv$logs, paste0("[", horodater(), "] ", texte), sep = "\n")
}

trans_nfd <- function(x) {
  if (requireNamespace("stringi", quietly = TRUE)) return(stringi::stri_trans_nfd(x))
  x
}

trans_nfc <- function(x) {
  if (requireNamespace("stringi", quietly = TRUE)) return(stringi::stri_trans_nfc(x))
  x
}

echapper_regex <- function(x) {
  gsub("([\\^\\$\\*\\+\\?\\(\\)\\[\\]\\{\\}\\.\\|\\\\\\-])", "\\\\\\1", x)
}

echapper_html <- function(x) {
  x <- gsub("&", "&amp;", x, fixed = TRUE)
  x <- gsub("<", "&lt;", x, fixed = TRUE)
  x <- gsub(">", "&gt;", x, fixed = TRUE)
  x
}

expandir_variantes_termes <- function(termes) {
  termes <- unique(as.character(termes))
  termes <- trimws(termes)
  termes <- termes[!is.na(termes) & nzchar(termes)]
  if (length(termes) == 0) return(character(0))

  variantes <- unique(c(
    termes,
    gsub("’", "'", termes, fixed = TRUE),
    gsub("'", "’", termes, fixed = TRUE)
  ))

  variantes[!is.na(variantes) & nzchar(variantes)]
}

echapper_segments_en_preservant_surlignage <- function(segments, start_tag, end_tag) {
  if (length(segments) == 0) return(segments)

  start_placeholder <- "__RAINETTE_HL_START__"
  end_placeholder <- "__RAINETTE_HL_END__"

  x <- gsub(start_tag, start_placeholder, segments, fixed = TRUE)
  x <- gsub(end_tag, end_placeholder, x, fixed = TRUE)
  x <- echapper_html(x)
  x <- gsub(start_placeholder, start_tag, x, fixed = TRUE)
  x <- gsub(end_placeholder, end_tag, x, fixed = TRUE)
  x
}

normaliser_id_classe <- function(x) {
  x_chr <- as.character(x)
  x_chr <- trimws(x_chr)
  x_num <- suppressWarnings(as.numeric(x_chr))

  need_extract <- is.na(x_num) & !is.na(x_chr) & nzchar(x_chr)
  if (any(need_extract)) {
    extrait <- sub("^.*?(\\d+).*$", "\\1", x_chr[need_extract])
    extrait[!grepl("\\d", x_chr[need_extract])] <- NA_character_
    x_num[need_extract] <- suppressWarnings(as.numeric(extrait))
  }

  x_num
}

construire_regex_terme_nfd <- function(terme) {
  if (is.na(terme) || !nzchar(terme)) return("")

  t <- tolower(trans_nfd(terme))
  chars <- strsplit(t, "", fixed = TRUE)[[1]]
  if (length(chars) == 0) return("")

  pieces <- vapply(chars, function(ch) {
    if (grepl("\\p{M}", ch, perl = TRUE)) return("")
    if (ch %in% c(" ", "\t", "\n", "\r")) return("\\s+")
    if (ch %in% c("'", "’")) return("['’]")

    if (grepl("\\p{L}", ch, perl = TRUE)) {
      return(paste0(echapper_regex(ch), "\\p{M}*"))
    }

    echapper_regex(ch)
  }, FUN.VALUE = character(1))

  paste0(pieces, collapse = "")
}

preparer_motifs_surlignage_nfd <- function(terms, taille_lot = 80) {
  terms <- unique(terms)
  terms <- terms[!is.na(terms) & nzchar(terms)]
  if (length(terms) == 0) return(list())

  terms <- terms[order(nchar(terms), decreasing = TRUE)]
  patterns <- vapply(terms, construire_regex_terme_nfd, FUN.VALUE = character(1))
  patterns <- patterns[nzchar(patterns)]
  if (length(patterns) == 0) return(list())

  lots <- split(patterns, ceiling(seq_along(patterns) / taille_lot))

  lapply(lots, function(lot) {
    paste0(
      "(?i)(?<![\\p{L}\\p{M}])(",
      paste0(lot, collapse = "|"),
      ")(?![\\p{L}\\p{M}])"
    )
  })
}

surligner_vecteur_html_unicode <- function(segments, motifs, start_tag, end_tag, on_error = NULL) {
  if (length(segments) == 0 || length(motifs) == 0) return(segments)

  out <- segments
  for (i in seq_along(out)) {
    s <- out[i]
    if (is.na(s) || !nzchar(s)) next

    s_nfd <- trans_nfd(s)

    for (pat in motifs) {
      s_nfd <- tryCatch(
        gsub(pat, paste0(start_tag, "\\1", end_tag), s_nfd, perl = TRUE),
        error = function(e) {
          if (is.function(on_error)) on_error(e, pat)
          s_nfd
        }
      )
    }

    out[i] <- trans_nfc(s_nfd)
  }
  out
}

detecter_segments_contenant_termes_unicode <- function(textes_index, termes) {
  if (length(textes_index) == 0) return(logical(0))

  termes <- unique(termes)
  termes <- termes[!is.na(termes) & nzchar(termes)]
  if (length(termes) == 0) return(rep(TRUE, length(textes_index)))

  motifs <- preparer_motifs_surlignage_nfd(termes, taille_lot = 80)
  if (length(motifs) == 0) return(rep(TRUE, length(textes_index)))

  present <- rep(FALSE, length(textes_index))
  textes_nfd <- trans_nfd(textes_index)

  for (pat in motifs) {
    ok <- tryCatch(grepl(pat, textes_nfd, perl = TRUE), error = function(e) rep(FALSE, length(textes_nfd)))
    present <- present | ok
  }

  present
}

# concordancier_spacy.R
# Concordancier dédié au mode dictionnaire spaCy.

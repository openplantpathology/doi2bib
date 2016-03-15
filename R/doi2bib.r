#' Convert doi to bibtex
#'
#' Convert a digital object identifier (doi) string into a bibtex entry using the
#' webservice \link{http://www.doi2bib.org}.
#'
#' @param doi The character string of the doi.
#' @param citekey Character string of equal length to \code{doi}. If used will replace citekeys with these values.
#'
#' @return a bibtex entry as a character string.
#'
#' @importFrom httr accept content GET
#'
#' @examples
#' doi2bib("10.1038/35012251")
#'
#' @export

setGeneric(
  "doi2bib",
  function(doi, citekey) {
    standardGeneric("doi2bib")
  }
)

setMethod(
  "doi2bib",
  c(doi = "character"),
  function(doi, citekey) {

    if (!missing(citekey)) {
      stopifnot(length(citekey) == length(doi))
    }

    refs <-
      vapply(
        doi,
        function(doi) {
          content(
            GET(
              url    = "http://www.doi2bib.org/",
              config = accept("application/x-bibtex"),
              path   = "doi2bib",
              query  = list(id = doi)
            ),
            as = "text",
            encoding = "UTF-8"
          )
        },
        character(length(doi))
      )

    # replace the citekeys
    if (!missing(citekey)) {
      mapply(
        function(refs, citekey) {
         sub("([^\\{]+\\{)[^,]+", paste0("\\1", citekey), refs)
        },
        refs,
        citekey
      )
    }

    refs

  }
)

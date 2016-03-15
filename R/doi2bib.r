#' Convert doi to bibtex
#'
#' Convert a digital object identifier (doi) string into a bibtex entry using
#' the webservice \url{http://www.doi2bib.org}.
#'
#' @param doi The character string of the doi.
#' @param citekey Character string of equal length to \code{doi}. If used will
#'   replace citekeys with these values.
#'
#' @return a bibtex entry as a character string.
#'
#' @importFrom httr accept content GET
#' @importFrom methods setGeneric setMethod
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

doi2bib_ <-
  function(doi) {
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
}

#'@describeIn doi2bib Convert doi to bibtex
setMethod(
  "doi2bib",
  c(doi = "character", citekey = "missing"),
  function(doi, citekey) {
    doi2bib_(doi)
  }
)

#'@describeIn doi2bib Convert doi to bibtex
setMethod(
  "doi2bib",
  c(doi = "character", citekey = "character"),
  function(doi, citekey) {

    stopifnot(length(citekey) == length(doi))

    refs <- doi2bib_(doi)

    # replace the citekeys
    mapply(
      function(refs, citekey) {
        sub("([^\\{]+\\{)[^,]+", paste0("\\1", citekey), refs)
      },
      refs,
      citekey
    )

  }
)

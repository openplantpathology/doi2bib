#' Convert doi to bibtex
#'
#' Convert a digital object identifier (doi) string into a bibtex entry using
#' the webservice \url{http://www.doi2bib.org}.
#'
#' @param ... One or more character strings of the doi. If arguments are named, names will replace citekeys
#' @param file an optional character string. If used the bibtex references are sent to file rather than returned.
#' @param quiet logical. Should the bibtex references be printed to the console?
#'
#' @return a list, returned invisibly, of bibtex references as a character strings, unless a file is specified, in which case nothing is returned
#'
#' @importFrom httr accept content GET
#' @importFrom methods setGeneric setMethod signature
#'
#' @examples
#' doi2bib(Margules2000 = "10.1038/35012251")
#'
#' @export

setGeneric(
  "doi2bib",
  function(..., file, quiet = FALSE) {
    standardGeneric("doi2bib")
  },
  signature = signature("...")
)

#'@describeIn doi2bib Convert doi to bibtex
setMethod(
  "doi2bib",
  "character",
  function(..., file, quiet) {

    stopifnot(missing(file) || is.character(file))

    stopifnot(is.logical(quiet))

    dois <- c(...)

    nms <- names(dois)

    refs <-
      lapply(
        dois,
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
        }
      )

    if (!is.null(nms)) {
      refs <-
        mapply(
          function(ref, nm) {
            if (nchar(nm) < 1) {
              ref
            } else {
              sub("([^\\{]+\\{)[^,]+", paste0("\\1", nm), ref)
            }
          },
          refs,
          nms,
          SIMPLIFY = FALSE
        )
    }

    if (!quiet) message(paste(refs, collapse = "\n"))

    if (!missing(file)) {
      cat(refs, file = file)
    } else {
      invisible(refs)
    }
  }
)

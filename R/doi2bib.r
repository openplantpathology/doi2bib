#' Convert DOI to Bibtex
#'
#' Convert a digital object identifier (DOI) string into a bibtex entry using
#' the webservice \url{http://www.doi2bib.org}.
#'
#' @param ... One or more DOIs, as \code{character} strings. If arguments are
#'   named, names will replace default citekeys.
#' @param file an optional \code{character} string. If used, the bibtex
#'   references are sent to \code{file} rather than being returned.
#' @param quiet \code{logical}. By default, bibtex references are printed to the
#'   console. By setting \code{quiet} to \code{TRUE}, this behaviour will be
#'   prevented.
#'
#' @return a \code{list}, returned invisibly, of bibtex references as
#'   \code{character} strings, unless \code{file} is specified, in which case
#'   the function returns \code{NULL} invisibly.
#'
#' @importFrom httr accept content GET
#' @importFrom methods setGeneric setMethod signature
#'
#' @examples
#' doi2bib(Margules2000 = "10.1038/35012251")
#' doi2bib(Margules2000 = "10.1038/35012251",
#'         Myers2000 = "10.1038/35002501")
#' @export

setGeneric(
  "doi2bib",
  function(..., file, quiet = FALSE) {
    standardGeneric("doi2bib")
  },
  signature = signature("...")
)

replace_citekeys <-
  function(refs, nms) {
    setNames(
      mapply(
        function(ref, nm) {
          ifelse(
            nchar(nm) < 1,
            ref,
            sub("([^\\{]+\\{)[^,]+", paste0("\\1", nm), ref)
          )
        },
        refs,
        nms,
        SIMPLIFY = FALSE
      ),
    nms
    )
  }

refs_to_file <-
  function(refs, file) {
    cat(paste(refs, collapse = "\n"), file = file, append = TRUE)
  }

#'@describeIn doi2bib Convert DOI to bibtex
setMethod(
  "doi2bib",
  "character",
  function(..., file, quiet) {

    stopifnot(missing(file) || is.character(file))
    stopifnot(is.logical(quiet))
    stopifnot(has_connection())

    dois <- c(...)

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

    nms <- names(dois)

    if (!is.null(nms)) {
      refs <- replace_citekeys(refs, nms)
    }

    if (!quiet) message(paste(refs, collapse = "\n"))

    if (!missing(file)) {
      refs_to_file(refs, file)
    } else {
      invisible(refs)
    }
  }
)

# skip test if cant connect to doi2bibs
has_connection <-
  function() {
    !httr::http_error("http://www.doi2bib.org/")
  }


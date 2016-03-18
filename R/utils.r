# skip test if cant connect to doi2bibs
check_connection <-
  function() {
    if (httr::http_error("http://www.doi2bib.org/")) {
      testthat::skip("www.doi2bib.org not available")
    }
  }

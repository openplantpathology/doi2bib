context("doi2bib")

test_that(
  "doi2bib returns a list of the correct length",
  {
    doi2bib:::check_connection()

    no_ref <- doi2bib("")
    invalid_ref <- doi2bib("This is not a DOI")
    ref <- doi2bib("10.1038/35012251")
    refs <- doi2bib("10.1038/35012251", "10.1111/j.1600-0587.2011.07085.x")
    named_dois <- list(
      "name 1" = "10.1038/35012251",
      "name 2" = "10.1111/j.1600-0587.2011.07085.x"
    )
    named_refs <- do.call(doi2bib, named_dois)

    expect_equivalent(class(no_ref), "list")
    expect_equivalent(length(no_ref), 1L)

    expect_equivalent(class(invalid_ref), "list")
    expect_equivalent(length(invalid_ref), 1L)

    expect_equivalent(class(ref), "list")
    expect_equivalent(length(ref), 1L)

    expect_equivalent(class(refs), "list")
    expect_equivalent(length(refs), 2L)

    expect_equivalent(class(named_refs), "list")
    expect_equivalent(length(named_refs), 2L)
  }
)

test_that(
  "replace_citekeys is replacing the citekeys",
  {
    doi2bib:::check_connection()

    refs <- doi2bib("10.1038/35012251", "10.1111/j.1600-0587.2011.07085.x")
    nms <- c("name 1", "name 2")
    refs <- doi2bib:::replace_citekeys(refs, nms)
    expect_equivalent(names(refs), nms)
  }
)

test_that(
  "refs_to_file is writing to a file",
  {
    doi2bib:::check_connection()

    refs <- doi2bib("10.1038/35012251", "10.1111/j.1600-0587.2011.07085.x")

    doi2bib:::refs_to_file(refs, "refs.bib")

    expect_true(file.exists("refs.bib"))
    expect_more_than(file.size("refs.bib"), 0)

    unlink("refs.bib")
  }
)

context("doi2bib")

test_that(
  "doi2bib returns a list",
  {
    ref <- doi2bib("10.1038/35012251")
    expect_equivalent(class(ref), "list")
  }
)

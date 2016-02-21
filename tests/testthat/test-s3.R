
context("Package with S3 functions")

test_that("Rd parsing", {

  map <- map_rd("pkg2")
  expect_equal(
    map,
    list(
      assert = list(print.graph = c(graph = "is.graph(.)")),
      coerce = list()
    )
  )
})

test_that("Package with S3", {

  test_package <- function() {
    on.exit(unloadNamespace("pkg2"))

    expect_error(
      pkg2::print.graph(NULL),
      "is.graph.graph. is not TRUE"
    )
  }

  install_tmp_pkg("pkg2", test_package())
})

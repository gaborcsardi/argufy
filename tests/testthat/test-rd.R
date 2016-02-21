
context("Rd parsing")

test_that("map_rd", {
  map <- map_rd("pkg1")

  expect_equal(names(map), c("assert", "coerce"))
  expect_equal(sort(names(map$assert)), c("plusmat", "prefix", "suffix"))
  expect_equal(sort(names(map$coerce)), "prefix2")

  expect_equal(
    map$assert$plusmat,
    c(A = "is.matrix(.) && identical(dim(A), dim(B))",
      B = "is.matrix(.) && identical(dim(A), dim(B))")
  )
  expect_equal(
    map$assert$prefix,
    c(str = "is.character", len = "is.integer")
  )
  expect_equal(
    map$assert$suffix,
    c(str = "is.character", len = "is.integer")
  )

  expect_equal(
    map$coerce$prefix2,
    c(str = "as.character(str)", len = "as.integer(len)")
  )
})

test_that("map_rd1", {
  map <- map_rd1("ex1.Rd", map = list(), get_rd_macros(NULL))
  expect_equal(
    map,
    list(assert = c(vertex_ids = "is_simplegraph(graph)"))
  )
})

test_that("map_rd warnings", {

  expect_warning(map_rd(tempfile()), "No manual pages")

  tmp <- tempfile()
  dir.create(tmp)
  expect_warning(map_rd(tmp), "No manual pages")

  cat("foo", file = file.path(tmp, "man"))
  expect_warning(map_rd(tmp), "is not a directory")
})

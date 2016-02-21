
context("Argufy package directory")

test_that("argufy_pkgdir", {

  pkg1 <- new.env()
  source("pkg1/R/pkg.R", local = pkg1)

  argufy_pkgdir("pkg1", "pkg1", pkg1)

  expect_error(pkg1$prefix(10, 1), "is.character.str. is not TRUE")
  expect_error(pkg1$prefix("foobar", NULL), "is.integer.len. is not TRUE")

  expect_error(pkg1$suffix(10, 1), "is.character.str. is not TRUE")
  expect_error(pkg1$suffix("foobar", NULL), "is.integer.len. is not TRUE")

  expect_error(
    pkg1$prefix2(base::ls, 1),
    "cannot coerce type 'closure' to vector of type 'character'"
  )
  expect_error(
    pkg1$prefix2("foobar", base::ls),
    "cannot coerce type 'closure' to vector of type 'integer'"
  )

  expect_error(
    pkg1$plusmat("x", 1),
    "is.matrix.A. && identical.dim.A., dim.B.. is not TRUE"
  )
})

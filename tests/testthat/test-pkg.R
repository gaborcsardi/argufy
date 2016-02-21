
context("Argufying during install")

test_that("argufying during install", {

  test_package <- function() {
    on.exit(unloadNamespace("pkg1"))

    expect_error(pkg1::prefix(10, 1), "is.character.str. is not TRUE")
    expect_error(pkg1::prefix("foobar", NULL), "is.integer.len. is not TRUE")

    expect_error(pkg1::suffix(10, 1), "is.character.str. is not TRUE")
    expect_error(pkg1::suffix("foobar", NULL), "is.integer.len. is not TRUE")

    expect_error(
      pkg1::prefix2(base::ls, 1),
      "cannot coerce type 'closure' to vector of type 'character'"
    )
    expect_error(
      pkg1::prefix2("foobar", base::ls),
      "cannot coerce type 'closure' to vector of type 'integer'"
    )

    expect_error(
      pkg1::plusmat("x", 1),
      "is.matrix.A. && identical.dim.A., dim.B.. is not TRUE"
    )
  }

  install_tmp_pkg("pkg1", test_package())
})

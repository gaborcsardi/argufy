context("coercions")

test_that("as_enum works", {

  f <- argufy(function( x = c("alpha", "beta", "gamma") ?~ as_enum) {
    x
  })

  expect_equal(f("alpha"), "alpha")
  expect_equal(f("beta"), "beta")
  expect_equal(f("gamma"), "gamma")

  expect_equal(f("a"), "alpha")
  expect_equal(f("b"), "beta")
  expect_equal(f("g"), "gamma")  

  expect_error(f("delta"), "should be one of")
})


context("argufy")


test_that("functions without checks", {

  f <- function(x, y = 10) x + 10
  expect_identical(argufy(f), f)

  f <- function() { }
  expect_identical(argufy(f), f)

  f <- function(x = 10 + 10, y = 20 && FALSE) { x * y }
  expect_identical(argufy(f), f)

})


test_that("functions with simple assertions", {

  f <- argufy(function(
    x =       ? is.integer,
    y =       ? is.character
  ) {
    NULL
  })

  expect_null(f(10L, "foo"))
  expect_null(f(x = 10L, "foo"))
  expect_null(f(x = 10L, y = "foo"))
  expect_null(f(10L, y = "foo"))

  expect_error(f(10, "foo"), "is.integer")
  expect_error(f(10L, 1), "is.character")
  expect_error(f(10, 1), "is.integer")

  f <- argufy(function(
    x =       ? is.integer,
    y = 42L   ? is.integer
  ) {
    y
  })

  expect_identical(f(42L), 42L)
  expect_identical(f(42L, 42L * 42L), 42L * 42L)

  expect_error(f(42), "is.integer")
  expect_error(f(42L, 42), "is.integer")

})


test_that("functions with complex assertions", {

  f <- argufy(function(
    x =                ? is.numeric(x) && length(x) == 1 && is.finite(x),
    z = c(42L, 42L)    ? is.numeric(z) && length(z) == 2
  ){
    c(x, z)
  })

  expect_identical(f(1), c(1, 42, 42))
  expect_identical(f(1, c(5, 5)), c(1, 5, 5))

  expect_error(f(Inf), "is.finite")
  expect_error(f(1:2), "length")
  expect_error(f(1, 1), "length")

})


test_that("functions with simple coercions", {

  f <- argufy(function(
    x = ? ~ as.character,
    y = ?   is.integer
  ) {
    x
  })

  expect_identical(f("foo", 10L), "foo")
  expect_identical(f(4242, 1:5), "4242")

  expect_error(f(function(){}, 1L), "cannot coerce type")

})


test_that("functions with complex coercions", {

  f <- argufy(function(
    x = ?   is.numeric,
    y = ? ~ if (x > 0) as.character(y) else y
  ) {
    y
  })

  expect_identical(f(10, 100), "100")
  expect_identical(f(-10, 100), 100)
  expect_identical(f(-10, f), f)

  expect_error(f(10, function(){}), "cannot coerce type")
})

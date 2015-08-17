context("question")

test_that("it calls utils::`?` if in top level", {
  with_mock(`base::sys.nframe` = function(...) 1,
            `argufy:::getElsewhere` = function(x, y) function(x) substitute(x),

            expect_equal(?test, as.symbol("test")))
})

test_that("it returns the argument if missing", {

  f1 <- function(x = ? is.character) {
    if (missing(x)) {
      "missing"
    } else {
      x
    }
  }

  expect_equal(f1(), "missing")

  expect_equal(f1(1), 1)

  expect_equal(argufy(f1)(), "missing")

  expect_equal(argufy(f1)("a"), "a")

  expect_error(argufy(f1)(1))
})

test_that("it returns the argument if default", {
  f1 <- function(x = "a" ? is.character) {
    x
  }

  expect_equal(f1(), "a")

  expect_equal(f1(1), 1)

  expect_equal(argufy(f1)(), "a")

  expect_equal(argufy(f1)("a"), "a")

  expect_error(argufy(f1)(1))
})

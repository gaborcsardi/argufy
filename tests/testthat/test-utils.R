
context("Utilities")

test_that("argufy_me", {
  expect_equal(argufy_me(), "OK, boss.")
})

test_that("find_parent", {

  f <- function() {
    foo <- "bar"
    g()
  }

  g <- function() {
    parent <- find_parent(quote(f))
    expect_equal(
      get("foo", envir = sys.frame(parent)),
      "bar"
    )
  }
})

test_that("find_all_parents", {

  f <- function(recurse = FALSE) {
    foo <- "bar"
    if (recurse) f() else g()
  }

  g <- function() {
    parents <- find_all_parents(quote(f))
    expect_equal(length(parents), 2)
    expect_equal(
      get("foo", envir = sys.frame(parents[1])),
      "bar"
    )
    expect_equal(
      get("foo", envir = sys.frame(parents[2])),
      "bar"
    )
  }
})

test_that("parse_deps", {

  test_cases <- list(
    list("foo, bar, foobar", c("foo", "bar", "foobar")),
    list("foo,\n  bar,\n  foobar", c("foo", "bar", "foobar")),
    list("foo", "foo"),
    list("foo (>= 1.0)", "foo"),
    list("foo (>= 1.0), bar", c("foo", "bar")),
    list("foo, bar (>=0.2-3)", c("foo", "bar")),
    list("", character(0)),
    list("\n\n", character(0))
  )

  lapply(test_cases, function(t) {
    expect_equal(parse_deps(t[[1]]), t[[2]], info = t[[1]])
  })
})

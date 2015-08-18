library(disposables)

context("argufy_package")
test_that("argufy_package works with simple functions", {
  pkg <- make_packages(
    imports = "argufy",
    simple = {
      a <- function(x = ? is.numeric) {
        if (x) {
          1
        } else {
          2
        }
      }

      argufy::argufy_package()
    })

  expect_equal(a(0), 2)

  expect_equal(a(1), 1)

  expect_error(a("a"), "is.numeric\\(x\\) is not TRUE")

  dispose_packages(pkg)
})

test_that("argufy_package works with S4 generics and methods", {

  pkg <- make_packages(
    imports = "argufy",
    TestS4 = {

      setGeneric("paste2", function(x = ? is.character, y) {
        standardGeneric("paste2")
      })

      setMethod("paste2",
        signature(x = "character", y = "missing"),
        function(x) {
          paste(x)
        })

      setMethod("paste2",
        c(x = "character", y = "ANY"),
        function(x, y = ? is.character) {
          paste(x, y)
        })

      argufy::argufy_package()
    })

  expect_equal(paste2("a"), "a")

  expect_equal(paste2("a", "b"), "a b")

  # argufy set on generic
  expect_error(paste2(1), "is.character\\(x\\) is not TRUE")

  # argufy set on method
  expect_error(paste2("a", 1), "is.character\\(y\\) is not TRUE")

  dispose_packages(pkg)
})

test_that("argufy_package works with environments", {
  env <- new.env(parent = new.env(parent = .GlobalEnv))
  env$fun <- function(x = ? is.numeric) {
    x
  }
  setGeneric("paste2", where = env, function(x = ? is.character, y) {
    standardGeneric("paste2")
  })

  setMethod("paste2", where = env,
    signature(x = "ANY", y = "missing"),
    function(x, y) {
      paste(x)
    })

  setMethod("paste2", where = env,
    c(x = "ANY", y = "ANY"),
    function(x, y = ? is.character) {
      paste(x, y)
    })
  argufy_package(env)

  expect_equal(env$fun(1), 1)

  expect_error(env$fun("a"), "is.numeric\\(x\\) is not TRUE")

  expect_equal(env$paste2("a"), "a")

  expect_equal(env$paste2("a", "b"), "a b")

  # argufy set on generic
  expect_error(env$paste2(1), "is.character\\(x\\) is not TRUE")

  # argufy set on method, calling it directly because S4 method dispatch
  # doesn't seem to work within an environment
  fun <- env$`.__T__paste2:.GlobalEnv`$`ANY#ANY`

  # workaround for r-oldrel discrepancy
  if (!is.null(fun)) {
    expect_error(fun("a", 1),
      "is.character\\(y\\) is not TRUE")
  }
})

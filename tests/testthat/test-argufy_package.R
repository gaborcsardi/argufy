{

  library(disposables)

  pkgs <- make_packages(
    TestS4 = {

    a <- function(x = ~ is.numeric) {
      if (x) {
        1
      } else {
        2
      }
    }

    setGeneric("paste2", function(x = ~ is.character, y) {
       standardGeneric("paste2")
      })

    setMethod("paste2",
      signature(x = "character", y = "missing"),
      function(x) {
        paste(x)
      })

    setMethod("paste2",
      c(x = "character", y = "ANY"),
      function(x, y = ~ is.character) {
        paste(x, y)
      })

    argufy:::argufy_package("TestS4")
    })

  context("argufy_package")
  test_that("argufy_package works with simple functions", {

    expect_equal(a(0), 2)

    expect_equal(a(1), 1)

    expect_error(a("a"), "is.numeric\\(x\\) is not TRUE")
  })

  test_that("argufy_package works with S4 generics and methods", {

    expect_equal(paste2("a"), "a")

    expect_equal(paste2("a", "b"), "a b")

    # argufy set on generic
    expect_error(paste2(1), "is.character\\(x\\) is not TRUE")

    # argufy set on method
    expect_error(paste2("a", 1), "is.character\\(y\\) is not TRUE")
  })

  dispose_packages(pkgs)
}

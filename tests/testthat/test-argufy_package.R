{
  temp_lib <- file.path(tempdir(), "lib")

  old <- .libPaths()
  .libPaths(temp_lib)
  on.exit(.libPaths(old))
  tools:::.install_packages("TestS4")

  context("argufy_package")
  test_that("argufy_package works with simple functions", {

    expect_equal(TestS4::a(0), 2)

    expect_equal(TestS4::a(1), 1)

    expect_error(TestS4::a("a"), "is.numeric\\(x\\) is not TRUE")
  })

  test_that("argufy_package works with S4 generics and methods", {

    expect_equal(TestS4::paste2("a"), "a")

    expect_equal(TestS4::paste2("a", "b"), "a b")

    # argufy set on generic
    expect_error(TestS4::paste2(1), "is.character\\(x\\) is not TRUE")

    # argufy set on method
    expect_error(TestS4::paste2("a", 1), "is.character\\(y\\) is not TRUE")
  })

}

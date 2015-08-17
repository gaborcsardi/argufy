context("getElsewhere")

test_that("it returns first result of utils::getAnywhere if filter doesn't match", {
  expect_equal(getElsewhere("demo", ""), utils::demo)

  expect_error(getElsewhere("demo", "utils"))
})

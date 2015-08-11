#' an example function
#'
#' @export
a <- function(x = ~ is.numeric) {
  if (x) {
    1
  } else {
    2
  }
}

#' @export
TestS4 <- setClass("TestS4",
  slots = list(name = "character", enabled = "logical"))

#' @export
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

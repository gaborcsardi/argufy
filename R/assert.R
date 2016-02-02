
#' argufy generic assertion call
#'
#' You most probably do not need to call this
#' function directly.
#'
#' @param ... Assertion.
#'
#' @export

assert <- function(...) {

  args <- as.list(match.call(expand.dots = FALSE)$...)

  if (length(args) == 1 && is.call(args[[1]])) {
    call <- args[[1]]
    fun <- get(as.character(call[[1]]))
    pkg <- fun_pkg(fun)

    ## assertthat
    if (is.function(attr(fun, "fail"))) {
      return(do.call(assertthat::assert_that, args))

    ## checkmate
    } else if (pkg == "checkmate") {
      return(do.call(checkmate::assert, args))

    } else if (grepl("^assertive", pkg)) {
      return(eval(call))
    }
  }

  sincall <- as.call(c(list(quote(stopifnot)), args))
  eval(sincall, envir = parent.frame())
}

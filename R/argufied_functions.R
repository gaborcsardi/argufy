
#' List argufied functions in a package or environment
#'
#' @param package R package name. It will be loaded.
#' @param envir Environment to list. Defaults to the environment
#'   of \code{package}. This means that internal and exported functions
#'   will be listed.
#' @return Data frame with columns: \code{func}, \code{assertions}.
#'   The latter is a logical column.
#'
#' @export

argufied_functions <- function(package, envir = asNamespace(package)) {

  funcs <- Filter(is.function, as.list(envir))
  funcs <- funcs[sort(names(funcs))]

  data.frame(
    stringsAsFactors = FALSE,
    row.names = seq_along(funcs),
    func = names(funcs),
    assertions = vapply(funcs, is_argufied, TRUE)
  )
}

is_argufied <- function(fun) {

  body <- body(fun)

  length(body) >= 2 &&
    length(body[[2]]) >= 2 &&
    is_argufy_header(body[[2]][[2]])
}

is_argufy_header <- function(x) {
  identical(x, argufy_header)
}

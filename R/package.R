
#' @title Add argument checks to a function
#'
#' @description
#' Function argument assertions via a concise declerative syntax.
#' The actual assertion code is generated automatically and inserted
#' at the beginning into the function.
#'
#' @details
#' Assertions are separated from the argument names by the
#' tilde (`~`) character. See examples below. Note that the equation
#' signs must be present in front of the tilde, even if the argument
#' does not have a default value.
#'
#' Assertions come in three forms:
#' 1. If the assertion is a function whose name starts with `is.` or
#'    `is_`, then this function must return true for the argument's value.
#' 1. If the assertion is a function whose name starts with `as.` or
#'    `as_`, then this is used as a coercion function for the argument.
#' 1. Otherwise the assertion must be a complete expression that
#'    evaluated to `TRUE`. You can refer to the argument and to other
#'    arguments within the assertion.
#'
#' @param fun The function to add the argument checks to.
#'   The argument checks are specified in the original argument list
#'   of `fun`. See details below.
#' @return Another function that is equivalent to `fun`, but implements
#'   the declared argument checks.
#'
#' @export
#' @examples
#' prefix <- argufy(function(
#'  str =     ~ as.character,
#'  len = 3   ~ is.numeric(len) && length(len) == 1 && is.finite(len)
#' ) {
#'   substring(x, 1, y)
#' })
#'
#' prefix

argufy <- function(fun) {
  if (!is.function(fun)) stop("'fun' must be a function")

  ## Parse the checks
  checks <- parse_checks(formals(fun))

  ## Remove checks from the arguments
  formals(fun) <- remove_checks(formals(fun))

  ## Add the checks to the body of the function
  fun <- add_checks(fun, checks)

  fun
}


parse_checks <- function(args) {
  mapply(names(args), args, SIMPLIFY = FALSE, FUN = function(name, arg) {
    list(
      name = name,
      args = get_arg(arg),
      check = get_check(arg)
    )
  })
}


has_check <- function(arg) {
  class(arg) == "call" && identical(arg[[1]], quote(`~`))
}


get_arg <- function(arg) {
  if (has_check(arg)) {
    if (length(arg) == 2) { quote(expr = ) } else { arg[[2]] }
  } else {
    arg
  }
}


get_check <- function(arg) {
  if (has_check(arg)) {
    if (length(arg) == 2) { arg[[2]] } else { arg[[3]] }
  } else {
    NULL
  }
}


remove_checks <- function(args) {
  lapply(args, get_arg)
}


is_check_function <- function(check) {
  pre <- substring(as.character(check$check), 1, 3)
  is.name(check$check) && (pre == "is." || pre == "is_")
}


is_coercion_function <- function(check) {
  pre <- substring(as.character(check$check), 1, 3)
  is.name(check$check) && (pre == "as." || pre == "as_")
}


get_check_expr <- function(check) {
  if (is_check_function(check)) {
    substitute(
      stopifnot(`_check_`(`_name_`)),
      list(`_check_` = check$check, `_name_` = as.name(check$name))
    )
  } else if (is_coercion_function(check)) {
    substitute(
      `_name_` <- `_coerce_`(`_name_`),
      list(`_coerce_` = check$check, `_name_` = as.name(check$name))
    )
  } else if (is.null(check$check)) {
    NULL
  } else {
    substitute(
      stopifnot(`_expr_`),
      list(`_expr_` = check$check)
    )
  }
}


drop_nulls <- function(l) {
  l [ ! vapply(l, is.null, TRUE) ]
}


create_check_expr <- function(checks) {
  exprs <- drop_nulls(lapply(checks, get_check_expr))
  as.call(c(list(quote(`{`)), exprs))
}


add_checks <- function(fun, checks) {

  check_expr <- create_check_expr(checks)

  new_body <- substitute(
    { `_check_`; `_body_` },
    list("_check_" = check_expr, "_body_" = body(fun))
  )
  body(fun) <- new_body

  fun
}

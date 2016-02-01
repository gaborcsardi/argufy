
#' @title Add argument checks to a function
#'
#' @description
#' Function argument assertions via a concise declerative syntax.
#' The actual assertion code is generated automatically and inserted
#' at the beginning into the function.
#'

argufy <- function(fun, ...) {
  if (!is.function(fun)) stop("'fun' must be a function")

  # these statements are needed to get S4 functions to work properly
  was_s4 <- isS4(fun)
  old_attributes <- attributes(fun)

  fmls <- formals(fun)

  # modify any formals specified in dots
  fmls <- modify_formals(fmls, ...)

  ## Parse the checks
  checks <- parse_checks(fmls)

  ## Remove checks from the arguments
  formals(fun) <- remove_checks(fmls)

  ## Add the checks to the body of the function
  fun <- add_checks(fun, checks)

  # S4 functions have additional attributes which need to be set, regular
  # functions do not have attributes so nothing is done.
  if (was_s4) {
    fun <- asS4(fun)
  }
  attributes(fun) <- old_attributes

  fun
}

modify_formals <- function(fmls, ...) {
  new_formals <- eval(substitute(alist(...)))
  changed <- match(names(new_formals), names(fmls))

  if (any(is.na(changed))) {
    stop("argument ", sQuote(names(new_formals)[is.na(changed)][1]),
         " does not exist in the argument list", call. = FALSE)
  }

  if (length(changed)) {
    fmls[changed] <- new_formals
  }

  fmls
}

parse_checks <- function(args) {
  mapply(names(args), args, SIMPLIFY = FALSE, FUN = function(name, arg) {
    list(
      name = name,
      args = get_arg(arg),
      coercion = is_coercion(arg),
      check = get_check(arg)
    )
  })
}


has_check <- function(arg) {
  class(arg) == "call" && identical(arg[[1]], quote(`?`))
}


get_arg <- function(arg) {
  if (has_check(arg)) {
    if (length(arg) == 2) { quote(expr = ) } else { arg[[2]] }
  } else {
    arg
  }
}


is_coercion <- function(arg) {
  has_check(arg) &&
    class(arg[[length(arg)]]) == "call" &&
    identical(arg[[length(arg)]][[1]], quote(`~`))
}


get_check <- function(arg) {
  if (has_check(arg)) {
    len <- length(arg)
    if (is_coercion(arg)) {
      arg[[length(arg)]][[2]]
    } else {
      arg[[length(arg)]]
    }
  } else {
    NULL
  }
}


remove_checks <- function(args) {
  lapply(args, get_arg)
}


create_assertion_call <- function(check) {
  if (is.name(check$check)) {
    substitute(
      if (!missing(`_name_`)) stopifnot(`_check_`(`_name_`)),
      list(`_check_` = check$check, `_name_` = as.name(check$name))
    )

  } else {
    substitute(
      if (!missing(`_name_`)) stopifnot(`_expr_`),
      list(`_expr_` = check$check, `_name_` = as.name(check$name))
    )
  }
}


create_coercion_call <- function(check) {
  if (is.name(check$check)) {
    substitute(
      if (!missing(`_name_`)) `_name_` <- `_coerce_`(`_name_`),
      list(`_coerce_` = check$check, `_name_` = as.name(check$name))
    )

  } else {
    substitute(
      if (!missing(`_name_`)) `_name_` <- `_expr_`,
      list(`_name_` = as.name(check$name), `_expr_` = check$check)
    )
  }
}


get_check_expr <- function(check) {
  if (is.null(check$check)) {
    NULL

  } else if (check$coercion) {
    create_coercion_call(check)

  } else {
    create_assertion_call(check)
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

  if (length(check_expr) <= 1) {
    return(fun)
  }

  new_body <- substitute(
    { `_check_`; `_body_` },
    list("_check_" = check_expr, "_body_" = body(fun))
  )
  body(fun) <- new_body

  fun
}

#' Replacement for ? function
#'
#' This function behaves identically to the existing ? function when used at
#' the top level, however when used within a function it is an identity function.
#' @param e1 First argument to pass along to `utils::"?"`.
#' @param e2 Second argument to pass along to `utils::"?"`.
#' @usage
#' # ?e2
#' # e1?e2
#' @export
# argument names are e1 and e2 to match utils::`?`
`?` <- function(e1, e2) {

  # top level calls have a sys.nframe() of 1
  if (sys.nframe() <= 1) {
    call <- sys.call()
    call[[1]] <- getElsewhere("?", c("argufy"))
    eval(call)
  } else {
    e1
  }
}

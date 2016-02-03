
#' Function to import to run argufy on your package
#'
#' This function does nothing, and only exists in itself.
#' It can be imported into a package, and then all functions of
#' that package will be transformed by argufy.
#'
#' @export

argufy_me <- function() {
  "OK, boss."
}

#' Add argument checks to a function
#'
#' Function argument assertions via a concise declerative syntax.
#' The actual assertion code is generated automatically and inserted
#' at the beginning into the function.
#'
#' @param fun Function to add assertions to.
#' @param assertions Character scalar, assertion expression.
#' @param coercions Character scalar, coercion expression.
#' @return Function with assertions added.
#'
#' @keywords internal

argufy <- function(fun, assertions = NULL, coercions = NULL) {
  if (!is.function(fun)) stop("'fun' must be a function")

  # these statements are needed to get S4 functions to work properly
  was_s4 <- isS4(fun)
  old_attributes <- attributes(fun)

  ## Add the checks to the body of the function
  fun <- add_checks(fun, assertions, coercions)

  # S4 functions have additional attributes which need to be set, regular
  # functions do not have attributes so nothing is done.
  if (was_s4) fun <- asS4(fun)

  attributes(fun) <- old_attributes

  fun
}

create_assertion_call <- function(check) {
  if (is.name(check$check)) {
    substitute(
      argufy::assert(`_check_`(`_name_`)),
      list(`_check_` = check$check, `_name_` = as.name(check$name))
    )

  } else {
    substitute(
      argufy::assert(`_expr_`),
      list(`_expr_` = check$check, `_name_` = as.name(check$name))
    )
  }
}


create_coercion_call <- function(check) {
  if (is.name(check$check)) {
    substitute(
      `_name_` <- `_coerce_`(`_name_`),
      list(`_coerce_` = check$check, `_name_` = as.name(check$name))
    )

  } else {
    substitute(
      `_name_` <- `_expr_`,
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


add_checks <- function(fun, assertions, coercions) {

  assertions <- lapply(names(assertions), function(x) {
    list(
      check = subs_dot(parse(text = assertions[[x]])[[1]], x),
      name = x,
      coercion = FALSE
    )
  })

  coercions <- lapply(names(coercions), function(x) {
    list(
      check = subs_dot(parse(text = coercions[[x]])[[1]], x),
      name = x,
      coercion = TRUE
    )
  })

  checks <- c(assertions, coercions)
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

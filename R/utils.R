
find_parent <- function(name) {
  calls <- sys.calls()
  for (i in seq_along(calls)) {
    if (identical(calls[[i]][[1]], name)) return(i)
  }
  NA_integer_
}

find_all_parents <- function(name) {
  calls <- sys.calls()
  Filter(
    function(i) identical(calls[[i]][[1]], name),
    seq_along(calls)
  )
}


parse_deps <- function(deps) {
  deps <- str_trim(strsplit(deps, ",")[[1]])
  deps <- lapply(strsplit(deps, "\\("), str_trim)
  deps <- lapply(deps, sub, pattern = "\\)$", replacement = "")
  vapply(deps, "[", "", 1)
}


str_trim <- function(x) {
  sub("^\\s+", "", sub("\\s+$", "", x))
}

subs_dot <- function (expr, name) {
  call <- substitute(
    substitute(expr, list(. = as.name(name))),
    list(expr = expr)
  )
  eval(call)
}

fun_pkg <- function(fun) {
  name <- environmentName(environment(fun))
  if (is.character(name) && length(name) == 1) name else ""
}

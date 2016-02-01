getElsewhere <- function(x, y) {
  found <- utils::getAnywhere(x)

  # attached packages from `search()` are prefixed with package:
  # Namespaces from `loadedNamespaces()` are not, so we filter both
  filtered <- !(found$where %in% paste0(c("", "package:", "namespace:"), y))

  if (!any(filtered)) {
    stop(sQuote(x), " not found in un-filtered locations", call. = FALSE)
  }
  found$objs[filtered][[1]]
}


find_parent <- function(name) {
  calls <- sys.calls()
  for (i in seq_along(calls)) {
    if (identical(calls[[i]][[1]], name)) return(i)
  }
  NA_integer_
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

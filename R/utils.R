getElsewhere <- function(x, y) {
  found <- utils::getAnywhere(x)

  # attached packages from `search()` are prefixed with package:
  # Namespaces from `loadedNamespaces()` are not, so we filter both
  filtered <- !(found$where %in% c(y, paste0("package:", y)))

  if (!any(filtered)) {
    stop(sQuote(x), " not found in un-filtered locations", call. = FALSE)
  }
  found$objs[filtered][[1]]
}

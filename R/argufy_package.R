#' argufy all of the functions within a package
#'
#' This function is best placed either in the last file sourced (\code{zzz.R}
#' or the last file listed in the collate directive) or in \code{.onLoad()}.
#' @inheritParams argufy
#' @param package the package to argufy
#' @export
argufy_package <- function(env = parent.frame(), ...) {
  if (is.character(env)) {
    env <- asNamespace(env)
  }

  argufy_environment(env, ...)

  argufy_S4(env, ...)

  invisible()
}

argufy_environment <- function(ns, ...) {
  nms <- ls(ns, all.names = TRUE)

  funs <- mget(nms, ns, mode = "function", ifnotfound = NA)

  funs <- funs[!is.na(funs)]

  Map (function(nme, fun) {
    fun <- argufy(fun, ...)
    assign(nme, fun, envir = ns)
  }, names(funs), funs)

  invisible()
}

argufy_S4 <- function(ns, ...) {
  generics <- getGenerics(ns)

  Map(generics@.Data, generics@package, USE.NAMES = FALSE,
      f = function(name, package) {
        what <- methodsPackageMetaName("T", paste(name, package, sep = ":"))

        table <- get(what, envir = ns)

        argufy_environment(table, ...)
      })
}

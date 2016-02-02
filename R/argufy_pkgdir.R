
#' Argufy a package that is being installed
#'
#' We load all help files from the package, and map the arguments
#' that contain assertions or coercions, to functions. (A single argument
#' might map to multiple functions.)
#'
#' When we are done with all help files, we go over the functions
#' and inject the checks into their code.
#'
#' @param pkg Package name.
#' @param pkg_dir Path to source package directory.
#' @param env The environment containing the objects in the package.
#'   The objects are updated in this environment.
#' @return Nothing.

argufy_pkgdir <- function(pkg, pkg_dir, env) {

  amap <- map_rd(pkg_dir)
  assert <- amap[["assert"]]
  coerce <- amap[["coerce"]]

  funcs <- ls(env, all.names = TRUE)
  lapply(funcs, function(f) {
    if (f %in% names(assert) || f %in% names(coerce)) {
      argufy_in(env, f, assert[[f]], coerce[[f]])
    }
  })

  invisible()
}

argufy_in <- function(env, fname, assertions, coercions) {
  fun <- get(fname, envir = env)
  if (!is.function(fun)) return()
  fun <- argufy(fun, assertions, coercions)
  assign(fname, fun, envir = env)
  invisible()
}

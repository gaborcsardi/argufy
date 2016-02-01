
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

  funcs <- ls(env, all.names = TRUE)
  lapply(funcs, function(f) {
    if (f %in% names(amap)) argufy_in(env, f, amap[[f]])
  })

  invisible()
}

argufy_in <- function(env, fname, checks) {
  fun <- get(fname, envir = env)
  if (!is.function(fun)) return()
  fun <- argufy(fun, checks)
  assign(fname, fun, envir = env)
  invisible()
}

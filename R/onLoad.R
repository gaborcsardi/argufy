
## We put in a trace at the beginning of makeLazyLoadDB
## Note that this function is called (at least) twice during
## package installation, first for the code R objects,
## then for the help file R objects. We need to ignore the second one.
##
## A cleaner solution would be to trace loadNamespace on exit,
## but it does not seem to be possible to do that in a robust way,
## because tracing on exit just calls on.exit(), and that will possibly
## interfere with the regular on.exit() calls within the function.

tracer_function <- function() {
  ## Check if a package is being installed
  instframeno <- find_parent(quote(do_install_source))
  if (is.na(instframeno)) return()

  ## Check if we are installing the code, as opposed to the help files
  llframeno <- find_parent(quote(makeLazyLoading))
  if (is.na(llframeno)) return()

  ## Check if the package uses argufy at all
  desc <- get("desc", envir = sys.frame(instframeno))
  imps <- parse_deps(desc["Imports"])
  if (! "argufy" %in% imps) return()

  ## Find the functions
  lazyframeno <- find_parent(quote(makeLazyLoadDB))

  fun_env <- get("from", envir = sys.frame(lazyframeno))

  cat("** argufying functions\n")
  argufy_environment(fun_env)
  argufy_S4(env, ...)

  invisible()
}
  
.onLoad <- function(libname, pkgname) {

  trace_call <- as.call(list(
    trace,
    as.call(list(as.symbol(":::"), quote(tools), quote(makeLazyLoadDB))),
    print = FALSE,
    tracer_function
  ))
  suppressMessages(eval(trace_call))
}

.onUnload <- function(path) {
  untrace_call <- as.call(list(
    untrace,
    as.call(list(as.symbol(":::"), quote(tools), quote(makeLazyLoadDB)))
  ))
  suppressMessages(eval(untrace_call))
}

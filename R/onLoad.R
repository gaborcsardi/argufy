
## We put in a trace at the beginning of makeLazyLoadDB
## Note that this function is called (at least) twice during
## package installation, first for the code R objects,
## then for the help file R objects. We need to ignore the second one.
##
## A cleaner solution would be to trace loadNamespace on exit,
## but it does not seem to be possible to do that in a robust way,
## because tracing on exit just calls on.exit(), and that will possibly
## interfere with the regular on.exit() calls within the function.

                                        # nocov start

tracer_function <- function() {
  ## Check if a package is being installed
  instframeno <- find_parent(quote(do_install_source))
  if (is.na(instframeno)) return()

  get_inst <- function(x) get(x, envir = sys.frame(instframeno))

  ## Check if we are installing the code, as opposed to the help files
  llframeno <- find_parent(quote(makeLazyLoading))
  if (is.na(llframeno)) return()

  ## Check if the package uses argufy at all
  desc <- get_inst("desc")
  if (! "Imports" %in% names(desc)) return()
  imps <- parse_deps(desc["Imports"])
  if (! "argufy" %in% imps) return()

  ## Find the functions
  lazyframeno <- find_parent(quote(makeLazyLoadDB))

  fun_env <- get("from", envir = sys.frame(lazyframeno))

  cat("** argufying functions\n")
  pkg <- get_inst("pkg_name")
  pkg_dir <- get_inst("pkg_dir")
  argufy_pkgdir(pkg, pkg_dir, fun_env)

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

  help_trace_call <- as.call(list(
    trace,
    as.call(list(as.symbol("::"), quote(tools), quote(loadPkgRdMacros))),
    print = FALSE,
    help_tracer_function
  ))
  suppressMessages(eval(help_trace_call))

  devtools_trace_call <- as.call(list(
    trace,
    as.call(list(as.symbol(":::"), quote(devtools), quote(run_ns_load_actions))),
    print = FALSE,
    devtools_tracer_function
  ))
  if ("devtools" %in% loadedNamespaces()) {
    suppressMessages(eval(devtools_trace_call))
  }
}

.onUnload <- function(path) {
  untrace_call <- as.call(list(
    untrace,
    as.call(list(as.symbol(":::"), quote(tools), quote(makeLazyLoadDB)))
  ))
  suppressMessages(eval(untrace_call))

  help_untrace_call <- as.call(list(
    untrace,
    as.call(list(as.symbol("::"), quote(tools), quote(loadPkgRdMacros)))
  ))
  suppressMessages(eval(help_untrace_call))
}

help_tracer_function <- function() {
  ## Check if a package is being installed
  instframeno <- find_parent(quote(do_install_source))
  if (is.na(instframeno)) return()

  get_inst <- function(x) get(x, envir = sys.frame(instframeno))

  ## If this is a recursive call, then bail out
  if (length(find_all_parents(quote(loadPkgRdMacros))) > 1) return()

  ## Check if the package uses argufy at all
  desc <- get_inst("desc")
  if (! "Imports" %in% names(desc)) return()
  imps <- parse_deps(desc["Imports"])
  if (! "argufy" %in% imps) return()

  parentno <- find_parent(quote(loadPkgRdMacros))
  get_parent <- function(x) get(x, envir = sys.frame(parentno))
  set_parent <- function(x, v) assign(x, v, envir = sys.frame(parentno))

  macros <- get_parent("macros")
  macros <- loadPkgRdMacros(system.file(package = "argufy"), macros)
  set_parent("macros", macros)
}

devtools_tracer_function <- function() {
  frameno <- find_parent(quote(run_ns_load_actions))
  pkg <- get("pkg", envir = sys.frame(frameno))
  ns <- getExportedValue("devtools", "ns_env")(pkg)
  argufy_pkgdir(pkg$name, pkg$path, ns)
}

                                        # nocov end


map_rd <- function(pkgdir) {

  map <- list()

  path <- file.path(pkgdir, "man")
  if (!file.exists(path)) {
    warning("No manual pages")
    return(map)
  }

  if (!file.info(path)$isdir) {
    warning("'man' is not a directory")
    return(map)
  }

  macros <- get_rd_macros(pkgdir)

  rdfiles <- list.files(path, pattern = "\\.Rd$")
  for (rd in rdfiles) map <- map_rd1(file.path(path, rd), map, macros)

  map
}


#' @importFrom tools parse_Rd loadRdMacros loadPkgRdMacros

get_rd_macros <- function(pkgdir) {

  ## System level extra macros
  macros <- loadRdMacros(
    file.path(R.home("share"), "Rd", "macros", "system.Rd")
  )

  ## Macros from the package being installed
  if (!is.null(pkgdir)) macros <- loadPkgRdMacros(pkgdir, macros)

  ## Argufy macros, two possibilities, because in devtools it is different
  argufy_macros1 <-
    system.file(package = "argufy", "help", "macros", "Rdmacros.Rd")
  if (argufy_macros1 != "") macros <- loadRdMacros(argufy_macros1, macros)

  argufy_macros2 <-
    system.file(package = "argufy", "man", "macros", "Rdmacros.Rd")
  if (argufy_macros2 != "") macros <- loadRdMacros(argufy_macros2, macros)

  macros
}


map_rd1 <- function(rdfile, map, macros) {

  Rd <- parse_Rd(
    rdfile,
    permissive = TRUE,
    macros = macros
  )

  usage <- rd_find(Rd, "\\usage")
  args <- rd_find(Rd, "\\arguments")
  items <- rd_find_all(args, "\\item")

  argmap <- map_arg_to_func(items, usage)

  for (arg in names(argmap)) {
    check <- argmap[[arg]]$check
    for (fun in argmap[[arg]]$funcs) {
      map[[fun]] <- c(map[[fun]], structure(check, names = arg))
    }
  }

  map
}


#' Returns NULL if not found

rd_find <- function(rd, tag) {
  for (e in rd) {
    if (attr(e, "Rd_tag") == tag) return(e)
  }
}

rd_find_all <- function(rd, tag) {
  Filter(function(e) attr(e, "Rd_tag") == tag, rd)
}

## Find arguments that have assertions.
## If we find one, also find the corresponding functions
## in usage.

map_arg_to_func <- function(items, usage) {

  checked_items <- Filter(assert_macro, items)

  map <- list()

  for (x in checked_items) {
    name <- as.vector(x[[1]][[1]])
    check <- x[[2]][[1]][2]
    map[[name]] <- list(check = check, funcs = character())
  }

  if (length(map) == 0) return(map)

  for (u in usage) {
    usage_args <- get_args(u)
    for (ua in usage_args$args) {
      if (ua %in% names(map)) {
        map[[ua]]$funcs <- c(map[[ua]]$funcs, usage_args$name)
      }
    }
  }

  map
}

assert_macro <- function(x) {
  attr(x[[2]][[1]], "Rd_tag") == "USERMACRO" &&
    grepl("% assert\n$", x[[2]][[1]][1])
}

get_args <- function(x) {

  fun <- tryCatch(
    as.list(parse(text = x)[[1]]),
    error = function(e) list("foo")
  )

  args <- lapply(
    seq_along(fun)[-1],
    function(i) {
      if (is.null(names(fun)) || names(fun)[i] == "") {
        as.character(fun[[i]])
      } else {
        names(fun)[i]
      }
    }
  )

  list(name = as.character(fun[[1]]), args = args)
}

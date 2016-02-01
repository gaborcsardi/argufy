
map_rd <- function(pkgdir) {

  map <- list()

  path <- file.path(pkgdir, "man")
  if (!file.exists(path)) {
    warning("No manual pages")
    return(map)
  }

  if (!file.info(path)$isdir) {
    warning("'man' is not a directory")
    reutrn(map)
  }

  macros <- loadRdMacros(
    file.path(R.home("share"), "Rd", "macros", "system.Rd")
  )
  macros <- loadPkgRdMacros(pkgdir, macros)
  macros <- loadRdMacros(
    system.file(package = "argufy", "Rdmacros.Rd"),
    macros
  )

  rdfiles <- list.files(path, pattern = "\\.Rd$")
  for (rd in rdfiles) map <- map_rd1(file.path(path, rd), map, macros)

  map
}


#' @importFrom tools parse_Rd loadRdMacros loadPkgRdMacros

map_rd1 <- function(rdfile, map, macros) {

  Rd <- parse_Rd(
    rdfile,
    permissive = TRUE,
    macros = macros
  )

  usage <- rd_find(Rd, "\\usage")
  args <- rd_find(Rd, "\\arguments")
  items <- rd_find_all(args, "\\item")

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

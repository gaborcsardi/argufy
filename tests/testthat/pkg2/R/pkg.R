
#' @importFrom argufy argufy_me
NULL

#' Print a graph
#' @param graph \assert{is.graph(.)} The input graph.
#' @export

foobar.graph <- function(graph) {
  cat("coocoo!\n")
}

#' @export

is.graph <- function(graph) {
  FALSE
}

#' @export

foobar <- function(graph)
  UseMethod("foobar")

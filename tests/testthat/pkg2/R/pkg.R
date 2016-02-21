
#' @importFrom argufy argufy_me
NULL

#' Print a graph
#' @param graph \assert{is.graph(.)} The input graph.
#' @export

print.graph <- function(graph) {
  cat("coocoo!\n")
}

is.graph <- function(graph) {
  inherits(graph, "graph")
}

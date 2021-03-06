% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/basics.R
\name{vertex_ids}
\alias{vertex_ids.graph}
\alias{print.graph}
\alias{foobar}
\title{Vertex ids of a graph}
\usage{
## S3 method
\method{vertex_ids}{graph}(graph)
\method{print}{graph}(graph, foo)
foobar()
}
\arguments{
  \item{graph}{\assert{is_simplegraph(graph)} The graph.}
  \item{foo}{Just foo.}
}
\value{
Character vector of vertex ids.
}
\description{
Vertex ids of a graph
}
\examples{
G <- graph(list(A = c("B", "C"), B = "C", C = "A"))
vertex_ids(G)
}

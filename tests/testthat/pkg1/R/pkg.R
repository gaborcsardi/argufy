
#' @importFrom argufy argufy_me
NULL

#' Prefix of a string
#'
#' @param str \assert{is.character} Character vector.
#' @param len \assert{is.integer} Integer vector.
#' @return Prefix is string, of prescribed length.
#' @export

prefix <- function(str, len) {
  substring(str, 1, len)
}

#' Suffix of a string
#'
#' @rdname prefix
#' @export

suffix <- function(str, len) {
  substring(str, nchar(str) - len + 1, nchar(str))
}



#' Return the prefixes of specified number of characters
#'
#' @param str \coerce{as.character(str)} Character vector.
#' @param len \coerce{as.integer(len)} Integer vector.
#' @return Prefix is string, of prescribed length.
#' @export

prefix2 <- function(str, len) {
  substring(str, 1, len)
}



#' Sum of two matrices
#'
#' @param A \assert{is.matrix(.) && identical(dim(A), dim(B))}
#'   The first matrix.
#' @param B \assert{is.matrix(.) && identical(dim(A), dim(B))}
#'   The second matrix.
#' @return Their sum.
#' @export

plusmat <- function(A, B) A + B


#' A coercion function for choice arguments in argufy
#'
#' Check if a string scalar argument is one of the pre-specified
#' choices, with partial matching.
#'
#' @param arg The argument to check and match.
#' @return The matched value.
#'
#' @export
#' @seealso `::argufy`, `base::match.arg`

as_enum <- function(arg) {
  formal.args <- formals(sys.function(sys.parent()))
  choices <- eval(formal.args[[deparse(substitute(arg))]])
  match.arg(arg, choices = choices)
}

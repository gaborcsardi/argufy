
```{r, setup, echo = FALSE, message = FALSE}
knitr::opts_chunk$set(
  comment = "#>",
  tidy = FALSE,
  error = FALSE,
  fig.width = 8,
  fig.height = 8)
```

<h1 align="center">
    <br>
    <br>
    <img width="400" src="./inst/logo.png" alt="argufy">
    <br>
    <br>
    <br>
</h1>

> Declarative Arguments Checks

[![Linux Build Status](https://travis-ci.org/gaborcsardi/argufy.svg?branch=master)](https://travis-ci.org/gaborcsardi/argufy)
[![Windows Build status](https://ci.appveyor.com/api/projects/status/github/gaborcsardi/argufy?svg=true)](https://ci.appveyor.com/project/gaborcsardi/argufy)
[![](http://www.r-pkg.org/badges/version/argufy)](http://www.r-pkg.org/pkg/argufy)
[![CRAN RStudio mirror downloads](http://cranlogs.r-pkg.org/badges/argufy)](http://www.r-pkg.org/pkg/argufy)
[![Coverage Status](https://img.shields.io/codecov/c/github/gaborcsardi/argufy/master.svg)](https://codecov.io/github/gaborcsardi/argufy?branch=master)


Declare your functions with argument checks, and `argufy()` will generate
and add the checking code for you.

## Installation

```{r eval = FALSE}
devtools::install_github("gaborcsardi/argufy")
```

## Usage

### Introduction

To use `argufy` in your R package, you need to import the `argufy_me`
function. (You don't actually have to call the function, just import it.)

Once you imported `argufy_me`, you can add assertions and coercions to
your Roxygen headers, and these will be picked up automatically
at installation time.

If you use Roxygen, importing `argufy_me` requires two qiuck and easy
steps:
* Include `argufy` in the `Imports` entry in the `DESCRIPTION` file:

  ```
  ...
  Suggests:
      testthat
  Imports:
      argufy,
      utils
  ...
  ```
* Put the following in any of your `.R` source files:
  
  ```r
  #' @importFrom argufy argufy_me
  NULL
  ```

### Assertions

State your assertions in the `@param` tags of your Roxygen
documentation, inside `\assert{}` Rd tags. An assertion can
be an R expression. It must evaluate to `TRUE`, each time the
function is called, and the argument is supplied.

The assertions are parsed by Roxygen, and put in the documentation.
They are also parsed when your package is installed, and they are added
to the body of the function(s) they refer to, automatically.

When you load the package with `library` amd call the function,
the added assertions are checked and an error is given if they fail.

Let's see an example.

```r
#' Prefix of a string
#'
#' @param str \assert{is.character(str)} Character vector.
#' @param len \assert{is.integer(len)} Integer vector.
#' @return Prefix is string, of prescribed length.

prefix <- function(str, len) {
  substring(str, 1, len)
}
```

If you call Roxygen to create the Rd documentation for this function,
and then install the package, `argufy` generates code from your assertions
and injects the code into the body of the function:

```r
prefix
#> function (str, len)
#> {
#>     {
#>         if (!missing(str))
#>             stopifnot(is.character(str))
#>         if (!missing(len))
#>             stopifnot(is.integer(len))
#>     }
#>     {
#>         substring(str, 1, len)
#>     }
#> }
```

The assertions are also included in the generated manual pages:
```
Arguments:
     str: [‘is.character(str)’] Character vector.
     len: [‘is.integer(len)’] Integer vector.
```

### Coercions

Quite often, coercing the argument to the desired type is a better
solution than a simple assertion, because it makes your function
extensible. E.g. if your function takes a data frame argument, then
instead of checking that the supplied object is indeed a data frame,
you can try to coerce it to a data frame. This way, your function will
work for any object that is coercible to a data frame (i.e. has an
`as.data.frame()` method).

`argufy` has a `\coerce{}` tag to declare coercions. It works very
similarly to the `\assert{}` tag, but the generated code is different:

```r
#' Prefix of a string
#'
#' @param str \coerce{as.character(str)} Character vector.
#' @param len \coerce{as.integer(len)} Integer vector.
#' @return Prefix is string, of prescribed length.

prefix2 <- function(str, len) {
  substring(str, 1, len)
}
```

And the generated code:

```r
prefix2
#> function (str, len)
#> {
#>     {
#>         if (!missing(str))
#>             str <- as.character(str)
#>         if (!missing(len))
#>             len <- as.integer(str)
#>     }
#>     {
#>         substring(str, 1, len)
#>     }
#> }
```

The coercion expression must fail by calling `stop()` if it is not
possible to coerce the supplied value in a meaningful way.

You can of course mix assertions and coercions for the same function.

### More concise assertions with `.`

`argufy` helps writing short and concise assertions. If your assertion
is a single function call on the supplied argument, you can simply
use the name of the function instead. For example:

```r
#' Prefix of a string
#'
#' @param str \assert{is.character} Character vector.
#' @param len \assert{is.integer} Integer vector.
#' @return Prefix is string, of prescribed length.

prefix <- function(str, len) {
  substring(str, 1, len)
}
```

If your assertion is more complex, then you can use a dot: `.` instead
of the argument name:

```r
#' Prefix of a string
#'
#' @param str \assert{is.character} Character vector.
#' @param len \assert{is.integer(.) && length(.) == 1} Integer vector.
#' @return Prefix is string, of prescribed length.

prefix <- function(str, len) {
  substring(str, 1, len)
}
```

### Assertions involving multiple arguments

Assertions can refer to multiple arguments by name. An assertion
can refer to any other argument, the order of the arguments
does not matter at all:

```r
#' Sum of two matrices
#'
#' @param A \assert{is.matrix(.) && identical(dim(A), dim(B))}
#'   The first matrix.
#' @param B \assert{is.matrix(.) && identical(dim(A), dim(B))}
#'   The second matrix.
#' @return Their sum.

plusmat <- function(A, B) A + B
```

The generated code:

```r
plusmat
#> function (A, B)
#> {
#>     {
#>         if (!missing(A))
#>             stopifnot(is.matrix(A) && identical(dim(A), dim(B)))
#>         if (!missing(B))
#>             stopifnot(is.matrix(B) && identical(dim(A), dim(B)))
#>     }
#>     A + B
#> }
```

### Reuse assertions for multiple functions

If you declare an assertion for an argument, it will be used for
all functions that share that argument, and are documented on that same Rd
manual page. For example:

```r
#' Prefix of a string
#'
#' @param str \assert{is.character} Character vector.
#' @param len \assert{is.integer} Integer vector.
#' @return Prefix is string, of prescribed length.

prefix <- function(str, len) {
  substring(str, 1, len)
}

#' Suffix of a string
#'
#' @rdname prefix

suffix <- function(str, len) {
  substring(str, nchar(str) - len + 1, nchar(str))
```

The generated code:

```r
suffix
#> function (str, len)
#> {
#>     {
#>         if (!missing(str))
#>             stopifnot(is.character(str))
#>         if (!missing(len))
#>             stopifnot(is.integer(len))
#>     }
#>     {
#>         substring(str, nchar(str) - len + 1, nchar(str))
#>     }
#> }
```

### Without Roxygen

You can also use `argufy` without Roxygen. Simply put your assertions and
coercions in the Rd manual pages, using the `\assert` and `\coerce` macros.
They are automatically added to the functions at install time.

## License

MIT © [Gábor Csárdi](https://github.com/gaborcsardi),
[Jim Hester](http://www.jimhester.com/).

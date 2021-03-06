<!-- -*- mode: markdown -*- -->



# argufy

> Declarative Argument Checks

[![Project Status: WIP - Initial development is in progress, but there has not yet been a stable, usable release suitable for the public.](http://www.repostatus.org/badges/latest/wip.svg)](http://www.repostatus.org/#wip)
[![Linux Build Status](https://travis-ci.org/gaborcsardi/argufy.svg?branch=master)](https://travis-ci.org/gaborcsardi/argufy)
[![Windows Build status](https://ci.appveyor.com/api/projects/status/github/gaborcsardi/argufy?svg=true)](https://ci.appveyor.com/project/gaborcsardi/argufy)
[![](http://www.r-pkg.org/badges/version/argufy)](http://www.r-pkg.org/pkg/argufy)
[![CRAN RStudio mirror downloads](http://cranlogs.r-pkg.org/badges/argufy)](http://www.r-pkg.org/pkg/argufy)
[![Coverage Status](https://img.shields.io/codecov/c/github/gaborcsardi/argufy/master.svg)](https://codecov.io/github/gaborcsardi/argufy?branch=master)


Declare your functions with argument checks, and `argufy` generates
and inserts the checking code for you.

---

  - [Installation](#installation)
  - [Usage](#usage)
    - [Introduction](#introduction)
    - [Assertions](#assertions)
	- [Internal functions](#internal-functions)
    - [Coercions](#coercions)
    - [More concise assertions with `.`](#more-concise-assertions-with-)
    - [Assertions involving multiple arguments](#assertions-involving-multiple-arguments)
	- [Reuse assertions for multiple functions](#reuse-assertions-for-multiple-functions)
	- [Without Roxygen](#without-roxygen)
  - [Frequently asked questions](#frequently-asked-questions)
  - [License](#license)

## Installation


```r
devtools::install_github("gaborcsardi/argufy")
```

## Usage

### Introduction

To use `argufy` in your R package, you need to import and call the
`argufy_me` function. Once you called `argufy_me`, you can add
assertions and coercions to your Roxygen headers, and these will be picked
up automatically at installation time. You also need to add `argufy`
to the `RdMacros` entry of the package.

In other words, importing `argufy_me` requires two qiuck and easy
steps:
* Include `argufy` in the `Imports` entry of the `DESCRIPTION` file:

  ```
  ...
  Imports:
      argufy
  RdMacros:
	  argufy
  ...
  ```
* Import the `argufy_me` function in a Roxygen header, and call the function.
  I.e. put the following in any of your `.R` source files:

  ```r
  #' @importFrom argufy argufy_me
  NULL
  argufy::argufy_me()
  ```

### Assertions

State your assertions in the `@param` tags of your Roxygen
documentation, inside `\assert{}` Rd tags. An assertion is
an R expression. It must evaluate to `TRUE`, each time the
function is called, and the argument is supplied, otherwise
the function quits with an error.

The assertions are parsed by Roxygen, and put in the manual page
of the function as well. They are also parsed when your package is
installed, and they are inserted to the body of the function(s) they refer
to, automatically.

When the user of your package loads your package via `library()`
or imports it into another package, the assertions are checked
and an error is given if any of them fail.

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


```
#> function (str, len) 
#> {
#>     {
#>         stopifnot(is.character(str))
#>         stopifnot(is.integer(len))
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

### Internal functions

It is possible to add assertions to internal functions, with a little bit
of extra work. If an internal function has assertions defined in `@param`
Roxygen tags, it is also required to add the `@keywords internal` tag, and
a manual page title. Roxygen will generate a manual page for the function
in this case, but will not include it in the index of manual pages:


```r
#' This is an internal function that merges two named lists, elementwise,
#' @param x \assert{is_named_list} First list.
#' @param y \assert(is_named_list} Second list.
#' @keywords internal

merge_lists <- function(x, y) {
  names <- unique(sort(c(names(x), names(y))))
  structure(
    lapply(names, function(n) { c(x[[n]], y[[n]]) }),
    names = names
  )
}
```

The generated code:


```
#> function (x, y) 
#> {
#>     {
#>         stopifnot(is_named_list(x))
#>         stopifnot(is_named_list(y))
#>     }
#>     {
#>         names <- unique(sort(c(names(x), names(y))))
#>         structure(lapply(names, function(n) {
#>             c(x[[n]], y[[n]])
#>         }), names = names)
#>     }
#> }
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


```
#> function (str, len) 
#> {
#>     {
#>         str <- as.character(str)
#>         len <- as.integer(len)
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


```
#> function (A, B) 
#> {
#>     {
#>         stopifnot(is.matrix(A) && identical(dim(A), dim(B)))
#>         stopifnot(is.matrix(B) && identical(dim(A), dim(B)))
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
}
```

The generated code for suffix:


```
#> function (str, len) 
#> {
#>     {
#>         stopifnot(is.character(str))
#>         stopifnot(is.character(len))
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

## Frequently asked questions

### I get an `R CMD check` `NOTE`

Right now `R CMD check` gives the following note about the `RdMacros`
field in `DESCRIPTION`:

```
Unknown, possibly mis-spelled, field in DESCRIPTION:
  ‘RdMacros’
```

This is a bug in `R CMD check`. `RdMacros` is a valid entry, it is
documented in [Writing R extensions](https://cran.r-project.org/doc/manuals/r-devel/R-exts.html#The-DESCRIPTION-file).

### I get warnings when building my package

```r
Warning: /tmp/Rtmp4dc3Q5/Rbuild5b3c7afe189/mypackage/man/myfun.Rd:10:
  unknown macro '\assert'
```

This is probably because you did not add the `RdMacros: argufy` entry
to `DESCRIPTION`.

### Assertions do not show up when using `devtools`

This is because the `srcref` attribute of the function is not modified,
so when printing it to the screen, the original, unpatched code is shown.
Use `print(func, useSource = FALSE)` to see the real, patched source code.

### How can I know that `argufy` worked during installation

Various ways:
* You'll see a message during installation:

   ```
   ** preparing package for lazy loading
   ** argufying functions
   ** help
   ```
* Check the source code of the functions in the installed packages.
* Check the manual pages of the installed package.
* Try calling the functions with assertions and see if you get
  the expected error message(s).
* You can also write test cases for it.

## License

MIT © [Gábor Csárdi](https://github.com/gaborcsardi),
[Jim Hester](http://www.jimhester.com/).

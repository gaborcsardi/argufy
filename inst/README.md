


<h1 align="center">
    <br>
    <br>
    <img width="400" src="./inst/logo.png" alt="crayon">
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


```r
devtools::install_github("gaborcsardi/argufy")
```

## Usage

### Introduction


```r
library(argufy)
```

Assertions about function arguments are to be added among the arguments,
after a `?` or a `?~` operator.

`?` assertions are checks that are run against the argument's value, and
errors are signalled for `FALSE` return values. The `?~` operator denotes
coercions, see more about them later.

The simplest assertion consists of a function name only. This is the
function to be called with the argument's value.


```r
#' Return the prefixes of specified number of characters
prefix <- function(
    str =     ? is.character,
    len =     ? is.numeric) {
  substring(x, 1, y)
}
```

Note that `argufy()` requires the equal signs after the argument names,
even if they have no default values. Just leave out the default values.

Also note that `prefix` is unusable as it is now. First you need to run
`argufy()` on it, to interpret the declarative assertions, and add them
to the code of the function. If you call `prefix` now, you might get
error messages, because R interprets the assertions as default arguments.


```r
prefix <- argufy(prefix)
body(prefix)
```

```
#> {
#>     {
#>         stopifnot(is.character(str))
#>         stopifnot(is.numeric(len))
#>     }
#>     {
#>         substring(x, 1, y)
#>     }
#> }
```

We used `body(prefix)` to print the code of the argufied `prefix` function.
If you just type in the name of the function, then the original code is
printed and you cannot actually see the checks. (This is because the
`srcref` attribute of the original function is used for printing.)

It is suggested that you call `argufy()` as you create the function,
so that you don't have unusable functions lying around:


```r
#' Return the prefixes of specified number of characters
prefix <- argufy(function(
    str =     ? is.character,
    len =     ? is.numeric) {
  substring(x, 1, y)
})

body(prefix)
```

```
#> {
#>     {
#>         stopifnot(is.character(str))
#>         stopifnot(is.numeric(len))
#>     }
#>     {
#>         substring(x, 1, y)
#>     }
#> }
```

Alternatively, if you are developing an R package, you can use
the `argufy_package()` function, to run `argufy()` on all functions
within your package. See more about this later.

### Default values

Default values can be specified after the equation signs, as usual,
and they must also satisfy the assertions. Note that assertions
on default values are only tested at running time, though.


```r
#' Return the prefixes of specified number of characters
prefix <- argufy(function(
    str =     ? is.character,
    len = 3   ? is.numeric) {
  substring(x, 1, y)
})

body(prefix)
```

```
#> {
#>     {
#>         stopifnot(is.character(str))
#>         stopifnot(is.numeric(len))
#>     }
#>     {
#>         substring(x, 1, y)
#>     }
#> }
```

### Simple assertions and coercions

Simple assertions consist of a single function name. These will be called
with the function argument's value, and they must return `TRUE`,
otherwise an error is signalled.

Often we don't want to specify a given type or class for an argument,
only that R must be able to coerce the argument to it. This is
what the `?~` operator does. Simple coercions consist of a single function
name. These will be called with the function argument's value as a coercion.
They must signal an error if the coercion is not possible. Here is an
example for the `prefix` function. This time we only require that `str`
can be converted to character vector:


```r
#' Return the prefixes of specified number of characters
prefix <- argufy(function(
    str =     ?~ as.character,
    len = 3   ?  is.numeric) {
  substring(x, 1, y)
})

body(prefix)
```

```
#> {
#>     {
#>         str <- as.character(str)
#>         stopifnot(is.numeric(len))
#>     }
#>     {
#>         substring(x, 1, y)
#>     }
#> }
```

Note that the check implementation for `str` is now a coercion.

### Generic assertions

Sometimes a single function does not do enough, and a more complex
assertion  or coercion is needed. E.g. we can require the `len` argument
of `prefix` to be a finite numeric scalar, i.e. a vector of length one:


```r
#' Return the prefixes of specified number of characters
prefix <- argufy(function(
    str =     ?~ as.character,
    len = 3   ?  is.numeric(len) && length(len) == 1 && is.finite(len)) {
  substring(x, 1, y)
})

body(prefix)
```

```
#> {
#>     {
#>         str <- as.character(str)
#>         stopifnot(is.numeric(len) && length(len) == 1 && is.finite(len))
#>     }
#>     {
#>         substring(x, 1, y)
#>     }
#> }
```

In this case the assertion must be a complete expression, and it
must return `TRUE` if the assertion holds.

### Using `argufy` in your packages

You can just include your functions in `argufy()` calls. Note that in most
cases you don't need the `argufy` package when you are _using_ your
package. You only need it when you are _building_ your package. So it is a
build time dependency, and it is perfectly fine to put it in
`Suggests` in the `DESCRIPTION` file, you don't need to import anything
from it.

Alternatively, you can call the `argufy_package()` function, to run
`argufy()` on all the functions in the package. For functions that do
not contain any check declarations, this does nothing. You need to make
sure that `argufy_package()` is called after all functions are defined:
you need to put it at the end of the R file that is sourced last,
e.g. you can name this file `zzz.R`. Alternatively you need to include
a `Collate` field in your `DESCRIPTION` file, and put the source file
with `argufy_package()` last.

## License

MIT © [Gábor Csárdi](https://github.com/gaborcsardi).

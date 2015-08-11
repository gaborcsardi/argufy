


# argufy

> Declerative Arguments Checks

[![Linux Build Status](https://travis-ci.org/gaborcsardi/argufy.svg?branch=master)](https://travis-ci.org/gaborcsardi/argufy)
[![Windows Build status](https://ci.appveyor.com/api/projects/status/github/gaborcsardi/argufy?svg=true)](https://ci.appveyor.com/project/gaborcsardi/argufy)
[![](http://www.r-pkg.org/badges/version/argufy)](http://www.r-pkg.org/pkg/argufy)
[![CRAN RStudio mirror downloads](http://cranlogs.r-pkg.org/badges/argufy)](http://www.r-pkg.org/pkg/argufy)


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
separated by a `~` (tilde) character.

The simplest assertions are just calls to functions whose name starts with
`is.` or `is_`. `argufy()` will add calls to these functions and stops the
function with an error message if they return `FALSE`.


```r
#' Return the prefixes of specified number of characters
prefix <- function(
  str =     ~ is.character,
  len =     ~ is.numeric) {
  
  substring(x, 1, y)
}
```

Note that `argufy()` requires the equal signs after the argument names,
even if they have no default values. Just leave out the default values.

Also note that `prefix` is unusable as it is now. First you need to run
`argufy()` on it, to interpret the declerative assertions, and add them
to the code of the function. If you call `prefix` now, you might get
error messages, because R interprets the assertions as default arguments.


```r
prefix <- argufy(prefix)
prefix
```

```
#> function (str, len) 
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

It is suggested that you call `argufy()` as you create the function,
so that you don't have unusable functions lying around:


```r
#' Return the prefixes of specified number of characters
prefix <- argufy(function(
  str =     ~ is.character,
  len =     ~ is.numeric) {
  
  substring(x, 1, y)
})

prefix
```

```
#> function (str, len) 
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

### Default values

Default values can be specified after the equation signs, as usual,
and they must also satisfy the assertions. Note that assertions
on default values are only tested at running time, though.


```r
#' Return the prefixes of specified number of characters
prefix <- argufy(function(
  str =     ~ is.character,
  len = 3   ~ is.numeric) {

  substring(x, 1, y)
})

prefix
```

```
#> function (str, len = 3) 
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

Simple assertions consist of a single function name, starting with the
`is.` or the `is_` prefix. These will be called with the function
argument's value, and they must return `TRUE`, otherwise an error
is signalled.

Often we don't want to specify a given type or class for an argument,
only that R must be able to coerce the argument to it. Simple coercions
consist of a single function name, starting with the `as.` or `as_` prefix.
These will be called with the function argument's value as a coercion.
They must signal an error if the coercion is not possible. Here is an
example for the `prefix` function. This time we only require that `str`
can be converted to character vector:


```r
#' Return the prefixes of specified number of characters
prefix <- argufy(function(
  str =     ~ as.character,
  len = 3   ~ is.numeric) {
 
  substring(x, 1, y)
})

prefix
```

```
#> function (str, len = 3) 
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
assertion is needed. E.g. we can require the `len` argument of `prefix`
to be a finite numeric scalar, i.e. a vector of length one:


```r
#' Return the prefixes of specified number of characters
prefix <- argufy(function(
  str =     ~ as.character,
  len = 3   ~ is.numeric(len) && length(len) == 1 && is.finite(len)) {
  
  substring(x, 1, y)
})

prefix
```

```
#> function (str, len = 3) 
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

## License

MIT © [Gábor Csárdi](https://github.com/gaborcsardi).
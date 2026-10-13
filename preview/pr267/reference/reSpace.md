# Replace any space or spaces with a single new.space character apiece

Replace any space or spaces with a single new.space character apiece

## Usage

``` r
.reSpace(
  x,
  new.space = "_",
  reduce.spaces = FALSE,
  old.space = "[[:punct:]|[:blank:]]"
)
```

## Arguments

- x:

  string\[s\] to be respaced

- new.space:

  character\[s\] with which to replace spaces

- reduce.spaces:

  logical. Reduce multiple consecutive spaces to a single one?

- old.space:

  regular expression defining space; default includes punctuation,
  space, and tab

## Value

The respaced character string

## Examples

``` r
loadflex:::.reSpace("this  \t old *!$?# mandolin", reduce.spaces=TRUE) # returns "this_old_mandolin"
#> [1] "this_old_mandolin"
```

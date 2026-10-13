# Require an argument to match the loadflex conventions for that argument name

This function is modeled on `match.arg`, but it is designed to
facilitate consistency in how argument names are used within the
`loadflex` package.

## Usage

``` r
match.arg.loadflex(arg, choices, several.ok = FALSE)
```

## Arguments

- arg:

  An argument, passed as a symbol. May be a missing argument to the
  calling function.

- choices:

  Optional. A vector of choices, character or logical, that should be
  accepted by partial matching.

- several.ok:

  logical. If TRUE, a vector of several choices in `arg` will be
  accepted and returned as such.

## Value

The argument after matching to the allowed options for that argument
name. This may differ from the original value of `arg` if `arg` is a
truncated version of one of the character options for the argument
(identified using `pmatch`, as in `match.arg`.

## Details

The key differences between this function and `match.arg` are that this
function (1) decides what the available options are without reference to
the defaults defined in the calling function, and (2) permits atomic but
non-character choices.

## Examples

``` r
# match.arg.loadflex knows about some variables by their names alone
flux.or.conc="conc"; flux.or.conc <- loadflex:::match.arg.loadflex(flux.or.conc)
attach.units=TRUE; attach.units <- loadflex:::match.arg.loadflex(attach.units)

# you can also pass in custom choices. as always, partial matching is available
ci.distrib="log"; ci.distrib <- loadflex:::match.arg.loadflex(ci.distrib, c("lognormal","normal"))
```

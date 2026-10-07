# Convert a metadata object to a 1-row data.frame

Organize the fields of a metadata object into a 1-row data.frame. If
there is a custom field, attempt to coerce that field into 1-row
data.frame columns using `as.data.frame`; if that effort fails, the
custom field will be excluded.

## Usage

``` r
# S3 method for class 'metadata'
as.data.frame(
  x,
  row.names = NULL,
  optional = FALSE,
  ...,
  stringsAsFactors = FALSE
)
```

## Arguments

- x:

  a loadflex metadata object

- row.names:

  NULL or a character vector giving the row names for the data frame.
  Missing values are not allowed.

- optional:

  logical. If TRUE, setting row names and converting column names (to
  syntactic names: see make.names) is optional. Note that all of R's
  base package as.data.frame() methods use optional only for column
  names treatment, basically with the meaning of data.frame(\*,
  check.names = !optional).

- ...:

  additional arguments to be passed to or from methods.

- stringsAsFactors:

  logical: should the character vector be converted to a factor?

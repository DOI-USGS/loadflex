# Capitalize first letter of words.

Convert each word in x to have an upper-case first letter and lower-case
subsequent letters. Doesn't make exceptions for articles like "the" or
"a". A word is defined as continuous alpha characters, so "bill's"
becomes "Bill'S" and "u.s.a." becomes "U.S.A".

## Usage

``` r
.sentenceCase(x)
```

## Arguments

- x:

  a character vector to convert

## Value

a character vector with each word having its first letter capitalized
and all others lowercase

## Examples

``` r
loadflex:::.sentenceCase("the QUICK brown Fox jumped oVer the LaZY doG")
#> [1] "The Quick Brown Fox Jumped Over The Lazy Dog"
loadflex:::.sentenceCase(c("QUICK brown Fox","LaZY doG"))
#> [1] "Quick Brown Fox" "Lazy Dog"       
loadflex:::.sentenceCase(c("u.s.a.", "u_s_a", "bill's", "3 bears", "2 be or not 2be"))
#> [1] "U.S.A."          "U_S_A"           "Bill'S"          "3 Bears"        
#> [5] "2 Be Or Not 2be"
```

<!-- badges: start -->
[![R-CMD-check](https://github.com/daisy-model/daisyrVis/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/daisy-model/daisyrVis/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

# daisyrVis 
A suite of visualization tools for Daisy log files written in R.

## Installation
`daisyrVis` is currently only available from github.

Use `pak` to install

    install.packages('pak')
    pak::pkg_install('https://github.com/daisy-model/daisyrVis')

and remove

    pak::pkg_remove('daisyrVis')

Or use `devtools` to install

    install.packages('devtools')
    devtools::install_git('https://github.com/daisy-model/daisyrVis')

and remove

    remove.packages('daisyrVis')

## Examples

    library(daisyrVis)
    example(plot_annual)

Should produce a plot similar to this

![Bar plots of four annualy logged variables from four different Daisy log files](inst/extdata/annual/Annual-FN/plot_annual_example.png)

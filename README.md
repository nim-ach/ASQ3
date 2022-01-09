
<!-- README.md is generated from README.Rmd. Please edit that file -->

# ASQ3

<!-- badges: start -->

[![R-CMD-check](https://github.com/nim-ach/ASQ3/workflows/R-CMD-check/badge.svg)](https://github.com/nim-ach/ASQ3/actions)
[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
<!-- badges: end -->

The purpose of the {ASQ3} package is to provide access to the data
collected by the Centro Asistencial Docente e Investigación de la
Universidad de Magallanes (CADI-UMAG).

The data were treated, eliminating any sensitive information that could
compromise the identity of the participants or linked individuals.

## Installation

You can install the development version of ASQ3 from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("nim-ach/ASQ3")
```

## Example

This is a basic example which shows you how to use the dataset from your
<i class="fab fa-r-project"></i> console.

``` r
library(ASQ3) # Load the package

head(dataset[, 1:5], n = 5) # Print the first 5 rows and columns
#>   profesional_id profesional_especialidad paciente_id evaluacion_numero
#> 1              2               Enfermeria           1                 1
#> 2              2               Enfermeria           2                 1
#> 3              3             Kinesiologia           3                 1
#> 4              3             Kinesiologia           4                 1
#> 5              3             Kinesiologia           5                 1
#>   evaluacion_fecha
#> 1       2021-10-05
#> 2       2021-04-08
#> 3       2020-09-14
#> 4       2020-10-29
#> 5       2020-07-23
```

For more information about the dataset, you can consult the package
documentation.

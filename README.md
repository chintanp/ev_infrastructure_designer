
<!-- README.md is generated from README.Rmd. Please edit that file -->

# EV Infrastructure Designer

**Now golemized**

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
<!-- badges: end -->

EV Infrastructure Designer lets users create custom charging station
deployment scenarios by selecting the locations by clicking on the map,
further configuring each individual charging station and then submitting
the scenario for analysis.

``` r
library(evides)
## basic example code
```

### Things to note about the package:

1.  Uses `golem` to generate a package from R Shiny code.
    (<https://github.com/ThinkR-open/golem>)
2.  Uses modules for effective code management.
    (<https://shiny.rstudio.com/articles/modules.html>)
3.  Validation of user inputs before insertion into database using
    sqlInterpolate to avoid SQL injection attacks.
    (<https://www.rdocumentation.org/packages/DBI/versions/0.5-1/topics/sqlInterpolate>)
4.  Uses `bs4Dash` to get the husky colors and `shinyWidgets`
    actionBttn. (<https://rinterface.github.io/bs4Dash/index.html>,
    <https://github.com/dreamRs/shinyWidgets>)
5.  Auth0 service for authentication.

### Publicly hosted

Shinyapps: <https://cp84.shinyapps.io/evi_des/>

Please note that the ‘evides’ project is released with a [Contributor
Code of Conduct](CODE_OF_CONDUCT.md). By contributing to this project,
you agree to abide by its terms.

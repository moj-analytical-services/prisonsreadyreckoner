
<!--

INSTRUCTIONS:

Render `README.Rmd` regularly, to keep `README.md` up-to-date:

devtools::build_readme()

You could also use GitHub Actions to re-render README.Rmd every time you push.
An example workflow can be found here:
<https://github.com/r-lib/actions/tree/v1/examples>.

You can also embed plots, for example:

#```{r pressure, echo = FALSE}
#plot(pressure)
#```

In that case, commit and push the resulting figure files so they display on
GitHub and CRAN.

-->

# prisonsreadyreckoner

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
<!-- badges: end -->

**A packaged stock-flow model built in R to estimate the cumulative
impact to prison population of pulling various ‘levers’ across the whole
Criminal Justice System related to police charge scenarios, court
sitting days, average time served in prison and recall.**

`prisonsreadyreckoner` provides functions for use by the Shiny app,
`prisons-ready-reckoner-app` and is a dependency of that app. It may
also be used for scenario work as a stand-alone package. Inputs for
`prisonsreadyreckoner` are provided by members of the CJS Modelling Team
or by functions in the accompanying internal MoJ package,
[`prisonsreadyreckonerupdater`](https://github.com/moj-analytical-services/prisonsreadyreckonerupdater).

## Installation

Assuming you are using `renv` for package maanagement, install
`prisonsreadyreckoner` by typing the following in the RStudio console:

``` r
renv::install("git@github.com:moj-analytical-services/prisonsreadyreckoner.git@v1.0.0")
```

where `v1.0.0` is the latest release. You may wish to double-check there
is not a more recent release available on Github.

`prisonsreadyreckoner` relies on the `botor` package, which relies on
the `boto3` Python package. To install `boto3`, run the following:

``` r
# Tell renv that Python will be used. By default this will point to a default
# Python binary file. Your lock file will be updated with details of the
# Python version used.
renv::use_python()
  
# Install the reticulate library to interface with Python, if not already
installed with # the package itself).
renv::install("reticulate")
  
# Install boto3.
reticulate::py_install("boto3")
```

## Running the model

Call the model with

``` r
prisonsreadyreckoner::run_prisonflowcast(params)
```

where `params` is a vector of parameter values conforming with current
[specifications](https://model-redevelopment-website.apps.alpha.mojanalytics.xyz/packages/r/prisonsreadyreckoner/articles/params.html).

## Further reading

For full details, see the accompanying vignette:

``` r
remotes::install_git("git@github.com:moj-analytical-services/prisonsreadyreckoner.git@<packageversion>",
                     force = TRUE,
                     build_vignettes = TRUE)
vignette('mojmodel')
```

where `<packageversion>` is the latest package version (e.g. `v1.0.0`).

Also available at [our
website](https://model-redevelopment-website.apps.alpha.mojanalytics.xyz/).

[Source
code](https://github.com/moj-analytical-services/prisonsreadyreckoner/)
on Github.

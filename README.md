
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

THIS README IS A DRAFT. NONE OF THE TEXT BELOW IS UP TO DATE.

**A packaged stock-flow model built in R to estimate the cumulative
impact to prison population of adding a given number of extra offender
receptions each month.**

## Installation

`prisonsreadyreckoner` relies on various packages, which you must
install before installing `prisonsreadyreckoner`. To install these
packages, type the following commands in the RStudio console:

``` r
renv::install("moj-analytical-services/Rdbtools")

renv::install("git@github.com:moj-analytical-services/fullsample.git")

renv::install("git@github.com:moj-analytical-services/mojmetr.git@v0.2.1")
```

Once these packages are installed, install `prisonsreadyreckoner` by
typing the following in the RStudio console:

``` r
renv::install("git@github.com:moj-analytical-services/prisonsreadyreckoner.git@<packageversion>")
```

where `@<packageversion>` is the latest release. For example, if the
latest release were v0.99.0, you would use `@v0.99.0`.

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

You will need access to the `alpha-prison-forecasting-data` bucket of S3
to be able to run the model.

To call the model, use the `run_prisonflowcast()` function.

library(testthat)
library(prisonsreadyreckoner)
# Need to install prisonsreadyreckonerupdater for all tests to pass.
# prisonsreadyreckonerupdater is not a dependency of the package - but useful
# for development purposes
#library(prisonsreadyreckonerupdater)

test_check("prisonsreadyreckoner")

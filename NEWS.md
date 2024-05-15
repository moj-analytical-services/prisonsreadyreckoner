<!--
INSTRUCTIONS:
Each version should have a level 1 header.

Use bullets to document changes.

Use level 2 headers to section changes, if necessary.

Include Github issue numbers preceded by a hash in parentheses, e.g. (#10).

Include Github pull requests preceded by a hash in parentheses with the author,
e.g. (#2, @bgburton)

-->

# prisonsreadyreckoner 4.0.0

Updated remand and sentencing methodology, added a parameter to the main function and updated dependencies list.

* The remand methodology has been updated to use a linear regression model developed by the Prison Demand team and used in their remand modelling. This change aligns those two models to the same approach.

*The sentencing methodology has been updated to use a table of ratios to convert the number of people sentenced into the number of people sent to prison. This table is supplied by the Prison Demand team and used in their modelling. This replaces the previous approach where the prisons ready reckoner calculated broader ratios from the input data. This change aligns those two models to the same approach.

*An additional parameter called `baseline_only` has been added to the `run_prisonsreadyreckoner()` function so that the user can request a table of results for the baseline and scenario, or just the baseline (if set to `TRUE`). This was added primarily for use by the Rshiny app. If a value for this parameter isn't supplied, the default is `FALSE`.

*The packages used (and their versions) that the prisons ready reckoner is dependant on has been updated.

## Breaking changes
The remand and sentencing changes require different input files and so are not backwards compatible.



# prisonsreadyreckoner 3.0.0

Minor changes to accommodate new inputs in which licence profiles are supplied
as part of the 'recall file'.

## Breaking changes

This version is not backwards compatible because the licence profiles are now
expected to be in the Excel recall file, rather than a separate .csv file.



# prisonsreadyreckoner 2.0.0

Prepared to introduce more sophisticated treatment of time on recall and police
charge scenarios, as requested on 15 June 2023.

* Introduces a lag into the ring-fenced Crown Court disposals. This aligns the
model with the main projection better.

* Introduces a lever to allow the user to change the time spent on recall.

* Expects police charge scenario files in a simplified format to future-proof
against multiple further scenarios. These simplified files are prepared by
prisonsreadyreckonerupdater instead.


## Breaking changes

This version is not backwards compatible with version 1.0.0 because additional
parameters are required to generate the lag in ring-fenced Crown Court disposals
and to implement time on recall impacts and because the scenario files are now
expected to have a simplified format.



# prisonsreadyreckoner 1.0.0

First release for sharing with customers. Adds:

* Simplistic remand calculations accounting for the release of a proportion of
remand prisoners following expiry of custody time limits.

## Breaking changes

This version is not backwards compatible with version 0.0.0 because additional
parameters are required to generate the remand population impacts.



# prisonsreadyreckoner 0.0.0

Initial development version, used for demonstration on 15 June 2023. Includes
functions for use by the Shiny app (although the Shiny app,
prisons-ready-reckoner-app, v0.7.0).

Lacks many of the requirements of a package.

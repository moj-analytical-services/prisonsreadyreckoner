# Script for calling all visualisation scripts and reporting failures.
#
# Guidance on visualisation maintenance
#
# To avoid unduly burdensome maintenance, you are advised to review 
# visualisations after each release and archive those that need no longer be
# maintained.
#
# You may wish to archive a visualisation if
# 1. the visualisation was created to explore data but now these are well
#    understood;
# 2. the visualisation was made to test an aspect of the model, which is now
#    considered stable and is unlikely to be further developed or affeced by
#    other developments in the current release.
#
# Archiving a visualisation
#
# 1. Archive the .Rmd only. The html file contains data so should not be
#    archived on Github.
# 2. Add a note to the .Rmd including
#      a) the last release for which the visualisation was prepared;
#      b) why the visualisation is being archived and why it is safe to do so;
#         and
#      c) any tips on reinstating the visualisation, should it need to be.


prisonsreadyreckoner::silence_botor()


path_visual <- paste("/home", Sys.getenv("USER"), "prisonsreadyreckoner/tests/visuals", sep="/")

files_Rmd <- list.files(path_visual, recursive = TRUE)
files_Rmd <- files_Rmd[!grepl("delete|archive", files_Rmd, ignore.case = TRUE) & grepl(".Rmd$", files_Rmd)]


status <- 0
for (file_Rmd in files_Rmd) {
  
  message("Running ", basename(file_Rmd))
  tryCatch({
      path_Rmd <- paste("/home", Sys.getenv("USER"), "prisonsreadyreckoner/tests/visuals", file_Rmd, sep="/")
      path_html <- sub(".Rmd$", ".html", path_Rmd)

      rmarkdown::render(path_Rmd, output_file = path_html, quiet = TRUE)
      # utils::browseURL(path_html)

    },
    warning = function(msg) {warning("Warning in ", basename(file_Rmd), ": ", msg)
      status <- 1},
    error = function(msg) {warning(basename(file_Rmd), " failed: ", msg)
      status <- 1}
  )

}

if (status)
  warnings()
  
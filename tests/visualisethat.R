# Script for calling all visualisation scripts and reporting failures.

path_visual <- paste("/home", Sys.getenv("USER"), "prisonsreadyreckoner/tests/visuals", sep="/")

files_Rmd <- list.files(path_visual, recursive = TRUE)
files_Rmd <- files_Rmd[!grepl("delete|archive", files_Rmd, ignore.case = TRUE) & grepl(".Rmd$", files_Rmd)]


for (file_Rmd in files_Rmd) {
  
  message("Running ", basename(file_Rmd))
  tryCatch({
      path_Rmd <- paste("/home", Sys.getenv("USER"), "prisonsreadyreckoner/tests/visuals", file_Rmd, sep="/")
      path_html <- sub(".Rmd$", ".html", path_Rmd)

      rmarkdown::render(path_Rmd, output_file = path_html)
      # utils::browseURL(path_html)
    
    },
    warning = function(msg) {warning("Failed: ", msg)},
    error = function(msg) {warning("Failed: ", msg)}
  )

}
  
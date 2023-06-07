.onAttach <- function(libname, pkgname) {
  packageStartupMessage("prisonsreadyreckoner for exploring prison impacts.")
}

.onLoad <- function(libname, pkgname) {

  # Set custom options without overriding options the user may have already set.
  op <- options()
  op_prisonsreadyreckoner <- list(
    prisonsreadyreckoner.author = "MoJ Model Redevelopment"
  )

  idx <- !(names(op_prisonsreadyreckoner) %in% names(op))
  if (any(idx))
    options(op_prisonsreadyreckoner[idx])
  
  
  invisible()
}

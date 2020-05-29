load_inst <- function(rda) {
  env = new.env()
  loaded <- load(system.file(paste0("../inst/extdata/", rda),
                             package = "frasyr"), envir = env)
  invisible(env[[loaded]])
}
return_file_type <- function(fname) {
  if (stringr::str_detect(fname, "csv$")) {
    "csv"
  } else if (stringr::str_detect(fname, "rda$")) {
    "rda"
  } else {
    stop("Unknown file type", fname)
  }
}

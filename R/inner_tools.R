load_inst <- function(rda) {
  env = new.env()
  loaded <- load(system.file(paste0("../inst/extdata/", rda),
                             package = "frasyr"), envir = env)
  invisible(env[[loaded]])
}
}

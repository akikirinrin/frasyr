#' @export
load_vpa <- function(fname, year) {
  ftype <- return_file_type(fname)
  if (ftype == "csv") {
    return(read.vpa(fname))
  }
  if (ftype == "rda") {
    get(load(fname))
  }
}

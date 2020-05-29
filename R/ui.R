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

#' Generates weights for years to use SR relationship
#'
#' @param srdata Object returned by \code{get.SRdata}
#' @param using Years to use either "allyears" or numeric years
#' @param n_yrs_to_remove N of recent years to omit
#'
#' \dontrun{
#' srdata <- get.SRdata(result_vpa)
#' generate_sr_weight(using = "allyear", srdata)
#' generate_sr_weight(using = 1982:1990, srdata)
#' generate_sr_weight(using = 1982:1990, srdata)
#' generate_sr_weight(n_yrs_to_remove = 3, srdata)
#' }
#' @export
generate_sr_weight <- function(srdata, using = NULL, n_yrs_to_remove = NULL) {
  assertthat::assert_that(is.data.frame(srdata))
  assertthat::has_name(srdata, "year")

  out <- rep(0, nrow(srdata))

  check_args <- function() {
    both_null     <- is.null(using) & is.null(n_yrs_to_remove)
    both_non_null <- !is.null(using) & !is.null(n_yrs_to_remove)
    if (both_null | both_non_null) {
      stop("Either `using` or `except` should be given")
    }
  }

  check_args()

  remove_recent_years <- !is.null(n_yrs_to_remove)
  if (remove_recent_years) {
    years_to_use <- srdata$year[1:(length(srdata$year) - n_yrs_to_remove)]
  } else {
    if (all(is.numeric(using))) {
        years_to_use <- using
    } else {
      if (length(using) == 1)  {
        if (using == "allyear") {
          years_to_use <- srdata$year
        } else {
          stop("Unknown format of args", using)
        }
      }
    }
  }
  out[srdata$year %in% years_to_use] <- 1
  force(out)
}

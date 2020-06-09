#' @export
load_data <- function(fname) {
  request_from_developers <- stringr::str_detect(fname, "/inst/extdata")
  if (request_from_developers) {
    fname <- system.file("extdata", extract_filename(fname), package = "frasyr")
  }
  ftype <- return_file_type(fname)
  if (ftype == "csv") {
    return(read.vpa(fname))
  } else if (ftype == "rda") {
    get(load(fname))
  } else {
    stop("Unknown filetype", call. = TRUE)
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


#' @export
set_sr_params <- function(type, method, ar = NULL) {

  check_sr_params(type = type, method = method)
  check_ar_params(method = method, ar = ar)

  list(type   = type,
       method = method,
       ar     = ar)
}

#' Set F current by direct input or referencing VPA result
#'
#' @param manual Numeric vector of F values such as
#' c(0.123, 1.234, 2.345, 3.456)
#' @param f_years Years to calculate mean F values
#' @param s_years Years to calculate selectivity
#' @param vpadata VPA object created by \code{vpa()}
#' @return Numeric vector
#' @examples
#' \dontrun{
#' vpadata <- vpa("some_setting")
#'
#' set_f_current(f_years = 2015:2017,
#'               vpadata = vpadata)
#'
#' set_f_current(f_years = 2015:2017,
#'               s_years = 2010:2016, # Use different years to calculate
#'               vpadata = vpadata)
#'
#' set_f_current(
#'   f_years = -4:-2, # (N - 1) years before the latest (latest: -1)
#'   vpadata = vpadata),
#' set_f_current(f_years = -4:-2,
#'               f_years = -10:-1,
#'               vpadata = vpadata),
#' }
#' @export
set_f_current <- function(manual  = NULL,
                          f_years = NULL,
                          s_years = NULL,
                          vpadata = NULL) {
  if (!is.null(manual)) {
    assertthat::assert_that(is.numeric(manual))
    if (!is.null(f_years) || !is.null(s_years)) {
      stop("'manual' option should be used solely.")
    }
    return(manual)
  }

  if (is.null(vpadata)) {
    stop("Give 'vpadata' to set_f_current().")
  }

  assertthat::assert_that(is.list(vpadata))
  assertthat::has_name(vpadata, "faa")

  if (is.null(s_years)) {
    # Use values in VPA object
    apply_year_colum(vpadata$faa, target_year = f_years)
  } else {
    # Calc from selectivity of different years
    if (is.null(f_years)) {
       stop("Set 'f_years' to calculate F using selectivity")
    }
    convert_faa_perSPR(vpadata,
                       sel_year = s_years,
                       faa_year = f_years)
  }
}

#' Retrieve function argument settings to reuse
#'
#' @param result objects created by make_future_data()
#' @export
retrieve_input <- function(result) {
  assertthat::assert_that(
    assertthat::has_name(result, "input"),
    assertthat::has_name(result$input, "model_average_option")
  )

  force(result$input)
}

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

generate_error <- function(arg, expect) {
  prefix <- paste0("'", arg, "' should be one of:")
  stop(paste(c(prefix, expect), collapse = " "))
}


#' Check args for auto-regression estimation
#'
#' @param type Character one of 'HS', 'RI' or 'BH'
#' @param method Character either 'L1' or 'L2'
check_sr_params <- function(type, method) {
  type_expected   <- c("HS", "BH", "RI")
  method_expected <- c("L1", "L2")

  if (!(type %in% type_expected)) {
    generate_error("type", type_expected)
  }
  if (!(method %in% method_expected)) {
    generate_error("method", method_expected)
  }
}

#' Check args for auto-regression estimation
#'
#' @param ar Character either 'inside' or 'outside'. NULL accepted
#' @inheritParams check_sr_params
check_ar_params <- function(method, ar) {

  if (is.null(ar)) return(NULL)

  assertthat::assert_that(
    is.character(method),
    is.character(ar)
    )

  switch(
    ar,
    "inside" = warning("ar = 'outside' is recommended."),
    "outside" = {
      if (method == "L1") {
        warning("Non-recommended combination: (method = 'L1', ar = 'outside').")
      }
    },
    stop ("'ar' should be either 'inside' or 'outside'.")
  )
}

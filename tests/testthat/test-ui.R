context("User interfaces")

test_that("load_data() works", {
  expect_is(load_data("../../inst/extdata/res_vpa_pma.rda"), "list")
  expect_is(load_data("../../inst/extdata/vpa.csv"), "list")
})


context("- generate_sr_weight()")
srdata    <- get.SRdata(load_data("../../inst/extdata/res_vpa_pma.rda"),
                        return.df = TRUE)
test_that("returns proper length vector", {
  wt_allyr <- generate_sr_weight(using = "allyear", srdata)
  expect_is(wt_allyr, "numeric")
  expect_equal(length(wt_allyr), nrow(srdata))

  wt_specyr <- generate_sr_weight(using = 1982, srdata)
  expect_is(wt_specyr, "numeric")
  expect_equal(length(wt_specyr), nrow(srdata))

  wt_removeyr <- generate_sr_weight(n_yrs_to_remove = 1, srdata)
  expect_is(wt_removeyr, "numeric")
  expect_equal(length(wt_removeyr), nrow(srdata))
})

n_years <- nrow(srdata)
assertthat::assert_that(max(srdata$year) == 2011)
assertthat::assert_that(n_years == 30)

test_that("sets specific years to one", {

  expect_equal(generate_sr_weight(using = 2011, srdata),
               c(rep(0, n_years - 1), 1))
  expect_equal(generate_sr_weight(using = 2010:2011, srdata),
               c(rep(0, n_years - 2), 1, 1))
  expect_equal(generate_sr_weight(using = c(2009, 2011), srdata),
               c(rep(0, n_years - 3), 1, 0, 1))
  expect_equal(generate_sr_weight(using = 1982:1990, srdata),
               c(rep(1, 9), rep(0, 21)))
})

test_that("removes recent years by given number", {

  test_n_years_removed <- function(number_of_years_to_remove) {
    expect_equal(
      generate_sr_weight(n_yrs_to_remove = number_of_years_to_remove,
                    srdata),
      c(rep(1, n_years - number_of_years_to_remove),
        rep(0, number_of_years_to_remove)))
  }

  test_n_years_removed(1)
  test_n_years_removed(2)
  test_n_years_removed(3)
  test_n_years_removed(10)

})

test_that(" returns error for unwanted args", {
  expect_error(generate_sr_weight(srdata),
               "Either `using` or `except` should be given")

  expect_error(generate_sr_weight(using = "allyear",
                             n_yrs_to_remove = 1,
                             srdata),
               "Either `using` or `except` should be given")
})


context("set_sr_params() works")

test_that("set params with default 'AR' args", {

  hsl1 <- set_sr_params(type   = "HS",
                        method = "L1")
  expect_equal(hsl1$type,   "HS")
  expect_equal(hsl1$method, "L1")
  expect_null(hsl1$ar)

  params <- set_sr_params(type   = "HS",
                          method = "L2")
  expect_equal(params$type,   "HS")
  expect_equal(params$method, "L2")
  expect_null(params$ar)
})

test_that("set params correctly", {
  test_srparams <- function(prms, expect) {
    expect_equal(prms$type,   expect[[1]])
    expect_equal(prms$method, expect[[2]])
    expect_equal(prms$ar,     expect[[3]])
  }

  expect_warning(
    test_srparams(set_sr_params(type   = "BH",
                                method = "L2",
                                ar     = "inside"),
                  list("BH", "L2", "inside")),
    "ar = 'outside' is recommended"
  )

  expect_warning(
    test_srparams(set_sr_params(type   = "HS",
                                method = "L1",
                                ar     = "outside"),
                  list("HS", "L1", "outside")),
    "Non-recommended combination"
  )
})

context("Set Fcurrent using set_f_current()")
tolerance <- 10 ^ -6

vpadata <- load_data("../../inst/extdata/res_vpa_pma.rda")

test_that("by giving manual F values", {

  expect_equal(
    set_f_current(manual = c(0.4901521, 1.1503151, 1.3131002, 1.3130997)),
    c(0.4901521, 1.1503151, 1.3131002, 1.3130997)
  )

  expect_equal(set_f_current(manual = c(1, 2, 3, 4)), 1:4)
  expect_equal(set_f_current(manual = c(100:110)),    100:110)

  expect_error(set_f_current(manual = c("a", "b")))
  expect_error(set_f_current(manual = TRUE))
  expect_error(set_f_current(manual = c(TRUE, FALSE)))

})

test_that("by specifying years to extract from vpa result", {

  expect_equal(
    set_f_current(f_years = 2011, vpadata = vpadata),
    c("0" = 0.635647, "1" = 1.234432, "2" = 1.297157, "3" = 1.297156),
    tolerance = tolerance
  )
  expect_equal(
    set_f_current(f_years = 2010:2011, vpadata = vpadata),
    c("0" = 0.5986131, "1" = 1.1950626, "2" = 1.3735349, "3" = 1.3735346),
    tolerance = tolerance
  )

  simplified_test <- function(year) {
    expect_equal(set_f_current(f_years = year,
                               vpadata = vpadata),
                 extract_xaa(vpadata, "f", year, mean_by = "age"),
                 tolerance = tolerance)
  }

  simplified_test(year = 2011)
  simplified_test(year = 2010:2011)
  simplified_test(year = 2009:2011)
  simplified_test(year = 2009:2010)
  simplified_test(year = 1982)
  simplified_test(year = 1982:1983)
  simplified_test(year = 1982:2011)
  simplified_test(year = 2009)
  simplified_test(year = c(2008, 2011))

})

context("- Calculate from selectivity")

test_that("specified by direct year", {

  expect_equal(
    set_f_current(s_years = 2011,
                  f_years = 2010,
                  vpadata = vpadata),
    c("0" = 0.5961694, "1" = 1.1577659, "2" = 1.2165952, "3" = 1.2165947),
    tolerance = tolerance
  )

  expect_equal(
    set_f_current(s_years = 2009:2011,
                  f_years = 2008:2010,
                  vpadata = vpadata),
    c("0" = 0.5956203, "1" = 1.2892137, "2" = 1.4308449, "3" = 1.4308447),
    tolerance = tolerance
  )

  expect_equal(
    set_f_current(s_years = c(2007, 2009, 2011),
                  f_years = 2009:2011,
                  vpadata = vpadata),
    c("0" = 0.5304168, "1" = 1.1756145, "2" = 1.3993345, "3" = 1.3993343),
    tolerance = tolerance
  )

  msg_arg_collision <- "'manual' option should be used solely"
  expect_error(set_f_current(s_years = 2011,
                             manual = c(1, 2, 3, 4)),
               msg_arg_collision)
  expect_error(set_f_current(s_years = 2011,
                             manual = c(1, 2, 3, 4)),
               msg_arg_collision)
  expect_error(set_f_current(s_years = 2011,
                             f_years = 2011,
                             manual = c(1, 2, 3, 4)),
               msg_arg_collision)

  expect_error(set_f_current(s_years = 2011,
                             manual = c(1, 2, 3, 4)),
               "'manual' option should be used solely")
  expect_error(set_f_current(s_years = 2011),
               "Give 'vpadata' to set_f_current()")
  expect_error(set_f_current(s_years = 2011, vpadata = vpadata),
               "Set 'f_years' to calculate F using selectivity")
})

test_that("specified by relative year", {
  expect_equal(
    set_f_current(s_years = -3:-1,
                  f_years = -3:-1,
                  vpadata = vpadata),
    c("0" = 0.5436309, "1" =  1.1766831, "2" =  1.3059519, "3" =  1.3059517),
    tolerance = tolerance
  )

  expect_equal(
    set_f_current(s_years = -4:-2,
                  f_years = -2:-1,
                  vpadata = vpadata),
    c("0" = 0.6195388, "1" =  1.1910085, "2" =  1.2515271, "3" =  1.2515271),
    tolerance = tolerance
  )
})


test_that("comparison between direct- and relative- year specification", {

  relyr_s <- -4:-2
  relyr_f <- -2:-1

  absyr_s <- 2008:2010
  absyr_f <- 2010:2011

  years <- extract_year_from(vpadata)
  assertthat::assert_that(
    all(select_from_tail(years, relyr_s) == absyr_s)
  )
  assertthat::assert_that(
    all(select_from_tail(years, relyr_f) == absyr_f)
  )

  expect_equal(
    set_f_current(s_years = relyr_s,
                  f_years = relyr_f,
                  vpadata = vpadata),
    set_f_current(s_years = absyr_s,
                  f_years = absyr_f,
                  vpadata = vpadata)
  )
})


context("Retrieve input from the result of scientist meeting")

test_that("usage of retrieve_input()", {

  dummy_future_result <- list(data = "data",
                              input = list(model_average_option = 123))
  retrieved <- retrieve_input(dummy_future_result)

  expect_equal(names(retrieved), "model_average_option")
  expect_equal(retrieved$model_average_option, 123)

  list_without_input <- list(data = "data")

  expect_error(
    retrieve_input(list_without_input),
  )
})


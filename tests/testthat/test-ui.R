context("User interfaces")

test_that("load_vpa() works", {
  expect_is(load_vpa("../../inst/extdata/res_vpa_pma.rda"), "list")
  expect_is(load_vpa("../../inst/extdata/vpa.csv"), "list")
})


context("- generate_sr_weight()")
srdata    <- get.SRdata(load_vpa("../../inst/extdata/res_vpa_pma.rda"),
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

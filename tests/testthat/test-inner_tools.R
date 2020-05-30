context("Tools for developers")

test_that("load_inst() enables simple loading", {
  msy <- load_inst("res_MSY_pma.rda")

  expect_setequal(
    names(msy),
    c("summary", "summaryAR", "summary_tb", "F.msy", "all.stat", "all.statAR",
      "all.stat_tb", "trace", "input.list", "ssb.ar.mean", "SPR.msy")
  )

  vpa <- load_inst("res_vpa_pma.rda")

  expect_setequal(
    names(vpa),
    c("input", "term.f", "np", "minimum", "minimum.c", "logLik", "gradient",
      "code", "q", "b", "sigma", "convergence", "message", "hessian", "Ft",
      "Fc.at.age", "Fc.mean", "Fc.max", "last.year", "Pope", "ssb.coef",
      "pred.index", "wcaa", "naa", "faa", "baa", "ssb", "saa")
  )
})


test_that("return_file_type() works", {
  expect_equal(return_file_type("foo.csv"), "csv")
  expect_equal(return_file_type("foo.rda"), "rda")

  expect_error(return_file_type("foo.csvv"), "Unknown file type")
  expect_error(return_file_type("foo.rdaa"), "Unknown file type")
})

test_that("check_sr_params() returns error to bad arguments", {
  expect_error(check_sr_params(type = "foo", method = "L1"))
  expect_error(check_sr_params(type = "",    method = "L1"))
  expect_error(check_sr_params(type = "",    method = "L3"))
  expect_error(check_sr_params(type = "",    method = "L3"))
})


test_that("check_ar_params() accepts only ar = inside/outside", {

  expect_warning(check_ar_params(method = "L2", ar = "inside"),
                 "ar = 'outside' is recommended.")
  expect_null(check_ar_params(method = "L2", ar = "outside"))

  msg <- "ar is not a character"
  expect_error(check_ar_params(method = "L1", ar = 3),     msg)
  expect_error(check_ar_params(method = "L1", ar = TRUE),  msg)
  expect_error(check_ar_params(method = "L1", ar = FALSE), msg)

  expect_error(check_ar_params(method = "L1", ar = "foo"),
               "'ar' should be either 'inside' or 'outside'")

})

test_that("check_ar_params() warns to non-recommended args", {
  expect_warning(check_ar_params(method = "L1", ar = "outside"),
                 "Non-recommended combination")

  msg <- "ar = 'outside' is recommended."
  expect_warning(check_ar_params(method = "L1", ar = "inside"), msg)
  expect_warning(check_ar_params(method = "L2", ar = "inside"), msg)
})

context("Extract something from object")

vpadata <- load_vpa("../../inst/extdata/res_vpa_pma.rda")

test_that("extract_xaa() extracts values from VPA result", {
  expect_equal(
    extract_xaa(vpadata, "f", 2011), vpadata$faa["2011"]
  )
  expect_equal(
    extract_xaa(vpadata, "f", 2010:2011), vpadata$faa[as.character(2010:2011)]
  )

  expect_equal(
    extract_xaa(vpadata, "n", 2011), vpadata$naa["2011"]
  )
  expect_equal(
    extract_xaa(vpadata, "n", 2010:2011), vpadata$naa[as.character(2010:2011)]
  )
})

context("- extract_year_from()")

test_that("from vpa object", {
  years <- as.numeric(colnames(vpadata$faa))

  expect_equal(extract_year_from(vpadata = vpadata), years)
  expect_equal(extract_year_from(vpadata = vpadata), years)
  expect_equal(extract_year_from(vpadata = vpadata, var = "naa"), years)
  expect_equal(extract_year_from(vpadata = vpadata, var = "baa"), years)
  expect_equal(extract_year_from(vpadata = vpadata, var = "faa"), years)
  expect_equal(extract_year_from(vpadata = vpadata, var = "saa"), years)

  expect_error(extract_year_from(),
               "Give me either 'vpadata' or 'msydata'")
})

test_that("from vpa object", {
  expect_error(extract_year_from(msydata = "input_MSY_object_here"),
               "Not implemented")
})


context("Handling vectors")

test_that("select_from_tail() selects elements from tail", {
  vec <- 1:10
  expect_equal(select_from_tail(vec, relative = -1),    10)
  expect_equal(select_from_tail(vec, relative = -3:-1), 8:10)
  expect_equal(select_from_tail(vec, relative = -1:-3), 8:10)

  msg <- "'relative' should be negative"
  expect_error(select_from_tail(vec, relative = 0), msg)
  expect_error(select_from_tail(vec, relative = 1), msg)
})

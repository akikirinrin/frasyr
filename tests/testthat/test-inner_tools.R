context("Tools for developers")

test_that("load_inst() enables simple loading", {
  msy <- load_inst("res_MSY_pma.rda")
  load(system.file("extdata", "res_MSY_pma.rda", package = "frasyr"))

  expect_setequal(
    names(msy),
    c("summary", "summaryAR", "summary_tb", "F.msy", "all.stat", "all.statAR",
      "all.stat_tb", "trace", "input.list", "ssb.ar.mean", "SPR.msy")
  )

  vpa <- load_inst("res_vpa_pma.rda")
  load(system.file("extdata", "res_vpa_pma.rda", package = "frasyr"))

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

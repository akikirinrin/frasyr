context("User interfaces")

test_that("load_vpa() works", {
  expect_is(load_vpa("../../inst/extdata/res_vpa_pma.rda"), "list")
  expect_is(load_vpa("../../inst/extdata/vpa.csv"), "list")
})


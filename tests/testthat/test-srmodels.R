context("Utils for selecting SR models")

data(res_vpa)

test_that("get.SRdata returns data frame", {
  expect_equal(class(get.SRdata(res_vpa, return.df = TRUE)),
               "data.frame")
  expect_equal(dim(get.SRdata(res_vpa, return.df = TRUE)),
               c(30, 3))
})

test_that("get.SRdata returns list", {
  expect_false(class(get.SRdata(res_vpa)) == "data.frame")
  expect_equal(length(get.SRdata(res_vpa)), 3)
  expect_null(dim(get.SRdata(res_vpa)))
})

test_that("get data frame from list object", {
  expect_equal(class(pull_df_from_list(list = res_vpa, dfname = "baa")),
               "data.frame")
  expect_equal(dim(pull_df_from_list(list = res_vpa, dfname = "baa")),
               c(4, 30))
})

test_that("Obtain SR data for plot quickly", {
  output <- make_SR_dframe("HS", "L1", res_vpa)
  expect_is(output, "data.frame")
  expect_equal(nrow(output), 100)
  expect_equal(ncol(output), 3)

  inputs_are <- function(SR, method, return_is) {
    expect_equal(make_SR_dframe(SR = SR, method = method, res_vpa) %>%
                   dplyr::pull(name) %>%
                   unique(),
                 return_is)
  }
  inputs_are("HS", "L1", return_is = "HS_L1")
  inputs_are("HS", "L2", return_is = "HS_L2")
  inputs_are("BH", "L1", return_is = "BH_L1")
  inputs_are("BH", "L2", return_is = "BH_L2")
  inputs_are("RI", "L1", return_is = "RI_L1")
  inputs_are("RI", "L2", return_is = "RI_L2")

})

test_that("normality test works correctly", {
  does_normally_distribute <- function(x, sd, expect) {
    expect_equal(pass_shapiro_and_ks(x = x, sd = sd, p_threshold = 0.05),
                 expect)
  }
  does_normally_distribute(rnorm(100, sd = 0.3), sd = 0.3, TRUE)
  does_normally_distribute(rnorm(100, sd = 0.8), sd = 0.3, FALSE)
  does_normally_distribute(rnorm(100, sd = 0.8), sd = 0.8, TRUE)
  does_normally_distribute(runif(100),           sd = 0.3, FALSE)
  does_normally_distribute(runif(100),           sd = 0.2, FALSE)
  does_normally_distribute(runif(100),           sd = 0.8, FALSE)
})

test_that("is_resid_normdist() enables simple normality test", {
  expect_true(is_resid_normdist(SR = "HS", method = "L1", vpares = res_vpa))
  expect_true(is_resid_normdist(SR = "HS", method = "L2", vpares = res_vpa))
  expect_true(is_resid_normdist(SR = "BH", method = "L1", vpares = res_vpa))
  expect_true(is_resid_normdist(SR = "BH", method = "L2", vpares = res_vpa))
  expect_true(is_resid_normdist(SR = "RI", method = "L1", vpares = res_vpa))
  expect_true(is_resid_normdist(SR = "RI", method = "L2", vpares = res_vpa))
})

test_that("confirm whether normtest function works as procedure in vignettes", {
  # HS-L1
  # Test by procedure in vignettes
  fitted <- res_vpa %>%
    get.SRdata() %>%
    fit.SR("HS", "L1")
  pass_shapiro <- shapiro.test(fitted$resid)$p.value > 0.05
  pass_ks      <-
    ks.test(fitted$resid, y = "pnorm", sd = fitted$pars$sd)$p.value > 0.05

  # compare
  expect_equal(is_resid_normdist(SR = "HS", method = "L1", vpares = res_vpa),
               as.logical(pass_shapiro * pass_ks))
  # -----------------------------------------------
  # HS-L2
  # Test by procedure in vignettes
  fitted <- res_vpa %>%
    get.SRdata() %>%
    fit.SR("HS", "L2")
  pass_shapiro <- shapiro.test(fitted$resid)$p.value > 0.05
  pass_ks      <-
    ks.test(fitted$resid, y = "pnorm", sd = fitted$pars$sd)$p.value > 0.05

  # compare
  expect_equal(is_resid_normdist(SR = "HS", method = "L2", vpares = res_vpa),
               as.logical(pass_shapiro * pass_ks))
})

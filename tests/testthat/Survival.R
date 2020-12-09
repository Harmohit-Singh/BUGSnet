library(testthat)
library(BUGSnet)

data(diabetes.sim)
rate2.slr <- data.prep(arm.data = diabetes.sim,
                       varname.t = "Treatment",
                       varname.s = "Study")

test_that("Data Preparation",
          {
            expect_that(rate2.slr, is_a("BUGSnetData"))
          })
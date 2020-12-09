library(testthat)
library(BUGSnet)

test_that("Data Preparation",
          {
            data(diabetes.sim)
            rate2.slr <- data.prep(arm.data = diabetes.sim,
                                   varname.t = "Treatment",
                                   varname.s = "Study")
            expect_that(rate2.slr, is_a("BUGSnetData"))
          })
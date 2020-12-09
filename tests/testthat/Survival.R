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

network.char <- net.tab(data = rate2.slr,
                        outcome = "diabetes",
                        N = "n",
                        type.outcome = "rate2",
                        time = "followup")

test_that("net.tab",
          {
            expect_true(nrow(network.char$network)==13)
          })
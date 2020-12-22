library(testthat)
library(BUGSnet)

data(afib)
dataprep <- data.prep(arm.data = afib,
                      varname.t = "treatment",
                      varname.s = "study")

fe_inconsistency_model <- nma.model(data=dataprep,
                                    outcome="events",
                                    N="sampleSize",
                                    reference="02",
                                    family="binomial",
                                    link="logit",
                                    effects="fixed",
                                    type="inconsistency",
                                    covariate="stroke",
                                    prior.beta="EXCHANGEABLE")

expect_error(nma.run(fe_inconsistency_model, n.iter = 1000), NA)


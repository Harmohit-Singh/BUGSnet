library(testthat)
library(BUGSnet)
library(vdiffr)

# Load sample dataset

df <- readRDS("test_Survival_results/df.rds")

# Prepare Data

rate2.slr <- data.prep(arm.data = df,
                       varname.t = "Treatment",
                       varname.s = "Study")

result <- readRDS("test_Survival_results/rate2.slr.rds")

test_that("Data Preparation",
          {
            expect_equal(rate2.slr, result)
          })

# Plot Network of Evidence

test_that("Plot Network of Evidence",
          {
            expect_doppelganger( title = "Network of Evidence",
            net.plot(rate2.slr, node.scale = 3, edge.scale=1.5))
          })
               
# Generate Network Characteristics

network.char <- net.tab(data = rate2.slr,
                        outcome = "diabetes",
                        N = "n",
                        type.outcome = "rate2",
                        time = "followup")

result <- readRDS("test_Survival_results/network.char.rds")

test_that("Data Preparation",
          {
            expect_equal(network.char, result)
          })

# Plot Patient Characteristics

test_that("Plot Patient Characteristics",
          {
            expect_doppelganger( title = "Patient Characteristics",
                                 data.plot(data = rate2.slr,
                                           covariate = "age", 
                                           half.length = "age_SD",
                                           by = "treatment",
                                           avg.hline=TRUE, #add overall average line?
                                           text.size = 12) 
                                 )
          })

# Build NMA Model

random_effects_model <- nma.model(data=rate2.slr,
                                  outcome="diabetes",
                                  N="n",
                                  reference="Placebo",
                                  family="binomial",
                                  link="cloglog",
                                  time = "followup",
                                  effects= "random")

result <- readRDS("test_Survival_results/random_effects_model.rds")

test_that("Build NMA Model",
          {
            expect_equal(random_effects_model, result)
          })

# Build NMA Model Regression

random_effects_model_reg <- nma.model(data=rate2.slr,
                                      outcome="diabetes",
                                      N="n",
                                      sd = "age_SD",
                                      reference="Placebo",
                                      family="normal",
                                      link="identity",
                                      effects= "random",
                                      covariate = "age",
                                      prior.beta="EXCHANGEABLE")

result <- readRDS("test_Survival_results/random_effects_model_reg.rds")

test_that("Build NMA Model Regression",
          {
            expect_equal(random_effects_model_reg, result)
          })

# Run NMA

set.seed(20190828)
random_effects_results <- nma.run(random_effects_model,
                                  n.adapt=1000,
                                  n.burnin=1000,
                                  n.iter=10000)

result <- readRDS("test_Survival_results/random_effects_results.rds")

test_that("Run NMA",
          {
            expect_equal(random_effects_results, result)
          })

# Run NMA Regression

set.seed(20190828)
random_effects_results_reg <- nma.run(random_effects_model_reg,
                                      n.adapt=1000,
                                      n.burnin=1000,
                                      n.iter=10000)

result <- readRDS("test_Survival_results/random_effects_results_reg.rds")

test_that("Run NMA Regression",
          {
            expect_equal(random_effects_results_reg, result)
          })

# Plot Model Fit

par(mfrow = c(1,2))
re_model_fit <- nma.fit(random_effects_results, main= "Random Effects Model")

result <- readRDS("test_Survival_results/re_model_fit.rds")

test_that("Plot Model Fit",
          {
            expect_equal(re_model_fit, result)
          })

# Check Inconsistency and Fixed Effects NMA Model

re_inconsistency_model <- nma.model(data=rate2.slr,
                                    outcome="diabetes",
                                    N="n",
                                    reference="Placebo",
                                    family="binomial",
                                    link="cloglog",
                                    time = "followup",
                                    type = "inconsistency", #specifies inconsistency model
                                    effects="fixed")

result <- readRDS("test_Survival_results/re_inconsistency_model.rds")

test_that("NMA Model Inconsistency and Fixed Effects",
          {
            expect_equal(re_inconsistency_model, result)
          })

# Check Inconsistency and Fixed Effects NMA Run

set.seed(20190828)

re_inconsistency_results <- nma.run(re_inconsistency_model,
                                    n.adapt=1000,
                                    n.burnin=1000,
                                    n.iter=10000)

result <- readRDS("test_Survival_results/re_inconsistency_results.rds")

test_that("NMA Run Inconsistency and Fixed Effects",
          {
            expect_equal(re_inconsistency_results, result)
          })

# Plot Inconsistency Model Fit

par(mfrow = c(1,2))
inconsist_model_fit <- nma.fit(re_inconsistency_results, main= "Inconsistency Model")

result <- readRDS("test_Survival_results/inconsist_model_fit.rds")

test_that("Plot Inconsistency Model",
          {
            expect_equal(inconsist_model_fit, result)
          })

# Compare

test_that("Compare",
          {
            expect_doppelganger( title = "Compare",
                                 nma.compare(re_model_fit, inconsist_model_fit)        
            )
          })

# SUCRA NMA Rank

sucra.out <- nma.rank(random_effects_results, largerbetter=FALSE, sucra.palette= "Set1")

result <- readRDS("test_Survival_results/sucra.out.rds")

test_that("SUCRA NMA Rank",
          {
            expect_equal(sucra.out, result)
          })

# League Table

league.out <- nma.league(random_effects_results,  
                         central.tdcy="median",
                         order = sucra.out$order,
                         log.scale = FALSE,
                         low.colour = "springgreen4",
                         mid.colour = "white",
                         high.colour = "red",
                         digits = 2)

result <- readRDS("test_Survival_results/league.out.rds")

test_that("NMA League",
          {
            expect_equal(league.out, result)
          })

# Forest Plot

test_that("Forest Plot",
          {
            expect_doppelganger( title = "Forest Plot",
                                 nma.forest(random_effects_results,
                                            central.tdcy="median",
                                            comparator = "Placebo",
                                            log.scale = FALSE)        
            )
          })

# NMA Diagnostics

test_that("NMA Diagnostics",
          {
            expect_doppelganger( title = "NMA Diagnostics",
                                 nma.diag(random_effects_results, plot_prompt = FALSE)       
            )
          })

# Regression Plot

test_that("Regression Plot",
          {
            expect_doppelganger( title = "Regression Plot",
                                nma.regplot(random_effects_results_reg)           
            )
          })

## 포트폴리오 구성

library(PortfolioAnalytics)

pf <- portfolio.spec(assets = colnames(rtn_daily))
pf_sim <- portfolio.spec(assets = colnames(rtn_sim))
pf_det <- portfolio.spec(assets = colnames(rtn_det))
pf_esg <- portfolio.spec(assets = colnames(rtn_esg))
pf_esg_sim <- portfolio.spec(assets = colnames(rtn_esg_sim))
pf_esg_det <- portfolio.spec(assets = colnames(rtn_esg_det))


## 제약조건

# 전체 포트폴리오 - sum of the weights

pf <- add.constraint(portfolio = pf,
                     type = "weight_sum",
                     min_sum = 0.5,
                     max_sum = 0.8)

# 전체 포트폴리오 - full_investment - min_sum = max_sum = 1

pf_sim <- add.constraint(portfolio = pf_sim,
                         type = "full_investment")
pf_det <- add.constraint(portfolio = pf_det,
                         type = "full_investment")
pf_esg <- add.constraint(portfolio = pf_esg,
                         type = "full_investment")
pf_esg_sim <- add.constraint(portfolio = pf_esg_sim,
                         type = "full_investment")
pf_esg_det <- add.constraint(portfolio = pf_esg_det,
                         type = "full_investment")

# 전체 포트폴리오 - dollar_neutral min_sum = max_sum = 0

pf <- add.constraint(portfolio = pf,
                     type = "dollar_neutral")

# 개별 자산 - box

pf <- add.constraint(portfolio = pf,
                     type = "box",
                     min = 0.2,
                     max = 0.7)

pf <- add.constraint(portfolio = pf,
                     type = "box",
                     min = rep(c(0.2, 0.1), 7),
                     max = rep(c(0.7, 0.4), 7))

# 개별 자산 - long_only - min = 0 and max = 1

pf_sim <- add.constraint(portfolio = pf_sim,
                         type = "long_only")
pf_det <- add.constraint(portfolio = pf_det,
                         type = "long_only")
pf_esg <- add.constraint(portfolio = pf_esg,
                         type = "long_only")
pf_esg_sim <- add.constraint(portfolio = pf_esg_sim,
                         type = "long_only")
pf_esg_det <- add.constraint(portfolio = pf_esg_det,
                         type = "long_only")

# 개별 자산 - group (sector, region, asset class, etc.)

pf <- add.constraint(portfolio = pf,
                     type = "group",
                     groups = list(1:7, 8:14),
                     group_min = 0.4,
                     group_max = 0.6)

# 제약조건으로 목표수익률을 줄 수도 있음

pf
pf_sim
pf_det
pf_esg
pf_esg_sim
pf_esg_det


## 목적함수

# 최대수익률

pf_sim <- add.objective(portfolio = pf_sim,
                        type = "return",
                        name = "mean")
pf_det <- add.objective(portfolio = pf_det,
                        type = "return",
                        name = "mean")
pf_esg <- add.objective(portfolio = pf_esg,
                        type = "return",
                        name = "mean")
pf_esg_sim <- add.objective(portfolio = pf_esg_sim,
                        type = "return",
                        name = "mean")
pf_esg_det <- add.objective(portfolio = pf_esg_det,
                        type = "return",
                        name = "mean")

# 최소위험

pf_sim <- add.objective(portfolio = pf_sim,
                        type = "risk",
                        name = "StdDev")
pf_det <- add.objective(portfolio = pf_det,
                        type = "risk",
                        name = "StdDev")
pf_esg <- add.objective(portfolio = pf_esg,
                        type = "risk",
                        name = "StdDev")
pf_esg_sim <- add.objective(portfolio = pf_esg_sim,
                        type = "risk",
                        name = "StdDev")
pf_esg_det <- add.objective(portfolio = pf_esg_det,
                        type = "risk",
                        name = "StdDev")

# risk_budget

pf <- add.objective(portfolio = pf,
                    type = "risk_budget",
                    name = "StdDev",
                    min_prisk = 0.05,
                    max_prisk = 0.1)

pf
pf_sim
pf_det
pf_esg
pf_esg_sim
pf_esg_det


## 포트폴리오 최적화 - 간단

# 목적함수 2가지이므로 평균분산포트폴리오
# 최소분산포트폴리오는 목적함수에 위험 하나만 포함

library(ROI)
library(ROI.plugin.glpk)
library(ROI.plugin.quadprog)

# 5가지 optimization methods
# ROI: R Optimization Infrastructure for linear and quadratic programming solvers
# random: Random portfolios
# DEoptim: Differential evolution
# GenSA: Generalized Simulated Annealing
# pso: Particle swarm optimization

# 4가지 portfolio moments
# sample: basic sample estimate of first four moments
# boudt: statistical factor model based on the work of Boudt et al., 2014
# black_litterman: first two moments using the Black-Litterman framework
# Meucci: first two moments using the Fully Flexible Views framework

set.seed(1)

opt_sim <- optimize.portfolio(R = rtn_sim,
                              portfolio = pf_sim,
                              optimize_method = "ROI")
opt_sim
extractWeights(opt_sim)
chart.Weights(opt_sim)
extractObjectiveMeasures(opt_sim)

rtn_opt_sim <- Return.portfolio(rtn_sim,
                                weight = extractWeights(opt_sim),
                                rebalance_on = "months")
head(rtn_opt_sim)


opt_det <- optimize.portfolio(R = rtn_det,
                              portfolio = pf_det,
                              optimize_method = "ROI")
opt_det
extractWeights(opt_det)
chart.Weights(opt_det)
extractObjectiveMeasures(opt_det)

rtn_opt_det <- Return.portfolio(rtn_det,
                                weight = extractWeights(opt_det),
                                rebalance_on = "months")
head(rtn_opt_det)


opt_esg <- optimize.portfolio(R = rtn_esg,
                              portfolio = pf_esg,
                              optimize_method = "ROI")
opt_esg
extractWeights(opt_esg)
chart.Weights(opt_esg)
extractObjectiveMeasures(opt_esg)

rtn_opt_esg <- Return.portfolio(rtn_esg,
                                weight = extractWeights(opt_esg),
                                rebalance_on = "months")
head(rtn_opt_esg)


opt_esg_sim <- optimize.portfolio(R = rtn_esg_sim,
                              portfolio = pf_esg_sim,
                              optimize_method = "ROI")
opt_esg_sim
extractWeights(opt_esg_sim)
chart.Weights(opt_esg_sim)
extractObjectiveMeasures(opt_esg_sim)

rtn_opt_esg_sim <- Return.portfolio(rtn_esg_sim,
                                weight = extractWeights(opt_esg_sim),
                                rebalance_on = "months")
head(rtn_opt_esg_sim)


opt_esg_det <- optimize.portfolio(R = rtn_esg_det,
                              portfolio = pf_esg_det,
                              optimize_method = "ROI")
opt_esg_det
extractWeights(opt_esg_det)
chart.Weights(opt_esg_det)
extractObjectiveMeasures(opt_esg_det)

rtn_opt_esg_det <- Return.portfolio(rtn_esg_det,
                                weight = extractWeights(opt_esg_det),
                                rebalance_on = "months")
head(rtn_opt_esg_det)


rtn_opt_all <- cbind(rtn_opt_sim,
                     rtn_opt_det,
                     rtn_opt_esg,
                     rtn_opt_esg_sim,
                     rtn_opt_esg_det)

colnames(rtn_opt_all) <- c("simple",
                           "detail",
                           "esg",
                           "esg+simple",
                           "esg+detail")

table.AnnualizedReturns(R = rtn_opt_all, Rf = 0.025/252)


## 포트폴리오 최적화 - Single Period, Random Portfolios 포함

set.seed(1)

rp <- random_portfolios(portfolio = pf_sim,
                        permutations = 500,
                        rp_method = "simplex") #method: simplex, sample, grid

opt_sim_rp <- optimize.portfolio(R = rtn_sim,
                                 portfolio = pf_sim,
                                 optimize_method = "random",
                                 rp = rp,
                                 trace = TRUE)
extractWeights(opt_sim_rp)
chart.Weights(opt_sim_rp)
extractObjectiveMeasures(opt_sim_rp)

rp <- random_portfolios(portfolio = pf_det,
                        permutations = 500,
                        rp_method = "simplex") #method: simplex, sample, grid

opt_det_rp <- optimize.portfolio(R = rtn_det,
                                 portfolio = pf_det,
                                 optimize_method = "random",
                                 rp = rp,
                                 trace = TRUE)
extractWeights(opt_det_rp)
chart.Weights(opt_det_rp)
extractObjectiveMeasures(opt_det_rp)

rp <- random_portfolios(portfolio = pf_esg,
                        permutations = 500,
                        rp_method = "simplex") #method: simplex, sample, grid

opt_esg_rp <- optimize.portfolio(R = rtn_esg,
                                 portfolio = pf_esg,
                                 optimize_method = "random",
                                 rp = rp,
                                 trace = TRUE)
extractWeights(opt_esg_rp)
chart.Weights(opt_esg_rp)
extractObjectiveMeasures(opt_esg_rp)

rp <- random_portfolios(portfolio = pf_esg,
                        permutations = 500,
                        rp_method = "simplex") #method: simplex, sample, grid

opt_esg_rp <- optimize.portfolio(R = rtn_esg,
                                 portfolio = pf_esg,
                                 optimize_method = "random",
                                 rp = rp,
                                 trace = TRUE)
extractWeights(opt_esg_rp)
chart.Weights(opt_esg_rp)
extractObjectiveMeasures(opt_esg_rp)

rp <- random_portfolios(portfolio = pf_esg_sim,
                        permutations = 500,
                        rp_method = "simplex") #method: simplex, sample, grid

opt_esg_sim_rp <- optimize.portfolio(R = rtn_esg_sim,
                                 portfolio = pf_esg_sim,
                                 optimize_method = "random",
                                 rp = rp,
                                 trace = TRUE)
extractWeights(opt_esg_sim_rp)
chart.Weights(opt_esg_sim_rp)
extractObjectiveMeasures(opt_esg_sim_rp)

rp <- random_portfolios(portfolio = pf_esg_det,
                        permutations = 500,
                        rp_method = "simplex") #method: simplex, sample, grid

opt_esg_det_rp <- optimize.portfolio(R = rtn_esg_det,
                                     portfolio = pf_esg_det,
                                     optimize_method = "random",
                                     rp = rp,
                                     trace = TRUE)
extractWeights(opt_esg_det_rp)
chart.Weights(opt_esg_det_rp)
extractObjectiveMeasures(opt_esg_det_rp)


rtn_opt_sim_rp <- Return.portfolio(rtn_sim,
                                   weight = extractWeights(opt_sim_rp),
                                   rebalance_on = "months")
rtn_opt_det_rp <- Return.portfolio(rtn_det,
                                   weight = extractWeights(opt_det_rp),
                                   rebalance_on = "months")
rtn_opt_esg_rp <- Return.portfolio(rtn_esg,
                                   weight = extractWeights(opt_esg_rp),
                                   rebalance_on = "months")
rtn_opt_esg_sim_rp <- Return.portfolio(rtn_esg_sim,
                                   weight = extractWeights(opt_esg_sim_rp),
                                   rebalance_on = "months")
rtn_opt_esg_det_rp <- Return.portfolio(rtn_esg_det,
                                   weight = extractWeights(opt_esg_det_rp),
                                   rebalance_on = "months")

rtn_opt_all_rp <- cbind(rtn_opt_sim_rp,
                        rtn_opt_det_rp,
                        rtn_opt_esg_rp,
                        rtn_opt_esg_sim_rp,
                        rtn_opt_esg_det_rp)

colnames(rtn_opt_all_rp) <- c("simple",
                              "detail",
                              "esg",
                              "esg+simple",
                              "esg+detail")

table.AnnualizedReturns(R = rtn_opt_all_rp, Rf = 0.025/252)


## 포트폴리오 최적화 - Periodic rebalancing, Random Portfolios 포함

# search_size, how many portfolios to test, is set to 1000 (default: 20,000)
# rebalance_on - frequency must be specified
# training_period - for the initial optimization
# rolling_window - for the window width (NULL: use all data available)

set.seed(1)

rp <- random_portfolios(portfolio = pf_sim,
                        permutations = 50,
                        rp_method = "simplex")

opt_sim_rb <- optimize.portfolio.rebalancing(R = rtn_sim,
                                           portfolio = pf_sim,
                                           optimize_method = "random",
                                           rp = rp,
                                           trace = TRUE,
                                           search_size = 1000,
                                           rebalance_on = "quarters",
                                           training_period = 360,
                                           rolling_window = 360)
head(extractWeights(opt_sim_rb))
chart.Weights(opt_sim_rb)
head(extractObjectiveMeasures(opt_sim_rb))

rp <- random_portfolios(portfolio = pf_det,
                        permutations = 50,
                        rp_method = "simplex")

opt_det_rb <- optimize.portfolio.rebalancing(R = rtn_det,
                                             portfolio = pf_det,
                                             optimize_method = "random",
                                             rp = rp,
                                             trace = TRUE,
                                             search_size = 1000,
                                             rebalance_on = "quarters",
                                             training_period = 360,
                                             rolling_window = 360)
head(extractWeights(opt_det_rb))
chart.Weights(opt_det_rb)
head(extractObjectiveMeasures(opt_det_rb))

rp <- random_portfolios(portfolio = pf_esg,
                        permutations = 50,
                        rp_method = "simplex")

opt_esg_rb <- optimize.portfolio.rebalancing(R = rtn_esg,
                                             portfolio = pf_esg,
                                             optimize_method = "random",
                                             rp = rp,
                                             trace = TRUE,
                                             search_size = 1000,
                                             rebalance_on = "quarters",
                                             training_period = 360,
                                             rolling_window = 360)
head(extractWeights(opt_esg_rb))
chart.Weights(opt_esg_rb)
head(extractObjectiveMeasures(opt_esg_rb))

rp <- random_portfolios(portfolio = pf_esg_sim,
                        permutations = 50,
                        rp_method = "simplex")

opt_esg_sim_rb <- optimize.portfolio.rebalancing(R = rtn_esg_sim,
                                             portfolio = pf_esg_sim,
                                             optimize_method = "random",
                                             rp = rp,
                                             trace = TRUE,
                                             search_size = 1000,
                                             rebalance_on = "quarters",
                                             training_period = 360,
                                             rolling_window = 360)
head(extractWeights(opt_esg_sim_rb))
chart.Weights(opt_esg_sim_rb)
head(extractObjectiveMeasures(opt_esg_sim_rb))

rp <- random_portfolios(portfolio = pf_esg_det,
                        permutations = 50,
                        rp_method = "simplex")

opt_esg_det_rb <- optimize.portfolio.rebalancing(R = rtn_esg_det,
                                             portfolio = pf_esg_det,
                                             optimize_method = "random",
                                             rp = rp,
                                             trace = TRUE,
                                             search_size = 1000,
                                             rebalance_on = "quarters",
                                             training_period = 360,
                                             rolling_window = 360)
head(extractWeights(opt_esg_det_rb))
chart.Weights(opt_esg_det_rb)
head(extractObjectiveMeasures(opt_esg_det_rb))


rtn_opt_sim_rb <- Return.portfolio(rtn_sim,
                                   weight = extractWeights(opt_sim_rb),
                                   rebalance_on = "months")
rtn_opt_det_rb <- Return.portfolio(rtn_det,
                                   weight = extractWeights(opt_det_rb),
                                   rebalance_on = "months")
rtn_opt_esg_rb <- Return.portfolio(rtn_esg,
                                   weight = extractWeights(opt_esg_rb),
                                   rebalance_on = "months")
rtn_opt_esg_sim_rb <- Return.portfolio(rtn_esg_sim,
                                       weight = extractWeights(opt_esg_sim_rb),
                                       rebalance_on = "months")
rtn_opt_esg_det_rb <- Return.portfolio(rtn_esg_det,
                                       weight = extractWeights(opt_esg_det_rb),
                                       rebalance_on = "months")

rtn_opt_all_rb <- cbind(rtn_opt_sim_rb,
                        rtn_opt_det_rb,
                        rtn_opt_esg_rb,
                        rtn_opt_esg_sim_rb,
                        rtn_opt_esg_det_rb)

colnames(rtn_opt_all_rb) <- c("simple",
                              "detail",
                              "esg",
                              "esg+simple",
                              "esg+detail")

table.AnnualizedReturns(R = rtn_opt_all_rb, Rf = 0.025/252)

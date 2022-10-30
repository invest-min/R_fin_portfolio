## 목적함수 2개인 평균분산포트폴리오 찾기
# 최소분산포트폴리오는 목적함수에 위험 하나만 포함시켜 계산

## 5가지 optimization methods
# ROI: R Optimization Infrastructure for linear and quadratic programming solvers
# random: Random portfolios
# DEoptim: Differential evolution
# GenSA: Generalized Simulated Annealing
# pso: Particle swarm optimization

## 4가지 portfolio moments
# sample: basic sample estimate of first four moments
# boudt: statistical factor model based on the work of Boudt et al., 2014
# black_litterman: first two moments using the Black-Litterman framework
# Meucci: first two moments using the Fully Flexible Views framework

## 최적화

# ROI

library(ROI)
library(ROI.plugin.glpk)
library(ROI.plugin.quadprog)

set.seed(1)

opt_roi <- optimize.portfolio(rtn,
                              portfolio = pf,
                              optimize_method = "ROI",
                              trace = TRUE)
opt_roi
extractWeights(opt_roi)
chart.Weights(opt_roi)
extractObjectiveMeasures(opt_roi)

rtn_roi <- Return.portfolio(rtn,
                            weight = extractWeights(opt_roi),
                            rebalance_on = "months")
head(rtn_roi)

# random

set.seed(1)

opt_ran <- optimize.portfolio(rtn,
                              portfolio = pf,
                              optimize_method = "random",
                              trace = TRUE)
opt_ran
extractWeights(opt_ran)
chart.Weights(opt_ran)
extractObjectiveMeasures(opt_ran)

rtn_ran <- Return.portfolio(rtn,
                            weight = extractWeights(opt_ran),
                            rebalance_on = "months")
head(rtn_ran)

# DEoptim

set.seed(1)

opt_deo <- optimize.portfolio(rtn,
                              portfolio = pf,
                              optimize_method = "DEoptim",
                              trace = TRUE)
opt_deo
extractWeights(opt_deo)
chart.Weights(opt_deo)
extractObjectiveMeasures(opt_deo)

rtn_deo <- Return.portfolio(rtn,
                            weight = extractWeights(opt_deo),
                            rebalance_on = "months")
head(rtn_deo)

# GenSA

library(GenSA)

set.seed(1)

opt_gsa <- optimize.portfolio(rtn,
                              portfolio = pf,
                              optimize_method = "GenSA",
                              trace = TRUE)
opt_gsa
extractWeights(opt_gsa)
chart.Weights(opt_gsa)
extractObjectiveMeasures(opt_gsa)

rtn_gsa <- Return.portfolio(rtn,
                            weight = extractWeights(opt_gsa),
                            rebalance_on = "months")
head(rtn_gsa)

# pso

library(pso)

set.seed(1)

opt_pso <- optimize.portfolio(rtn,
                              portfolio = pf,
                              optimize_method = "pso",
                              trace = TRUE)
opt_pso
extractWeights(opt_pso)
chart.Weights(opt_pso)
extractObjectiveMeasures(opt_pso)

rtn_pso <- Return.portfolio(rtn,
                            weight = extractWeights(opt_pso),
                            rebalance_on = "months")
head(rtn_pso)


## 결과비교

rtn_all <- cbind(rtn_roi, rtn_ran, rtn_deo, rtn_gsa, rtn_pso)
colnames(rtn_all) <- c("ROI", "Random", "DEO", "GSA", "PSO")
table.AnnualizedReturns(rtn_all, Rf = rf_average/252)

wgt_roi <- extractWeights(opt_roi)
wgt_ran <- extractWeights(opt_ran)
wgt_deo <- extractWeights(opt_deo)
wgt_gsa <- extractWeights(opt_gsa)
wgt_pso <- extractWeights(opt_pso)

(rbind(wgt_roi, wgt_ran, wgt_deo, wgt_gsa, wgt_pso))

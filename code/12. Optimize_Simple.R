# 목적함수 2개인 평균분산포트폴리오 찾기
# 최소분산포트폴리오는 목적함수에 위험 하나만 포함시켜 계산

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

## ROI

# 최적화

library(ROI)
library(ROI.plugin.glpk)
library(ROI.plugin.quadprog)

set.seed(1)

opt_3 <- optimize.portfolio(rtn_3,
                            portfolio = pf_3,
                            optimize_method = "ROI",
                            trace = TRUE)
opt_3
extractWeights(opt_3)
chart.Weights(opt_3)
extractObjectiveMeasures(opt_3)

rtn_opt_3 <- Return.portfolio(rtn_3[, 1:3],
                              weight = extractWeights(opt_3),
                              rebalance_on = "months")
head(rtn_opt_3)

opt_31 <- optimize.portfolio(rtn_31,
                             portfolio = pf_31,
                             optimize_method = "ROI",
                             trace = TRUE)
opt_31
extractWeights(opt_31)
chart.Weights(opt_31)
extractObjectiveMeasures(opt_31)

rtn_opt_31 <- Return.portfolio(rtn_31[, 1:3],
                               weight = extractWeights(opt_31),
                               rebalance_on = "months")
head(rtn_opt_31)

opt_32 <- optimize.portfolio(rtn_32,
                             portfolio = pf_32,
                             optimize_method = "ROI",
                             trace = TRUE)
opt_32
extractWeights(opt_32)
chart.Weights(opt_32)
extractObjectiveMeasures(opt_32)

rtn_opt_32 <- Return.portfolio(rtn_32[, 1:3],
                               weight = extractWeights(opt_32),
                               rebalance_on = "months")
head(rtn_opt_32)

opt_3_esg <- optimize.portfolio(rtn_3,
                                portfolio = pf_3_esg,
                                optimize_method = "ROI",
                                trace = TRUE)
opt_3_esg
extractWeights(opt_3_esg)
chart.Weights(opt_3_esg)
extractObjectiveMeasures(opt_3_esg)

rtn_opt_3_esg <- Return.portfolio(rtn_3[, 4:6],
                                  weight = extractWeights(opt_3_esg),
                                  rebalance_on = "months")
head(rtn_opt_3_esg)

opt_31_esg <- optimize.portfolio(rtn_31,
                                 portfolio = pf_31_esg,
                                 optimize_method = "ROI",
                                 trace = TRUE)
opt_31_esg
extractWeights(opt_31_esg)
chart.Weights(opt_31_esg)
extractObjectiveMeasures(opt_31_esg)

rtn_opt_31_esg <- Return.portfolio(rtn_3[, 4:6],
                                   weight = extractWeights(opt_31_esg),
                                   rebalance_on = "months")
head(rtn_opt_31_esg)

opt_32_esg <- optimize.portfolio(rtn_3,
                                 portfolio = pf_32_esg,
                                 optimize_method = "ROI",
                                 trace = TRUE)
opt_32_esg
extractWeights(opt_32_esg)
chart.Weights(opt_32_esg)
extractObjectiveMeasures(opt_32_esg)

rtn_opt_32_esg <- Return.portfolio(rtn_32[, 4:6],
                                   weight = extractWeights(opt_32_esg),
                                   rebalance_on = "months")
head(rtn_opt_32_esg)

# 결과비교

rtn_opt_3_all <- cbind(rtn_opt_3, rtn_opt_3_esg)
rtn_opt_31_all <- cbind(rtn_opt_31, rtn_opt_31_esg)
rtn_opt_32_all <- cbind(rtn_opt_32, rtn_opt_32_esg)

colnames(rtn_opt_3_all) <- c("pf_3", "pf_3_esg")
colnames(rtn_opt_31_all) <- c("pf_31", "pf_31_esg")
colnames(rtn_opt_32_all) <- c("pf_32", "pf_32_esg")

rtn_opt <- cbind(table.AnnualizedReturns(rtn_opt_3_all, Rf = rf_3_average/252),
                 table.AnnualizedReturns(rtn_opt_31_all, Rf = rf_31_average/252),
                 table.AnnualizedReturns(rtn_opt_32_all, Rf = rf_32_average/252))
rtn_opt


wgt_3 <- extractWeights(opt_3)
wgt_31 <- extractWeights(opt_31)
wgt_32 <- extractWeights(opt_32)
wgt_3_esg <- extractWeights(opt_3_esg)
wgt_31_esg <- extractWeights(opt_31_esg)
wgt_32_esg <- extractWeights(opt_32_esg)

(cbind(rbind(wgt_3, wgt_31, wgt_32), rbind(wgt_3_esg, wgt_31_esg, wgt_32_esg)))


## random

# 최적화

set.seed(1)

opt_3 <- optimize.portfolio(rtn_3,
                            portfolio = pf_3,
                            optimize_method = "random")
opt_3
extractWeights(opt_3)
chart.Weights(opt_3)
extractObjectiveMeasures(opt_3)

rtn_opt_3 <- Return.portfolio(rtn_3[, 1:3],
                              weight = extractWeights(opt_3),
                              rebalance_on = "months")
head(rtn_opt_3)

opt_31 <- optimize.portfolio(rtn_31,
                             portfolio = pf_31,
                             optimize_method = "random")
opt_31
extractWeights(opt_31)
chart.Weights(opt_31)
extractObjectiveMeasures(opt_31)

rtn_opt_31 <- Return.portfolio(rtn_31[, 1:3],
                               weight = extractWeights(opt_31),
                               rebalance_on = "months")
head(rtn_opt_31)

opt_32 <- optimize.portfolio(rtn_32,
                             portfolio = pf_32,
                             optimize_method = "random")
opt_32
extractWeights(opt_32)
chart.Weights(opt_32)
extractObjectiveMeasures(opt_32)

rtn_opt_32 <- Return.portfolio(rtn_32[, 1:3],
                               weight = extractWeights(opt_32),
                               rebalance_on = "months")
head(rtn_opt_32)

opt_3_esg <- optimize.portfolio(rtn_3,
                                portfolio = pf_3_esg,
                                optimize_method = "random")
opt_3_esg
extractWeights(opt_3_esg)
chart.Weights(opt_3_esg)
extractObjectiveMeasures(opt_3_esg)

rtn_opt_3_esg <- Return.portfolio(rtn_3[, 4:6],
                                  weight = extractWeights(opt_3_esg),
                                  rebalance_on = "months")
head(rtn_opt_3_esg)

opt_31_esg <- optimize.portfolio(rtn_31,
                                 portfolio = pf_31_esg,
                                 optimize_method = "random")
opt_31_esg
extractWeights(opt_31_esg)
chart.Weights(opt_31_esg)
extractObjectiveMeasures(opt_31_esg)

rtn_opt_31_esg <- Return.portfolio(rtn_3[, 4:6],
                                   weight = extractWeights(opt_31_esg),
                                   rebalance_on = "months")
head(rtn_opt_31_esg)

opt_32_esg <- optimize.portfolio(rtn_3,
                                 portfolio = pf_32_esg,
                                 optimize_method = "random")
opt_32_esg
extractWeights(opt_32_esg)
chart.Weights(opt_32_esg)
extractObjectiveMeasures(opt_32_esg)

rtn_opt_32_esg <- Return.portfolio(rtn_32[, 4:6],
                                   weight = extractWeights(opt_32_esg),
                                   rebalance_on = "months")
head(rtn_opt_32_esg)

# 결과비교

rtn_opt_3_all <- cbind(rtn_opt_3, rtn_opt_3_esg)
rtn_opt_31_all <- cbind(rtn_opt_31, rtn_opt_31_esg)
rtn_opt_32_all <- cbind(rtn_opt_32, rtn_opt_32_esg)

colnames(rtn_opt_3_all) <- c("pf_3", "pf_3_esg")
colnames(rtn_opt_31_all) <- c("pf_31", "pf_31_esg")
colnames(rtn_opt_32_all) <- c("pf_32", "pf_32_esg")

rtn_opt <- cbind(table.AnnualizedReturns(rtn_opt_3_all, Rf = rf_3_average/252),
                 table.AnnualizedReturns(rtn_opt_31_all, Rf = rf_31_average/252),
                 table.AnnualizedReturns(rtn_opt_32_all, Rf = rf_32_average/252))
rtn_opt


wgt_3 <- extractWeights(opt_3)
wgt_31 <- extractWeights(opt_31)
wgt_32 <- extractWeights(opt_32)
wgt_3_esg <- extractWeights(opt_3_esg)
wgt_31_esg <- extractWeights(opt_31_esg)
wgt_32_esg <- extractWeights(opt_32_esg)

(cbind(rbind(wgt_3, wgt_31, wgt_32), rbind(wgt_3_esg, wgt_31_esg, wgt_32_esg)))


## DEoptim

# 최적화

set.seed(1)

opt_3 <- optimize.portfolio(rtn_3,
                            portfolio = pf_3,
                            optimize_method = "DEoptim")
opt_3
extractWeights(opt_3)
chart.Weights(opt_3)
extractObjectiveMeasures(opt_3)

rtn_opt_3 <- Return.portfolio(rtn_3[, 1:3],
                              weight = extractWeights(opt_3),
                              rebalance_on = "months")
head(rtn_opt_3)

opt_31 <- optimize.portfolio(rtn_31,
                             portfolio = pf_31,
                             optimize_method = "DEoptim")
opt_31
extractWeights(opt_31)
chart.Weights(opt_31)
extractObjectiveMeasures(opt_31)

rtn_opt_31 <- Return.portfolio(rtn_31[, 1:3],
                               weight = extractWeights(opt_31),
                               rebalance_on = "months")
head(rtn_opt_31)

opt_32 <- optimize.portfolio(rtn_32,
                             portfolio = pf_32,
                             optimize_method = "DEoptim")
opt_32
extractWeights(opt_32)
chart.Weights(opt_32)
extractObjectiveMeasures(opt_32)

rtn_opt_32 <- Return.portfolio(rtn_32[, 1:3],
                               weight = extractWeights(opt_32),
                               rebalance_on = "months")
head(rtn_opt_32)

opt_3_esg <- optimize.portfolio(rtn_3,
                                portfolio = pf_3_esg,
                                optimize_method = "DEoptim")
opt_3_esg
extractWeights(opt_3_esg)
chart.Weights(opt_3_esg)
extractObjectiveMeasures(opt_3_esg)

rtn_opt_3_esg <- Return.portfolio(rtn_3[, 4:6],
                                  weight = extractWeights(opt_3_esg),
                                  rebalance_on = "months")
head(rtn_opt_3_esg)

opt_31_esg <- optimize.portfolio(rtn_31,
                                 portfolio = pf_31_esg,
                                 optimize_method = "DEoptim")
opt_31_esg
extractWeights(opt_31_esg)
chart.Weights(opt_31_esg)
extractObjectiveMeasures(opt_31_esg)

rtn_opt_31_esg <- Return.portfolio(rtn_3[, 4:6],
                                   weight = extractWeights(opt_31_esg),
                                   rebalance_on = "months")
head(rtn_opt_31_esg)

opt_32_esg <- optimize.portfolio(rtn_3,
                                 portfolio = pf_32_esg,
                                 optimize_method = "DEoptim")
opt_32_esg
extractWeights(opt_32_esg)
chart.Weights(opt_32_esg)
extractObjectiveMeasures(opt_32_esg)

rtn_opt_32_esg <- Return.portfolio(rtn_32[, 4:6],
                                   weight = extractWeights(opt_32_esg),
                                   rebalance_on = "months")
head(rtn_opt_32_esg)

# 결과비교

rtn_opt_3_all <- cbind(rtn_opt_3, rtn_opt_3_esg)
rtn_opt_31_all <- cbind(rtn_opt_31, rtn_opt_31_esg)
rtn_opt_32_all <- cbind(rtn_opt_32, rtn_opt_32_esg)

colnames(rtn_opt_3_all) <- c("pf_3", "pf_3_esg")
colnames(rtn_opt_31_all) <- c("pf_31", "pf_31_esg")
colnames(rtn_opt_32_all) <- c("pf_32", "pf_32_esg")

rtn_opt <- cbind(table.AnnualizedReturns(rtn_opt_3_all, Rf = rf_3_average/252),
                 table.AnnualizedReturns(rtn_opt_31_all, Rf = rf_31_average/252),
                 table.AnnualizedReturns(rtn_opt_32_all, Rf = rf_32_average/252))
rtn_opt


wgt_3 <- extractWeights(opt_3)
wgt_31 <- extractWeights(opt_31)
wgt_32 <- extractWeights(opt_32)
wgt_3_esg <- extractWeights(opt_3_esg)
wgt_31_esg <- extractWeights(opt_31_esg)
wgt_32_esg <- extractWeights(opt_32_esg)

(cbind(rbind(wgt_3, wgt_31, wgt_32), rbind(wgt_3_esg, wgt_31_esg, wgt_32_esg)))


## GenSA

# 최적화

library(GenSA)

set.seed(1)

opt_3 <- optimize.portfolio(rtn_3,
                            portfolio = pf_3,
                            optimize_method = "GenSA")
opt_3
extractWeights(opt_3)
chart.Weights(opt_3)
extractObjectiveMeasures(opt_3)

rtn_opt_3 <- Return.portfolio(rtn_3[, 1:3],
                              weight = extractWeights(opt_3),
                              rebalance_on = "months")
head(rtn_opt_3)

opt_31 <- optimize.portfolio(rtn_31,
                             portfolio = pf_31,
                             optimize_method = "GenSA")
opt_31
extractWeights(opt_31)
chart.Weights(opt_31)
extractObjectiveMeasures(opt_31)

rtn_opt_31 <- Return.portfolio(rtn_31[, 1:3],
                               weight = extractWeights(opt_31),
                               rebalance_on = "months")
head(rtn_opt_31)

opt_32 <- optimize.portfolio(rtn_32,
                             portfolio = pf_32,
                             optimize_method = "GenSA")
opt_32
extractWeights(opt_32)
chart.Weights(opt_32)
extractObjectiveMeasures(opt_32)

rtn_opt_32 <- Return.portfolio(rtn_32[, 1:3],
                               weight = extractWeights(opt_32),
                               rebalance_on = "months")
head(rtn_opt_32)

opt_3_esg <- optimize.portfolio(rtn_3,
                                portfolio = pf_3_esg,
                                optimize_method = "GenSA")
opt_3_esg
extractWeights(opt_3_esg)
chart.Weights(opt_3_esg)
extractObjectiveMeasures(opt_3_esg)

rtn_opt_3_esg <- Return.portfolio(rtn_3[, 4:6],
                                  weight = extractWeights(opt_3_esg),
                                  rebalance_on = "months")
head(rtn_opt_3_esg)

opt_31_esg <- optimize.portfolio(rtn_31,
                                 portfolio = pf_31_esg,
                                 optimize_method = "GenSA")
opt_31_esg
extractWeights(opt_31_esg)
chart.Weights(opt_31_esg)
extractObjectiveMeasures(opt_31_esg)

rtn_opt_31_esg <- Return.portfolio(rtn_3[, 4:6],
                                   weight = extractWeights(opt_31_esg),
                                   rebalance_on = "months")
head(rtn_opt_31_esg)

opt_32_esg <- optimize.portfolio(rtn_3,
                                 portfolio = pf_32_esg,
                                 optimize_method = "GenSA")
opt_32_esg
extractWeights(opt_32_esg)
chart.Weights(opt_32_esg)
extractObjectiveMeasures(opt_32_esg)

rtn_opt_32_esg <- Return.portfolio(rtn_32[, 4:6],
                                   weight = extractWeights(opt_32_esg),
                                   rebalance_on = "months")
head(rtn_opt_32_esg)

# 결과비교

rtn_opt_3_all <- cbind(rtn_opt_3, rtn_opt_3_esg)
rtn_opt_31_all <- cbind(rtn_opt_31, rtn_opt_31_esg)
rtn_opt_32_all <- cbind(rtn_opt_32, rtn_opt_32_esg)

colnames(rtn_opt_3_all) <- c("pf_3", "pf_3_esg")
colnames(rtn_opt_31_all) <- c("pf_31", "pf_31_esg")
colnames(rtn_opt_32_all) <- c("pf_32", "pf_32_esg")

rtn_opt <- cbind(table.AnnualizedReturns(rtn_opt_3_all, Rf = rf_3_average/252),
                 table.AnnualizedReturns(rtn_opt_31_all, Rf = rf_31_average/252),
                 table.AnnualizedReturns(rtn_opt_32_all, Rf = rf_32_average/252))
rtn_opt


wgt_3 <- extractWeights(opt_3)
wgt_31 <- extractWeights(opt_31)
wgt_32 <- extractWeights(opt_32)
wgt_3_esg <- extractWeights(opt_3_esg)
wgt_31_esg <- extractWeights(opt_31_esg)
wgt_32_esg <- extractWeights(opt_32_esg)

(cbind(rbind(wgt_3, wgt_31, wgt_32), rbind(wgt_3_esg, wgt_31_esg, wgt_32_esg)))


## pso

# 최적화

library(pso)

set.seed(1)

opt_3 <- optimize.portfolio(rtn_3,
                            portfolio = pf_3,
                            optimize_method = "pso")
opt_3
extractWeights(opt_3)
chart.Weights(opt_3)
extractObjectiveMeasures(opt_3)

rtn_opt_3 <- Return.portfolio(rtn_3[, 1:3],
                              weight = extractWeights(opt_3),
                              rebalance_on = "months")
head(rtn_opt_3)

opt_31 <- optimize.portfolio(rtn_31,
                             portfolio = pf_31,
                             optimize_method = "pso")
opt_31
extractWeights(opt_31)
chart.Weights(opt_31)
extractObjectiveMeasures(opt_31)

rtn_opt_31 <- Return.portfolio(rtn_31[, 1:3],
                               weight = extractWeights(opt_31),
                               rebalance_on = "months")
head(rtn_opt_31)

opt_32 <- optimize.portfolio(rtn_32,
                             portfolio = pf_32,
                             optimize_method = "pso")
opt_32
extractWeights(opt_32)
chart.Weights(opt_32)
extractObjectiveMeasures(opt_32)

rtn_opt_32 <- Return.portfolio(rtn_32[, 1:3],
                               weight = extractWeights(opt_32),
                               rebalance_on = "months")
head(rtn_opt_32)

opt_3_esg <- optimize.portfolio(rtn_3,
                                portfolio = pf_3_esg,
                                optimize_method = "pso")
opt_3_esg
extractWeights(opt_3_esg)
chart.Weights(opt_3_esg)
extractObjectiveMeasures(opt_3_esg)

rtn_opt_3_esg <- Return.portfolio(rtn_3[, 4:6],
                                  weight = extractWeights(opt_3_esg),
                                  rebalance_on = "months")
head(rtn_opt_3_esg)

opt_31_esg <- optimize.portfolio(rtn_31,
                                 portfolio = pf_31_esg,
                                 optimize_method = "pso")
opt_31_esg
extractWeights(opt_31_esg)
chart.Weights(opt_31_esg)
extractObjectiveMeasures(opt_31_esg)

rtn_opt_31_esg <- Return.portfolio(rtn_3[, 4:6],
                                   weight = extractWeights(opt_31_esg),
                                   rebalance_on = "months")
head(rtn_opt_31_esg)

opt_32_esg <- optimize.portfolio(rtn_3,
                                 portfolio = pf_32_esg,
                                 optimize_method = "pso")
opt_32_esg
extractWeights(opt_32_esg)
chart.Weights(opt_32_esg)
extractObjectiveMeasures(opt_32_esg)

rtn_opt_32_esg <- Return.portfolio(rtn_32[, 4:6],
                                   weight = extractWeights(opt_32_esg),
                                   rebalance_on = "months")
head(rtn_opt_32_esg)

# 결과비교

rtn_opt_3_all <- cbind(rtn_opt_3, rtn_opt_3_esg)
rtn_opt_31_all <- cbind(rtn_opt_31, rtn_opt_31_esg)
rtn_opt_32_all <- cbind(rtn_opt_32, rtn_opt_32_esg)

colnames(rtn_opt_3_all) <- c("pf_3", "pf_3_esg")
colnames(rtn_opt_31_all) <- c("pf_31", "pf_31_esg")
colnames(rtn_opt_32_all) <- c("pf_32", "pf_32_esg")

rtn_opt <- cbind(table.AnnualizedReturns(rtn_opt_3_all, Rf = rf_3_average/252),
                 table.AnnualizedReturns(rtn_opt_31_all, Rf = rf_31_average/252),
                 table.AnnualizedReturns(rtn_opt_32_all, Rf = rf_32_average/252))
rtn_opt


wgt_3 <- extractWeights(opt_3)
wgt_31 <- extractWeights(opt_31)
wgt_32 <- extractWeights(opt_32)
wgt_3_esg <- extractWeights(opt_3_esg)
wgt_31_esg <- extractWeights(opt_31_esg)
wgt_32_esg <- extractWeights(opt_32_esg)

(cbind(rbind(wgt_3, wgt_31, wgt_32), rbind(wgt_3_esg, wgt_31_esg, wgt_32_esg)))

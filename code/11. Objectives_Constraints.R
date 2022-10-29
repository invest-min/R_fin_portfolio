## 포트폴리오 구성

library(PortfolioAnalytics)

pf_3 <- portfolio.spec(assets = colnames(rtn_3[, 1:3]))
pf_31 <- portfolio.spec(assets = colnames(rtn_31[, 1:3]))
pf_32 <- portfolio.spec(assets = colnames(rtn_32[, 1:3]))
pf_3_esg <- portfolio.spec(assets = colnames(rtn_3[, 4:6]))
pf_31_esg <- portfolio.spec(assets = colnames(rtn_31[, 4:6]))
pf_32_esg <- portfolio.spec(assets = colnames(rtn_32[, 4:6]))


## 제약조건

# 전체 포트폴리오 - full_investment: min_sum = max_sum = 1

pf_3 <- add.constraint(portfolio = pf_3,
                       type = "full_investment")
pf_31 <- add.constraint(portfolio = pf_31,
                        type = "full_investment")
pf_32 <- add.constraint(portfolio = pf_32,
                        type = "full_investment")
pf_3_esg <- add.constraint(portfolio = pf_3_esg,
                           type = "full_investment")
pf_31_esg <- add.constraint(portfolio = pf_31_esg,
                            type = "full_investment")
pf_32_esg <- add.constraint(portfolio = pf_32_esg,
                            type = "full_investment")

# 전체 포트폴리오 - dollar_neutral: min_sum = max_sum = 0

pf <- add.constraint(portfolio = pf,
                     type = "dollar_neutral")

# 전체 포트폴리오 - sum of the weights 수치로도 지정 가능

pf <- add.constraint(portfolio = pf,
                     type = "weight_sum",
                     min_sum = 0.5,
                     max_sum = 0.8)

# 개별 자산 - long_only: min = 0 and max = 1

pf_3 <- add.constraint(portfolio = pf_3,
                       type = "long_only")
pf_31 <- add.constraint(portfolio = pf_31,
                        type = "long_only")
pf_32 <- add.constraint(portfolio = pf_32,
                        type = "long_only")
pf_3_esg <- add.constraint(portfolio = pf_3_esg,
                           type = "long_only")
pf_31_esg <- add.constraint(portfolio = pf_31_esg,
                            type = "long_only")
pf_32_esg <- add.constraint(portfolio = pf_32_esg,
                            type = "long_only")

# 개별 자산 - box

pf <- add.constraint(portfolio = pf,
                     type = "box",
                     min = 0.2,
                     max = 0.7)

pf <- add.constraint(portfolio = pf,
                     type = "box",
                     min = c(0.1, 0.2, 0.3),
                     max = c(0.4, 0.5, 0.6))

# 개별 자산 - group (sector, region, asset class, etc.)

pf <- add.constraint(portfolio = pf,
                     type = "group",
                     groups = list(1:2, 3),
                     group_min = 0.4,
                     group_max = 0.6)

# 제약조건으로 목표수익률을 줄 수도 있음

pf_3
pf_31
pf_32
pf_3_esg
pf_31_esg
pf_32_esg


## 목적함수

# 최대수익률

pf_3 <- add.objective(portfolio = pf_3,
                      type = "return",
                      name = "mean")
pf_31 <- add.objective(portfolio = pf_31,
                       type = "return",
                       name = "mean")
pf_32 <- add.objective(portfolio = pf_32,
                       type = "return",
                       name = "mean")
pf_3_esg <- add.objective(portfolio = pf_3_esg,
                          type = "return",
                          name = "mean")
pf_31_esg <- add.objective(portfolio = pf_31_esg,
                           type = "return",
                           name = "mean")
pf_32_esg <- add.objective(portfolio = pf_32_esg,
                           type = "return",
                           name = "mean")

# 최소위험

pf_3 <- add.objective(portfolio = pf_3,
                      type = "risk",
                      name = "StdDev")
pf_31 <- add.objective(portfolio = pf_31,
                       type = "risk",
                       name = "StdDev")
pf_32 <- add.objective(portfolio = pf_32,
                       type = "risk",
                       name = "StdDev")
pf_3_esg <- add.objective(portfolio = pf_3_esg,
                          type = "risk",
                          name = "StdDev")
pf_31_esg <- add.objective(portfolio = pf_31_esg,
                           type = "risk",
                           name = "StdDev")
pf_32_esg <- add.objective(portfolio = pf_32_esg,
                           type = "risk",
                           name = "StdDev")

# risk_budget

pf <- add.objective(portfolio = pf,
                    type = "risk_budget",
                    name = "StdDev",
                    min_prisk = 0.05,
                    max_prisk = 0.1)

pf_3
pf_31
pf_32
pf_3_esg
pf_31_esg
pf_32_esg

## 포트폴리오 구성

library(PortfolioAnalytics)

pf <- portfolio.spec(assets = colnames(rtn))

## 제약조건

# 전체 포트폴리오 - full_investment: min_sum = max_sum = 1

pf <- add.constraint(portfolio = pf,
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

pf <- add.constraint(portfolio = pf,
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

pf


## 목적함수

# 최대수익률

pf <- add.objective(portfolio = pf,
                    type = "return",
                    name = "mean")

# 최소위험

pf <- add.objective(portfolio = pf,
                    type = "risk",
                    name = "StdDev")

# risk_budget

pf <- add.objective(portfolio = pf,
                    type = "risk_budget",
                    name = "StdDev",
                    min_prisk = 0.05,
                    max_prisk = 0.1)

pf

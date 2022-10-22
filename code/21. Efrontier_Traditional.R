## Efficient frontiers can be plotted two ways
# 1. Run optimize.portfolio with trace=TRUE and then chart that object
# 2. create an efficient frontier and then chart that object

## 포트폴리오 생성 - mean-es, mean-var

library(DEoptim)

funds <- colnames(rtn_sim)
port_funds <- portfolio.spec(funds)

port_funds <- add.constraint(port_funds, type = "full_investment")
port_funds <- add.constraint(port_funds, type = "box", min = 0.15, max = 0.45)
port_funds <- add.constraint(port_funds, type = "group",
                             groups = list(c(1, 2), c(3)),
                             group_min = 0.05,
                             group_max = 0.7)

port_funds_es <- add.objective(port_funds, type = "risk", name = "ES")
port_funds_es <- add.objective(port_funds_es, type = "return", name = "mean")

port_funds_var <- add.objective(port_funds,
                                type = "risk", name = "var", risk_aversion = 10)
port_funds_var <- add.objective(port_funds_var, type = "return", name = "mean")


## Efficient Frontier

# 1. Run optimize.portfolio with trace=TRUE and then chart that object

opt_var <- optimize.portfolio(R = rtn_sim,
                              portfolio = port_funds_var,
                              optimize_method = "ROI",
                              trace = TRUE)

chart.EfficientFrontier(opt_var,
                        match.col = "StdDev",
                        n.portfolios = 25,
                        type = "l")

port_funds_var$objectives[[2]]$risk_aversion = 0.25 # 위험회피성향 수정

opt_var <- optimize.portfolio(R = rtn_sim,
                              portfolio = port_funds_var,
                              optimize_method = "ROI",
                              trace = TRUE)

chart.EfficientFrontier(opt_var,
                        match.col = "StdDev",
                        n.portfolios = 25,
                        type = "l")

chart.EF.Weights(opt_var, match.col = "StdDev")
chart.EF.Weights(opt_var, match.col = "StdDev", by.groups = TRUE)

ef <- extractEfficientFrontier(object = opt_var,
                               match.col = "StdDev",
                               n.portfolios = 15)
ef
summary(ef, digits=5)

chart.EF.Weights(ef, match.col = "StdDev", colorset = bluemono)
chart.EF.Weights(ef, match.col = "StdDev", colorset = bluemono, by.groups = TRUE)


opt_es <- optimize.portfolio(R = rtn_sim,
                             portfolio = port_funds_es,
                             optimize_method = "random",
                             search_size = 2000,
                             trace=TRUE)

chart.EfficientFrontier(opt_es,
                        match.col = "ES",
                        main = "mean-ETL RP Efficient Frontier",
                        type = "l",
                        col = "blue",
                        rf = 0,
                        RAR.text = "STARR")

# 2. create an efficient frontier and then chart that object

ef_var <- create.EfficientFrontier(R = rtn_sim,
                                   portfolio = port_funds,
                                   type = "mean-StdDev")
ef_var
summary(ef_var, digits = 2)
ef_var$frontier

chart.EfficientFrontier(ef_var,
                        match.col = "StdDev",
                        type = "l",
                        RAR.text = "Sharpe Ratio", # 디폴트는 Modified Sharpe Ratio
                        pch = 4)

chart.EfficientFrontier(ef_var,
                        match.col = "StdDev",
                        type = "l",
                        tangent.line = FALSE) # tangency line 제거

chart.EfficientFrontier(ef_var,
                        match.col = "StdDev",
                        type = "b",
                        rf = NULL) # tangency portfolio and line, SR, rf 제거

chart.EfficientFrontier(ef_var,
                        match.col = "StdDev",
                        type = "l", 
                        tangent.line = FALSE,
                        chart.assets = FALSE) # assets 제거

chart.EfficientFrontier(ef_var,
                        match.col = "StdDev",
                        type = "l", 
                        tangent.line = FALSE,
                        labels.assets = FALSE, # assets 이름만 제거
                        pch.assets = 1) # 점 모양 변경

chart.EF.Weights(ef_var,
                 colorset = bluemono,
                 match.col = "StdDev")

chart.EF.Weights(ef_var,
                 colorset = bluemono,
                 by.groups = TRUE,
                 match.col = "StdDev")

chart.EF.Weights(ef_var,
                 colorset = bluemono,
                 match.col = "StdDev",
                 main = "",
                 cex.lab = 1) # default is cex.lab=0.8


ef_es <- create.EfficientFrontier(R = rtn_sim,
                                  portfolio = port_funds,
                                  type="mean-ES")
ef_es
summary(ef_es)
ef_es$frontier

chart.EfficientFrontier(ef_es,
                        match.col = "ES",
                        main = "mean-ETL Efficient Frontier",
                        type = "l",
                        col = "blue",
                        RAR.text = "STARR")

chart.EF.Weights(ef_es, colorset = bluemono, match.col = "ES")
chart.EF.Weights(ef_es, by.groups = TRUE, colorset = bluemono, match.col = "ES")


ef_es_rp <- create.EfficientFrontier(R = rtn_sim,
                                     portfolio = port_funds_es,
                                     type="random",
                                     match.col="ES")

chart.EfficientFrontier(ef_es_rp,
                        match.col = "ES",
                        main = "mean-ETL RP Efficient Frontier",
                        type = "l",
                        col = "blue",
                        rf=0)

chart.EF.Weights(ef_es_rp, colorset = bluemono, match.col = "ES")


## Overlay efficient frontiers of multiple portfolios

# 그림 겹치기

port_over <- portfolio.spec(assets = funds)
port_over <- add.constraint(portfolio = port_over, type = "full_investment")

port_long <- add.constraint(portfolio = port_over, type = "long_only")

port_box <- add.constraint(portfolio = port_over, type = "box",
                           min = 0.05, max = 0.65)

port_group <- add.constraint(portfolio = port_over,
                             type = "group", 
                             groups = list(groupA=c(1, 2),
                                           groupB=c(3)),
                             group_min = c(0.25, 0.15), 
                             group_max = c(0.75, 0.55))
port_group <- add.constraint(portfolio = port_group, type="long_only")

port_list <- combine.portfolios(list(port_long, port_box, port_group))
legend.labels <- c("Long Only", "Box", "Group + Long Only")

chart.EfficientFrontierOverlay(R = rtn_sim,
                               portfolio_list = port_list,
                               type = "mean-StdDev",
                               match.col = "StdDev",
                               legend.loc = "topleft", 
                               legend.labels = legend.labels,
                               cex.legend = 0.6,
                               labels.assets = FALSE,
                               pch.assets = 18)

# Efficient frontier with varying confidence levels for ES calculation

ES90 <- add.objective(portfolio = port_long,
                      type = "risk",
                      name = "ES", 
                      arguments = list(p = 0.9))

ES92 <- add.objective(portfolio = port_long,
                      type = "risk",
                      name = "ES", 
                      arguments = list(p = 0.92))

ES95 <- add.objective(portfolio = port_long,
                      type = "risk",
                      name = "ES", 
                      arguments = list(p = 0.95))

port_list <- combine.portfolios(list(ES.90 = ES90, ES.92 = ES92, ES.95 = ES95))
legend.labels <- c("ES (p=0.9)", "ES (p=0.92)", "ES (p=0.95)")

chart.EfficientFrontierOverlay(R = rtn_sim,
                               portfolio_list = port_list,
                               type = "mean-ES", 
                               match.col = "ES",
                               legend.loc = "topleft", 
                               legend.labels = legend.labels,
                               cex.legend = 0.6,
                               labels.assets = FALSE,
                               pch.assets = 18)

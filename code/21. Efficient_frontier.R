## Efficient frontiers can be plotted two ways
# 1. Run optimize.portfolio with trace=TRUE and then chart that object
# 2. create an efficient frontier and then chart that object


## 1. Run optimize.portfolio with trace=TRUE and then chart that object

chart.EfficientFrontier(opt_3,
                        match.col = "StdDev",
                        n.portfolios = 100,
                        type = "p")

chart.EfficientFrontier(opt_3,
                        match.col = "StdDev",
                        type = "l")

chart.EF.Weights(opt_3, match.col = "StdDev")

ef_3 <- extractEfficientFrontier(object = opt_3,
                                 match.col = "StdDev",
                                 n.portfolios = 15)
ef_3
summary(ef_3, digits=5)
chart.EF.Weights(ef_3, match.col = "StdDev", colorset = bluemono)


chart.EfficientFrontier(opt_31,
                        match.col = "StdDev",
                        type = "l")

chart.EF.Weights(opt_31, match.col = "StdDev")

ef_31 <- extractEfficientFrontier(object = opt_31,
                                  match.col = "StdDev",
                                  n.portfolios = 15)
chart.EF.Weights(ef_31, match.col = "StdDev", colorset = bluemono)


chart.EfficientFrontier(opt_32,
                        match.col = "StdDev",
                        type = "l")

chart.EF.Weights(opt_32, match.col = "StdDev")

ef_32 <- extractEfficientFrontier(object = opt_32,
                                  match.col = "StdDev",
                                  n.portfolios = 15)
chart.EF.Weights(ef_32, match.col = "StdDev", colorset = bluemono)


chart.EfficientFrontier(opt_3_esg,
                        match.col = "StdDev",
                        type = "l")

chart.EF.Weights(opt_3_esg, match.col = "StdDev")

ef_3_esg <- extractEfficientFrontier(object = opt_3_esg,
                                  match.col = "StdDev",
                                  n.portfolios = 15)
chart.EF.Weights(ef_3_esg, match.col = "StdDev", colorset = bluemono)


chart.EfficientFrontier(opt_31_esg,
                        match.col = "StdDev",
                        type = "l")

chart.EF.Weights(opt_31_esg, match.col = "StdDev")

ef_31_esg <- extractEfficientFrontier(object = opt_31_esg,
                                  match.col = "StdDev",
                                  n.portfolios = 15)
chart.EF.Weights(ef_31_esg, match.col = "StdDev", colorset = bluemono)


chart.EfficientFrontier(opt_32_esg,
                        match.col = "StdDev",
                        type = "l")

chart.EF.Weights(opt_32_esg, match.col = "StdDev")

ef_32_esg <- extractEfficientFrontier(object = opt_32_esg,
                                  match.col = "StdDev",
                                  n.portfolios = 15)
chart.EF.Weights(ef_32_esg, match.col = "StdDev", colorset = bluemono)


## 2. create an efficient frontier and then chart that object

ef_3 <- create.EfficientFrontier(rtn_3,
                                 portfolio = pf_3,
                                 type = "mean-StdDev")
ef_3
summary(ef_3, digits = 2)
ef_3$frontier

chart.EfficientFrontier(ef_3,
                        match.col = "StdDev",
                        type = "l",
                        RAR.text = "Sharpe Ratio", # 디폴트는 Modified Sharpe Ratio
                        pch = 4)

chart.EfficientFrontier(ef_3,
                        match.col = "StdDev",
                        type = "l",
                        tangent.line = FALSE) # tangency line 제거

chart.EfficientFrontier(ef_3,
                        match.col = "StdDev",
                        type = "b",
                        rf = NULL) # tangency portfolio and line, SR, rf 제거

chart.EfficientFrontier(ef_3,
                        match.col = "StdDev",
                        type = "l", 
                        tangent.line = FALSE,
                        chart.assets = FALSE) # assets 제거

chart.EfficientFrontier(ef_3,
                        match.col = "StdDev",
                        type = "l", 
                        tangent.line = FALSE,
                        labels.assets = FALSE, # assets 이름만 제거
                        pch.assets = 1) # 점 모양 변경

chart.EF.Weights(ef_3,
                 colorset = bluemono,
                 match.col = "StdDev")

chart.EF.Weights(ef_3,
                 colorset = bluemono,
                 match.col = "StdDev",
                 main = "",
                 cex.lab = 1) # default is cex.lab=0.8


## Overlay efficient frontiers of multiple portfolios

# 그림 겹치기

pf_list <- combine.portfolios(list(pf_3, pf_3_esg))
legend.labels <- c("normal", "esg")

chart.EfficientFrontierOverlay(rtn_3,
                               portfolio_list = pf_list,
                               type = "mean-StdDev",
                               match.col = "StdDev",
                               legend.loc = "topleft", 
                               legend.labels = legend.labels,
                               cex.legend = 0.6,
                               labels.assets = FALSE,
                               pch.assets = 18)

# Efficient frontier with varying confidence levels for ES calculation

ES90 <- add.objective(portfolio = pf,
                      type = "risk",
                      name = "ES", 
                      arguments = list(p = 0.9))

ES92 <- add.objective(portfolio = pf,
                      type = "risk",
                      name = "ES", 
                      arguments = list(p = 0.92))

ES95 <- add.objective(portfolio = pf,
                      type = "risk",
                      name = "ES", 
                      arguments = list(p = 0.95))

pf_list <- combine.portfolios(list(ES.90 = ES90, ES.92 = ES92, ES.95 = ES95))
legend.labels <- c("ES (p=0.9)", "ES (p=0.92)", "ES (p=0.95)")

chart.EfficientFrontierOverlay(rtn,
                               portfolio_list = pf_list,
                               type = "mean-ES", 
                               match.col = "ES",
                               legend.loc = "topleft", 
                               legend.labels = legend.labels,
                               cex.legend = 0.6,
                               labels.assets = FALSE,
                               pch.assets = 18)

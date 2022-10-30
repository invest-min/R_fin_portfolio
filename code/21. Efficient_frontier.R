## Efficient frontiers can be plotted two ways
# 1. Run optimize.portfolio with trace=TRUE and then chart that object
# 2. create an efficient frontier and then chart that object


# 1. Run optimize.portfolio with trace=TRUE and then chart that object

chart.EfficientFrontier(opt_roi,
                        match.col = "StdDev",
                        n.portfolios = 100,
                        type = "p")

chart.EfficientFrontier(opt_roi,
                        match.col = "StdDev",
                        type = "l")

chart.EF.Weights(opt_roi, match.col = "StdDev")

ef_roi <- extractEfficientFrontier(object = opt_roi,
                                   match.col = "StdDev",
                                   n.portfolios = 15)
ef_roi
summary(ef_roi, digits=5)
chart.EF.Weights(ef_roi, match.col = "StdDev", colorset = bluemono)


chart.EfficientFrontier(opt_pso,
                        match.col = "StdDev",
                        n.portfolios = 100,
                        type = "p")

chart.EfficientFrontier(opt_pso,
                        match.col = "StdDev",
                        type = "l")

chart.EF.Weights(opt_pso, match.col = "StdDev")

ef_pso <- extractEfficientFrontier(object = opt_pso,
                                   match.col = "StdDev",
                                   n.portfolios = 15)
ef_pso
summary(ef_pso, digits=5)
chart.EF.Weights(ef_pso, match.col = "StdDev", colorset = bluemono)


# 2. create an efficient frontier and then chart that object

ef <- create.EfficientFrontier(rtn,
                               portfolio = pf,
                               type = "mean-StdDev")
ef
summary(ef, digits = 2)
ef$frontier

chart.EfficientFrontier(ef,
                        match.col = "StdDev",
                        type = "l",
                        RAR.text = "Sharpe Ratio", # 디폴트는 Modified Sharpe Ratio
                        pch = 4)

chart.EfficientFrontier(ef,
                        match.col = "StdDev",
                        type = "l",
                        tangent.line = FALSE) # tangency line 제거

chart.EfficientFrontier(ef,
                        match.col = "StdDev",
                        type = "b",
                        rf = NULL) # tangency portfolio and line, SR, rf 제거

chart.EfficientFrontier(ef,
                        match.col = "StdDev",
                        type = "l", 
                        tangent.line = FALSE,
                        chart.assets = FALSE) # assets 제거

chart.EfficientFrontier(ef,
                        match.col = "StdDev",
                        type = "l", 
                        tangent.line = FALSE,
                        labels.assets = FALSE, # assets 이름만 제거
                        pch.assets = 1) # 점 모양 변경

chart.EF.Weights(ef,
                 colorset = bluemono,
                 match.col = "StdDev")

chart.EF.Weights(ef,
                 colorset = bluemono,
                 match.col = "StdDev",
                 main = "",
                 cex.lab = 1) # default is cex.lab=0.8


## Overlay efficient frontiers of multiple portfolios

# 여러 포트폴리오 겹치기

pf_list <- combine.portfolios(list(pf_1, pf_2))
legend.labels <- c("portfolio 1", "portfolio 2")

chart.EfficientFrontierOverlay(rtn,
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

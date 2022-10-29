## Return

library(quantmod)

# 한 자산씩

rtn_3 <- periodReturn(idx_3,
         period = "daily", #weekly, monthly, quarterly, yearly
         type = "arithmetic") #arithmetic(discrete) or log(continuous)
head(rtn_3) #한 열로 합쳐짐. periodReturn은 한 자산씩 계산해야 함

rtn_stock <- periodReturn(idx_3$stock,
                          period = "daily",
                          type = "arithmetic")[-1]
colnames(rtn_stock) = "stock"
head(rtn_stock)
barChart(rtn_stock)

rtn_stock <- dailyReturn(idx_3$stock, #함수명에 daily 등 가능
                         type = "arithmetic")[-1]
colnames(rtn_stock) = "stock"
head(rtn_stock)
barChart(rtn_stock)

# 여러 자산 한 번에

rtn_3 <- ROC(idx_3, #여러 자산 한 번에 계산 가능
             type = "discrete")[-1,] #type: continuous / discrete
head(rtn_3)
barChart(rtn_3$stock)

library(PerformanceAnalytics)

rtn_3 <- Return.calculate(idx_3, #여러 변수 한 번에 계산 가능
                          method = "discrete")[-1] #discrete/log/difference
head(rtn_3)
summary(rtn_3)

rtn_3_annual <- Return.annualized(rtn_3, scale = 252)
barplot(rtn_3_annual)

rm(rtn_stock)

# 수익률 상관관계

cor(rtn_3)
corrgram(as.data.frame(rtn_3))

scatter.smooth(idx_3$stock, idx_3$bond)
scatter.smooth(idx_3$stock, idx_3$reit)
scatter.smooth(idx_3$reit, idx_3$bond)


## Risk

# 총위험

var(rtn_3)
cov(rtn_3)
var(rtn_3$stock)
var(rtn_3$stock)^(1/2)

library(PerformanceAnalytics)

StdDev(rtn_3)
barplot(StdDev(rtn_3))

StdDev.annualized(rtn_3)
barplot(StdDev.annualized(rtn_3))

# 하방위험

SemiVariance(rtn_3)
barplot(SemiVariance(rtn_3))

SemiDeviation(rtn_3)
barplot(SemiDeviation(rtn_3))

VaR(R = rtn_3, p = .95, method = "historical") # Historical VaR (quantile)
barplot(-VaR(R = rtn_3, p = .95, method = "historical"))

VaR(R = rtn_3, p = .95, method = "gaussian") # Parametric Mean VaR (m1, m2)
barplot(-VaR(R = rtn_3, p = .95, method = "gaussian"))

VaR(R = rtn_3, p = .95, method = "modified") # Modified Cornish-Fisher VaR (m3, m4)
barplot(-VaR(R = rtn_3, p = .95, method = "modified"))

CVaR(R = rtn_3, p = .95, method = "historical")
barplot(-CVaR(R = rtn_3, p = .95, method = "historical"))

CVaR(R = rtn_3, p = .95, method = "gaussian")
barplot(-CVaR(R = rtn_3, p = .95, method = "gaussian"))

CVaR(R = rtn_3, p = .95, method = "modified")
barplot(-CVaR(R = rtn_3, p = .95, method = "modified"))


## Risk-Return

cvar_3 <- -CVaR(R = rtn_3, p = .95, method = "historical")
(risk_return_3 <- t(rbind(cvar_3, rtn_3_annual)))

plot(risk_return_3)
text(risk_return_3, labels = row.names(risk_return_3), pos = 4)

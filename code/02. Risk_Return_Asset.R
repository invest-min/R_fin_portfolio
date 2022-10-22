## Return

library(quantmod)

rtn_daily <- periodReturn(idx_daily,
             period = "daily", #weekly, monthly, quarterly, yearly
             type = "arithmetic") #arithmetic(discrete) or log(continuous)
head(rtn_daily) #한 열로 합쳐짐. periodReturn은 한 자산씩 계산해야 함

rtn_stock <- periodReturn(idx_daily$stock,
                          period = "daily",
                          type = "arithmetic")[-1]
colnames(rtn_stock) = "stock"
head(rtn_stock)
barChart(rtn_stock)

rtn_stock <- dailyReturn(idx_daily$stock, #함수명에 daily 등 가능
                         type = "arithmetic")[-1]
colnames(rtn_stock) = "stock"
head(rtn_stock)
barChart(rtn_stock)


rtn_daily <- ROC(idx_daily, #여러 변수 한 번에 계산 가능
                 type = "discrete")[-1,] #type: continuous / discrete
head(rtn_daily)
barChart(rtn_daily$stock)


library(PerformanceAnalytics)

rtn_weekly <- Return.calculate(idx_weekly, #여러 변수 한 번에 계산 가능
                               method = "discrete")[-1] #discrete/log/difference
head(rtn_weekly)
barChart(rtn_weekly$stock)

rm(rtn_stock)

## Risk

library(PerformanceAnalytics)

VaR(R = rtn_daily, p = .95, method = "historical") # Historical VaR
VaR(R = rtn_daily, p = .95, method = "gaussian") # Parametric Mean VaR
VaR(R = rtn_daily, p = .95, method = "modified") # Modified Cornish-Fisher VaR

CVaR(R = rtn_daily, p = .95, method = "historical")
CVaR(R = rtn_daily, p = .95, method = "gaussian")
CVaR(R = rtn_daily, p = .95, method = "modified")

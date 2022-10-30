## Return

# 한 자산씩

library(quantmod)

rtn <- periodReturn(idx,
       period = "daily", #weekly, monthly, quarterly, yearly
       type = "arithmetic") #arithmetic(discrete) or log(continuous)
head(rtn) #한 열로 합쳐짐. periodReturn은 한 자산씩 계산해야 함

rtn_stock <- periodReturn(idx$stock,
                          period = "daily",
                          type = "arithmetic")[-1] #첫날 제거
colnames(rtn_stock) = "stock"
head(rtn_stock)
barChart(rtn_stock, up.col = "black", dn.col = "black", theme = "white")

rtn_stock <- dailyReturn(idx$stock, #함수명에 daily 등 가능
                         type = "arithmetic")[-1]
colnames(rtn_stock) = "stock"
head(rtn_stock)
barChart(rtn_stock, up.col = "black", dn.col = "black", theme = "white")

# 여러 자산 한 번에

rtn <- ROC(idx, #여러 자산 한 번에 계산 가능
           type = "discrete")[-1,] #type: continuous / discrete
head(rtn)
barChart(rtn_stock, up.col = "black", dn.col = "black", theme = "white")

library(PerformanceAnalytics)

rtn <- Return.calculate(idx, #여러 변수 한 번에 계산 가능
                        method = "discrete")[-1] #discrete/log/difference
head(rtn)
(rtn_summary <- summary(rtn))
rtn_annual <- Return.annualized(rtn, scale = 252) #월은 scale = 12
barplot(rtn_annual)

rm(rtn_stock)

# 수익률 비교 및 상관관계

boxplot(rtn)
boxplot(rtn, ylim = c(-0.01, 0.01))

rtn_annual_long <- as.data.frame(rtn_annual)
rtn_annual_long <- rownames_to_column(rtn_annual_long, "period")
rtn_annual_long <- rtn_annual_long %>% pivot_longer(-period,
                                                    names_to="asset",
                                                    values_to="return")
rtn_annual_long


library(corrgram)
cor(rtn)
corrgram(as.data.frame(rtn))

scatter.smooth(rtn$stock, rtn$bond)
scatter.smooth(rtn$stock, rtn$reit)
scatter.smooth(rtn$reit, rtn$bond)


## Risk

# 총위험

var(rtn)
cov(rtn)
var(rtn$stock)
var(rtn$stock)^(1/2)

library(PerformanceAnalytics)

(rsk_std <- StdDev(rtn))
barplot(rsk_std)

StdDev.annualized(rtn)
barplot(StdDev.annualized(rtn))

# 하방위험

(rsk_smv <- SemiVariance(rtn))
barplot(rsk_smv)

SemiDeviation(rtn)
barplot(SemiDeviation(rtn))


(rsk_varh <- VaR(rtn, p = .95, method = "historical")) # Historical VaR
barplot(-rsk_varh)

(rsk_varg <- VaR(rtn, p = .95, method = "gaussian")) # Parametric Mean VaR
barplot(-rsk_varg)

(rsk_varm <- VaR(rtn, p = .95, method = "modified")) # Cornish-Fisher VaR
barplot(-rsk_varm)


(rsk_esh <- CVaR(rtn, p = .95, method = "historical"))
barplot(-rsk_esh)

(rsk_esg <- CVaR(rtn, p = .95, method = "gaussian"))
barplot(-rsk_esg)

(rsk_esm <- CVaR(rtn, p = .95, method = "modified"))
barplot(-rsk_esm)


# 위험 비교

rsk <- rbind(rsk_std, rsk_smv,
             rsk_varh, rsk_varg, rsk_varm,
             rsk_esh, rsk_esg, rsk_esm)
row.names(rsk) <- c("std", "smv", "VaRh", "VaRg", "VaRm", "ESh", "ESg", "ESm")
rsk


## Risk-Return

(rr_std <- t(rbind(rsk_std, rtn_annual)))
plot(rr_std)
text(rr_std, labels = row.names(rr_std), pos = 4)

(rr_var <- t(rbind(rsk_varm, rtn_annual)))
plot(rr_var)
text(rr_var, labels = row.names(rr_var), pos = 4)

(rr_es <- t(rbind(rsk_esm, rtn_annual)))
plot(rr_es)
text(rr_es, labels = row.names(rr_es), pos = 4)

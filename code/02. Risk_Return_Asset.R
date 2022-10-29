## Return

library(quantmod)

# 한 자산씩

rtn_3 <- periodReturn(idx_3,
         period = "daily", #weekly, monthly, quarterly, yearly
         type = "arithmetic") #arithmetic(discrete) or log(continuous)
head(rtn_3) #한 열로 합쳐짐. periodReturn은 한 자산씩 계산해야 함

rtn_stock <- periodReturn(idx_3$stock,
                          period = "daily",
                          type = "arithmetic")[-1] #첫날 제거
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
(rtn_3_summary <- summary(rtn_3))
rtn_3_annual <- Return.annualized(rtn_3, scale = 252) #월은 scale = 12
barplot(rtn_3_annual)

rtn_31 <- Return.calculate(idx_31,
                           method = "discrete")[-1]
head(rtn_31)
(rtn_31_summary <- summary(rtn_31))
rtn_31_annual <- Return.annualized(rtn_31, scale = 252)
barplot(rtn_31_annual)

rtn_32 <- Return.calculate(idx_32,
                           method = "discrete")[-1]
head(rtn_32)
(rtn_32_summary <- summary(rtn_32))
rtn_32_annual <- Return.annualized(rtn_32, scale = 252)
barplot(rtn_32_annual)

rm(rtn_stock)

# 수익률 비교 및 상관관계

boxplot(rtn_3)
boxplot(rtn_3, ylim = c(-0.01, 0.01))
boxplot(rtn_31, ylim = c(-0.01, 0.01))
boxplot(rtn_32, ylim = c(-0.01, 0.01))

rtn_annual <- rbind(rtn_3_annual, rtn_31_annual, rtn_32_annual)
row.names(rtn_annual) <- c("y16", "y13", "y46")
rtn_annual

rtn_annual_long <- as.data.frame(rtn_annual)
rtn_annual_long <- rownames_to_column(rtn_annual_long, "period")
rtn_annual_long <- rtn_annual_long %>% pivot_longer(-period,
                                                    names_to="asset",
                                                    values_to="return")
rtn_annual_long


library(corrgram)
cor(rtn_3)
corrgram(as.data.frame(rtn_3))
cor(rtn_31)
corrgram(as.data.frame(rtn_31))
cor(rtn_32)
corrgram(as.data.frame(rtn_32))

scatter.smooth(rtn_3$stock, rtn_3$bond)
scatter.smooth(rtn_3$stock, rtn_3$reit)
scatter.smooth(rtn_3$reit, rtn_3$bond)

scatter.smooth(rtn_31$stock, rtn_31$bond)
scatter.smooth(rtn_31$stock, rtn_31$reit)
scatter.smooth(rtn_31$reit, rtn_31$bond)

scatter.smooth(rtn_32$stock, rtn_32$bond)
scatter.smooth(rtn_32$stock, rtn_32$reit)
scatter.smooth(rtn_32$reit, rtn_32$bond)


## Risk

# 총위험

var(rtn_3)
cov(rtn_3)
var(rtn_3$stock)
var(rtn_3$stock)^(1/2)

library(PerformanceAnalytics)

(rsk_3_std <- StdDev(rtn_3))
barplot(rsk_3_std)

StdDev.annualized(rtn_3)
barplot(StdDev.annualized(rtn_3))

(rsk_31_std <- StdDev(rtn_31))
barplot(rsk_31_std)

(rsk_32_std <- StdDev(rtn_32))
barplot(rsk_32_std)

# 하방위험

(rsk_3_smv <- SemiVariance(rtn_3))
barplot(rsk_3_smv)

SemiDeviation(rtn_3)
barplot(SemiDeviation(rtn_3))

(rsk_31_smv <- SemiVariance(rtn_31))
barplot(rsk_31_smv)

(rsk_32_smv <- SemiVariance(rtn_32))
barplot(rsk_32_smv)


(rsk_3_varh <- VaR(rtn_3, p = .95, method = "historical")) # Historical VaR
barplot(-rsk_3_varh)

(rsk_3_varg <- VaR(rtn_3, p = .95, method = "gaussian")) # Parametric Mean VaR
barplot(-rsk_3_varg)

(rsk_3_varm <- VaR(rtn_3, p = .95, method = "modified")) # Cornish-Fisher VaR
barplot(-rsk_3_varm)

(rsk_31_varh <- VaR(rtn_31, p = .95, method = "historical"))
barplot(-rsk_31_varh)

(rsk_31_varg <- VaR(rtn_31, p = .95, method = "gaussian"))
barplot(-rsk_31_varg)

(rsk_31_varm <- VaR(rtn_31, p = .95, method = "modified"))
barplot(-rsk_31_varm)

(rsk_32_varh <- VaR(rtn_32, p = .95, method = "historical"))
barplot(-rsk_32_varh)

(rsk_32_varg <- VaR(rtn_32, p = .95, method = "gaussian"))
barplot(-rsk_32_varg)

(rsk_32_varm <- VaR(rtn_32, p = .95, method = "modified"))
barplot(-rsk_32_varm)


(rsk_3_esh <- CVaR(rtn_3, p = .95, method = "historical"))
barplot(-rsk_3_esh)

(rsk_3_esg <- CVaR(rtn_3, p = .95, method = "gaussian"))
barplot(-rsk_3_esg)

(rsk_3_esm <- CVaR(rtn_3, p = .95, method = "modified"))
barplot(-rsk_3_esm)

(rsk_31_esh <- CVaR(rtn_31, p = .95, method = "historical"))
barplot(-rsk_31_esh)

(rsk_31_esg <- CVaR(rtn_31, p = .95, method = "gaussian"))
barplot(-rsk_31_esg)

(rsk_31_esm <- CVaR(rtn_31, p = .95, method = "modified"))
barplot(-rsk_31_esm)

(rsk_32_esh <- CVaR(rtn_32, p = .95, method = "historical"))
barplot(-rsk_32_esh)

(rsk_32_esg <- CVaR(rtn_32, p = .95, method = "gaussian"))
barplot(-rsk_32_esg)

(rsk_32_esm <- CVaR(rtn_32, p = .95, method = "modified"))
barplot(-rsk_32_esm)

# 위험 비교

rsk_3 <- rbind(rsk_3_std, rsk_3_smv,
               rsk_3_varh, rsk_3_varg, rsk_3_varm,
               rsk_3_esh, rsk_3_esg, rsk_3_esm)
row.names(rsk_3) <- c("std", "smv", "VaRh", "VaRg", "VaRm", "ESh", "ESg", "ESm")
rsk_3

rsk_31 <- rbind(rsk_31_std, rsk_31_smv,
               rsk_31_varh, rsk_31_varg, rsk_31_varm,
               rsk_31_esh, rsk_31_esg, rsk_31_esm)
row.names(rsk_31) <- c("std", "smv", "VaRh", "VaRg", "VaRm", "ESh", "ESg", "ESm")
rsk_31

rsk_32 <- rbind(rsk_32_std, rsk_32_smv,
               rsk_32_varh, rsk_32_varg, rsk_32_varm,
               rsk_32_esh, rsk_32_esg, rsk_32_esm)
row.names(rsk_32) <- c("std", "smv", "VaRh", "VaRg", "VaRm", "ESh", "ESg", "ESm")
rsk_32


## Risk-Return

(rr_3_std <- t(rbind(rsk_3_std, rtn_3_annual)))
plot(rr_3_std)
text(rr_3_std, labels = row.names(rr_3_std), pos = 4)

(rr_31_std <- t(rbind(rsk_31_std, rtn_31_annual)))
plot(rr_31_std)
text(rr_31_std, labels = row.names(rr_31_std), pos = 4)

(rr_32_std <- t(rbind(rsk_32_std, rtn_32_annual)))
plot(rr_32_std)
text(rr_32_std, labels = row.names(rr_32_std), pos = 4)


(rr_3_var <- t(rbind(rsk_3_varm, rtn_3_annual)))
plot(rr_3_var)
text(rr_3_var, labels = row.names(rr_3_var), pos = 4)

(rr_31_var <- t(rbind(rsk_31_varm, rtn_31_annual)))
plot(rr_31_var)
text(rr_31_var, labels = row.names(rr_31_var), pos = 4)

(rr_32_var <- t(rbind(rsk_32_varm, rtn_32_annual)))
plot(rr_32_var)
text(rr_32_var, labels = row.names(rr_32_var), pos = 4)


(rr_3_es <- t(rbind(rsk_3_esm, rtn_3_annual)))
plot(rr_3_es)
text(rr_3_es, labels = row.names(rr_3_es), pos = 4)

(rr_31_es <- t(rbind(rsk_31_esm, rtn_31_annual)))
plot(rr_31_es)
text(rr_31_es, labels = row.names(rr_31_es), pos = 4)

(rr_32_es <- t(rbind(rsk_32_esm, rtn_32_annual)))
plot(rr_32_es)
text(rr_32_es, labels = row.names(rr_32_es), pos = 4)

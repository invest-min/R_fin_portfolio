## Return

# 자산비중과 무위험이자율 준비

library(PerformanceAnalytics)

w <- c(1/3, 1/3, 1/3) #자산별 투자비중

rf_3 <- na.omit(Ad(getSymbols("^IRX", #13 Week Treasury Bill 연률
                   auto.assign = F,
                   from = "2016/9/30",
                   to = "2022/10/1",
                   periodicity = "daily")))
rf_3 <- rf_3 / 100
colnames(rf_3) <- "y16"
head(rf_3)
tail(rf_3)
summary(rf_3)
(rf_3_average <- mean(rf_3$y16))

rf_31 <- rf_3[index(rf_3) <= "2019-09-30",]
colnames(rf_31) <- "y13"
head(rf_31)
tail(rf_31)
summary(rf_31)
(rf_31_average <- mean(rf_31$y13))

rf_32 <- rf_3[index(rf_3) > "2019-09-30",] #이자율이므로 9/30 제외
colnames(rf_32) <- "y46"
head(rf_32)
tail(rf_32)
summary(rf_32)
(rf_32_average <- mean(rf_32$y46))

# 수익률 계산

rtnp_3n <- Return.portfolio(rtn_3[, 1:3], w)
rtnp_3r <- Return.portfolio(rtn_3[, 1:3], w, rebalance_on = "months")
rtnp_3 <- cbind(rtnp_3n, rtnp_3r)
colnames(rtnp_3) <- c("y16", "y16_rb")
head(rtnp_3)
tail(rtnp_3)
(rtnp_3_annual <- table.AnnualizedReturns(rtnp_3, Rf = rf_3_average/252))

rtnp_31n <- Return.portfolio(rtn_31[, 1:3], w)
rtnp_31r <- Return.portfolio(rtn_31[, 1:3], w, rebalance_on = "months")
rtnp_31 <- cbind(rtnp_31n, rtnp_31r)
colnames(rtnp_31) <- c("y13", "y13_rb")
head(rtnp_31)
tail(rtnp_31)
(rtnp_31_annual <- table.AnnualizedReturns(rtnp_31, Rf = rf_31_average/252))

rtnp_32n <- Return.portfolio(rtn_32[, 1:3], w)
rtnp_32r <- Return.portfolio(rtn_32[, 1:3], w, rebalance_on = "months")
rtnp_32 <- cbind(rtnp_32n, rtnp_32r)
colnames(rtnp_32) <- c("y46", "y46_rb")
head(rtnp_32)
tail(rtnp_32)
(rtnp_32_annual <- table.AnnualizedReturns(rtnp_32, Rf = rf_32_average/252))

(rtnp_all <- cbind(rtnp_3_annual, rtnp_31_annual, rtnp_32_annual))


rtnp_3n_esg <- Return.portfolio(rtn_3[, 4:6], w)
rtnp_3r_esg <- Return.portfolio(rtn_3[, 4:6], w, rebalance_on = "months")
rtnp_3_esg <- cbind(rtnp_3n_esg, rtnp_3r_esg)
colnames(rtnp_3_esg) <- c("y16_esg", "y16_rb_esg")
head(rtnp_3_esg)
tail(rtnp_3_esg)
(rtnp_3_esg_annual <- table.AnnualizedReturns(rtnp_3_esg, Rf = rf_3_average/252))

rtnp_31n_esg <- Return.portfolio(rtn_31[, 4:6], w)
rtnp_31r_esg <- Return.portfolio(rtn_31[, 4:6], w, rebalance_on = "months")
rtnp_31_esg <- cbind(rtnp_31n_esg, rtnp_31r_esg)
colnames(rtnp_31_esg) <- c("y13_esg", "y13_rb_esg")
head(rtnp_31_esg)
tail(rtnp_31_esg)
(rtnp_31_esg_annual <- table.AnnualizedReturns(rtnp_31_esg, Rf = rf_31_average/252))

rtnp_32n_esg <- Return.portfolio(rtn_32[, 4:6], w)
rtnp_32r_esg <- Return.portfolio(rtn_32[, 4:6], w, rebalance_on = "months")
rtnp_32_esg <- cbind(rtnp_32n_esg, rtnp_32r_esg)
colnames(rtnp_32_esg) <- c("y46_esg", "y46_rb_esg")
head(rtnp_32_esg)
tail(rtnp_32_esg)
(rtnp_32_esg_annual <- table.AnnualizedReturns(rtnp_32_esg, Rf = rf_32_average/252))

(rtnp_all_esg <- cbind(rtnp_3_esg_annual, rtnp_31_esg_annual, rtnp_32_esg_annual))

(rtnp <- t(cbind(rtnp_all, rtnp_all_esg)))


## Risk

# 포트폴리오 내 개별 자산의 위험

VaR(rtn_3, p = .95, weights = NULL, # 개별 자산의 VaR 계산
    portfolio_method = "single",
    method = "modified")

VaR(rtn_31, p = .95, weights = NULL,
    portfolio_method = "single",
    method = "modified")

VaR(rtn_32, p = .95, weights = NULL,
    portfolio_method = "single",
    method = "modified")

# 포트폴리오 전체 위험과 개별 자산의 기여도

VaR(rtn_3[, 1:3], p = .95, weights = w,
    portfolio_method = "component",
    method = "modified")

VaR(rtn_31[, 1:3], p = .95, weights = w,
    portfolio_method = "component",
    method = "modified")

VaR(rtn_32[, 1:3], p = .95, weights = w,
    portfolio_method = "component",
    method = "modified")

VaR(rtn_3[, 1:3], p = .95, weights = w,
    portfolio_method = "marginal",
    method = "modified")

VaR(rtn_31[, 1:3], p = .95, weights = w,
    portfolio_method = "marginal",
    method = "modified")

VaR(rtn_32[, 1:3], p = .95, weights = w,
    portfolio_method = "marginal",
    method = "modified")


VaR(rtn_3[, 4:6], p = .95, weights = w,
    portfolio_method = "component",
    method = "modified")

VaR(rtn_31[, 4:6], p = .95, weights = w,
    portfolio_method = "component",
    method = "modified")

VaR(rtn_32[, 4:6], p = .95, weights = w,
    portfolio_method = "component",
    method = "modified")

VaR(rtn_3[, 4:6], p = .95, weights = w,
    portfolio_method = "marginal",
    method = "modified")

VaR(rtn_31[, 4:6], p = .95, weights = w,
    portfolio_method = "marginal",
    method = "modified")

VaR(rtn_32[, 4:6], p = .95, weights = w,
    portfolio_method = "marginal",
    method = "modified")


CVaR(rtn_3[, 1:3], p = .95, weights = w,
     portfolio_method = "component", #CVaR은 marginal 없음
     method = "modified")

ES(rtn_31[, 1:3], p = .95, weights = w,
   portfolio_method = "component",
   method = "modified")

ETL(rtn_32[, 1:3], p = .95, weights = w,
    portfolio_method = "component",
    method = "modified")


CVaR(rtn_3[, 4:6], p = .95, weights = w,
     portfolio_method = "component",
     method = "modified")

ES(rtn_31[, 4:6], p = .95, weights = w,
   portfolio_method = "component",
   method = "modified")

ETL(rtn_32[, 4:6], p = .95, weights = w,
    portfolio_method = "component",
    method = "modified")

# 포트폴리오 위험 비교

varp_3h <- VaR(rtn_3[, 1:3], p = .95, weights = w,
               portfolio_method = "component",
               method = "historical")$hVaR

varp_3g <- VaR(rtn_3[, 1:3], p = .95, weights = w,
               portfolio_method = "component",
               method = "gaussian")$VaR[1]

varp_3m <- VaR(rtn_3[, 1:3], p = .95, weights = w,
               portfolio_method = "component",
               method = "modified")$MVaR[1]

varp_3 <- abs(c(varp_3h, varp_3g, varp_3m))
names(varp_3) <- c("historical", "gaussian", "modified")
barplot(varp_3)

varp_31m <- VaR(rtn_31[, 1:3], p = .95, weights = w,
               portfolio_method = "component",
               method = "modified")$MVaR[1]

varp_32m <- VaR(rtn_32[, 1:3], p = .95, weights = w,
                portfolio_method = "component",
                method = "modified")$MVaR[1]

varp_m <- abs(c(varp_3m, varp_31m, varp_32m))
names(varp_m) <- c("y16", "y13", "y46")
barplot(varp_m)


varp_3h_esg <- VaR(rtn_3[, 4:6], p = .95, weights = w,
               portfolio_method = "component",
               method = "historical")$hVaR

varp_3g_esg <- VaR(rtn_3[, 4:6], p = .95, weights = w,
               portfolio_method = "component",
               method = "gaussian")$VaR[1]

varp_3m_esg <- VaR(rtn_3[, 4:6], p = .95, weights = w,
               portfolio_method = "component",
               method = "modified")$MVaR[1]

varp_3_esg <- abs(c(varp_3h_esg, varp_3g_esg, varp_3m_esg))
names(varp_3_esg) <- c("historical", "gaussian", "modified")
barplot(varp_3_esg)

varp_31m_esg <- VaR(rtn_31[, 4:6], p = .95, weights = w,
                portfolio_method = "component",
                method = "modified")$MVaR[1]

varp_32m_esg <- VaR(rtn_32[, 4:6], p = .95, weights = w,
                portfolio_method = "component",
                method = "modified")$MVaR[1]

varp_m_esg <- abs(c(varp_3m_esg, varp_31m_esg, varp_32m_esg))
names(varp_m_esg) <- c("y16", "y13", "y46")
barplot(varp_m_esg)

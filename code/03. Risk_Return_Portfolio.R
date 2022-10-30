## Return

# 자산비중과 무위험이자율 준비

library(PerformanceAnalytics)

w <- c(1/3, 1/3, 1/3) #자산별 투자비중

rf <- na.omit(Ad(getSymbols("^IRX", #13 Week Treasury Bill 연률
                 auto.assign = F,
                 from = "2016/9/30",
                 to = "2022/10/1",
                 periodicity = "daily")))
rf <- rf / 100
colnames(rf) <- "rf"
head(rf)
tail(rf)
summary(rf)
(rf_average <- mean(rf$rf))

# 수익률 계산

rtnp <- Return.portfolio(rtn, w)
colnames(rtnp) <- "rtnp"
head(rtnp)
tail(rtnp)
(rtnp_annual <- table.AnnualizedReturns(rtnp, Rf = rf_average/252))


## Risk

# 포트폴리오 내 개별 자산의 위험

VaR(rtn, p = .95, weights = NULL, # 개별 자산의 VaR 계산
    portfolio_method = "single",
    method = "modified")

# 포트폴리오 전체 위험과 개별 자산의 기여도

VaR(rtn, p = .95, weights = w,
    portfolio_method = "component",
    method = "modified")

VaR(rtn, p = .95, weights = w,
    portfolio_method = "marginal",
    method = "modified")


CVaR(rtn, p = .95, weights = w,
     portfolio_method = "component", #CVaR은 marginal 없음
     method = "modified")

ES(rtn, p = .95, weights = w,
   portfolio_method = "component",
   method = "modified")

ETL(rtn, p = .95, weights = w,
    portfolio_method = "component",
    method = "modified")

# 포트폴리오 위험 비교

varp_h <- VaR(rtn, p = .95, weights = w,
               portfolio_method = "component",
               method = "historical")$hVaR

varp_g <- VaR(rtn, p = .95, weights = w,
               portfolio_method = "component",
               method = "gaussian")$VaR[1]

varp_m <- VaR(rtn, p = .95, weights = w,
               portfolio_method = "component",
               method = "modified")$MVaR[1]

varp <- abs(c(varp_h, varp_g, varp_m))
names(varp) <- c("historical", "gaussian", "modified")
barplot(varp)

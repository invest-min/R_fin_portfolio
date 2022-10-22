## Return

rtn_sim <- cbind(rtn_daily$stock, rtn_daily$bond, rtn_daily$reit)
rtn_det <- cbind(rtn_daily$stock_ex, rtn_daily$stock_fn,
                     rtn_daily$bond_tr, rtn_daily$bond_co,
                     rtn_daily$reit)
rtn_esg <- cbind(rtn_daily$stock_esg, rtn_daily$bond_green, rtn_daily$reit_esg)
rtn_esg_sim <- cbind(rtn_sim, rtn_esg)
rtn_esg_det <- cbind(rtn_det, rtn_esg)

library(PerformanceAnalytics)

rtnp_sim <- Return.portfolio(rtn_sim, c(1/3, 1/3, 1/3))
rtnp_sim_reb <- Return.portfolio(rtn_sim, c(1/3, 1/3, 1/3),
                                 rebalance_on = "months")
rtnp_sim_all <- cbind(rtnp_sim, rtnp_sim_reb)
colnames(rtnp_sim_all) <- c("Non-Rebalanced", "Monthly Rebalanced")
table.AnnualizedReturns(rtnp_sim_all, Rf = 0.025/252)

rtnp_det <- Return.portfolio(rtn_det, c(1/5, 1/5, 1/5, 1/5, 1/5))
rtnp_esg <- Return.portfolio(rtn_esg, c(1/3, 1/3, 1/3))
rtnp_esg_sim <- Return.portfolio(rtn_esg_sim,
                                 c(1/6, 1/6, 1/6, 1/6, 1/6, 1/6))
rtnp_esg_det <- Return.portfolio(rtn_esg_det,
                                 c(1/8, 1/8, 1/8, 1/8, 1/8, 1/8, 1/8, 1/8))

rtnp_all <- cbind(rtnp_sim, rtnp_det, rtnp_esg, rtnp_esg_sim, rtnp_esg_det)
colnames(rtnp_all) <- c("simple", "detail", "esg", "esg_sim", "esg_det")
table.AnnualizedReturns(rtnp_all, Rf = 0.025/252)

rm(rtnp_sim_reb, rtnp_sim_all)


## Risk

# 개별 자산의 위험

var_sim_his <- VaR(rtn_sim, p = .95, weights = NULL, # 개별 자산의 VaR 계산
               portfolio_method = "single",
               method = "historical")

var_sim_gau <- VaR(rtn_sim, p = .95, weights = NULL,
               portfolio_method = "single",
               method = "gaussian")

var_sim_mod <- VaR(rtn_sim, p = .95, weights = NULL,
               portfolio_method = "single",
               method = "modified")

var_sim_all <- data.frame(rbind(var_sim_his, var_sim_gau, var_sim_mod))
row.names(var_sim_all) <- c("historical", "gaussian", "modified")
var_sim_all

# 포트폴리오의 위험과 개별 자산의 기여도

VaR(rtn_sim, p = .95, weights = c(1/3, 1/3, 1/3),
    portfolio_method = "component",
    method = "modified")

VaR(rtn_sim, p = .95, weights = c(1/3, 1/3, 1/3),
    portfolio_method = "marginal",
    method = "modified")


CVaR(rtn_sim, p = .95, weights = c(1/3, 1/3, 1/3), # CVaR = ES = ETS
     portfolio_method = "component",
     method = "modified")

ES(rtn_sim, p = .95, weights = c(1/3, 1/3, 1/3),
   portfolio_method = "component",
   method = "modified")

ETL(rtn_sim, p = .95, weights = c(1/3, 1/3, 1/3),
    portfolio_method = "component",
    method = "modified")

# 포트폴리오 위험의 시각화

varp_sim_his <- VaR(rtn_sim, p = .95, weights = c(1/3, 1/3, 1/3),
                portfolio_method = "component",
                method = "historical")$hVaR

varp_sim_gau <- VaR(rtn_sim, p = .95, weights = c(1/3, 1/3, 1/3),
                portfolio_method = "component",
                method = "gaussian")$VaR[1]

varp_sim_mod <- VaR(rtn_sim, p = .95, weights = c(1/3, 1/3, 1/3),
                portfolio_method = "component",
                method = "modified")$MVaR[1]

var_sim_all$portfolio <- c(varp_sim_his, varp_sim_gau, varp_sim_mod)

(var_sim_all <- abs(var_sim_all))

library(reshape2)

var_sim_all$type <- c("historical", "gaussian", "modified")
var_sim_plot <- melt(var_sim_all, variable.name = "ticker", value.name = "VaR")

library(ggplot2)

dev.off()
ggplot(var_sim_plot, aes(x = type, y = VaR, fill = ticker)) +
  geom_bar(stat = "identity", position = "dodge")

## Single Period

set.seed(1)

rp <- random_portfolios(portfolio = pf,
                        permutations = 500,
                        rp_method = "simplex") #method: simplex, sample, grid

opt_rp <- optimize.portfolio(rtn,
                             portfolio = pf,
                             optimize_method = "random",
                             rp = rp,
                             trace = TRUE)
extractWeights(opt_rp)
chart.Weights(opt_rp)
extractObjectiveMeasures(opt_rp)

rtn_rp <- Return.portfolio(rtn,
                           weight = extractWeights(opt_rp),
                           rebalance_on = "months")

table.AnnualizedReturns(rtn_rp, Rf = rf/252)


## Periodic Rebalancing

# search_size: how many portfolios to test (default 20,000)
# rebalance_on - frequency must be specified
# training_period - for the initial optimization
# rolling_window - for the window width (NULL: use all data available)

set.seed(1)

opt_rb <- optimize.portfolio.rebalancing(rtn,
                                         portfolio = pf,
                                         optimize_method = "random",
                                         rp = rp,
                                         trace = TRUE,
                                         search_size = 1000,
                                         rebalance_on = "quarters",
                                         training_period = 360,
                                         rolling_window = 360)
head(extractWeights(opt_rb))
chart.Weights(opt_rb)
head(extractObjectiveMeasures(opt_rb))

rtn_rb <- Return.portfolio(rtn,
                           weight = extractWeights(opt_rb),
                           rebalance_on = "months")

table.AnnualizedReturns(rtn_rb, Rf = rf/252)

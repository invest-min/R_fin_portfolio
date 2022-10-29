## Download

library(quantmod)

# yahoo에서 지수 및 종목 다운로드

getSymbols("^GSPC", #S&P 500. 지수 티커에는 "^"
           src="yahoo", #디폴트. 생략 가능
           from = "2016/9/30",
           to = "2022/10/1",
           periodicity = "daily") #daily, weekly(월), monthly(1일)

head(GSPC) #9/30부터. xts 형식 - 날짜가 rownames에 있음
tail(GSPC) #9/30까지

barChart(GSPC)
barChart(GSPC, multi.col = T, theme = "white")

candleChart(GSPC, multi.col = T, theme = "white")

chartSeries(GSPC) #자동으로 적합한 그래프
chartSeries(GSPC, multi.col = T, theme = "white")

addMACD()
addBBands()


getSymbols("AAPL",
           from = "2016/9/30",
           to = "2022/10/1",
           periodicity = "weekly")

getSymbols("GOOG",
           from = "2016/9/30",
           to = "2022/10/1",
           periodicity = "weekly")

two <- na.omit(merge(Ad(AAPL), Ad(GOOG))) #xts 형식 유지하여 합치고, NA 처리
head(two)
colnames(two) = c("AAPL", "GOOG")
head(two)
barChart(two$AAPL) #가격밖에 없어서 선그래프만
barChart(two$GOOG)

two <- data.frame(AAPL$AAPL.Adjusted, GOOG$GOOG.Adjusted) #df로 합치기도 가능
colnames(two) = c("AAPL", "GOOG")
head(two)
barChart(two$AAPL) #단, xts 형식 아니라서 실행 오류

two <- as.xts(two)
barChart(two$AAPL)
barChart(two$GOOG)

# yahoo에서 원하는 이름으로 다운로드

goog <- getSymbols("GOOG", # 객체명 정해주고,
                   auto.assign = F, # 자동으로 객체 만드는 것 끄기
                   from = "2016/9/30",
                   to = "2022/10/1",
                   periodicity = "daily")
head(goog)

goog <- Ad(getSymbols("GOOG", # Adjusted Closing Price만 다운로드
                      auto.assign = F,
                      from = "2016/9/30",
                      to = "2022/10/1",
                      periodicity = "daily"))
head(goog)

rm(GSPC, AAPL, GOOG, goog, two)


## Import

library(readxl)
idx_daily <- read_excel("data/S&P DJ Indices_tr.xlsx", sheet = "idx_daily")
head(idx_daily) #날짜가 변수로 포함

library(tidyverse)
idx_daily <- idx_daily %>% column_to_rownames("date") #날짜를 행이름으로
idx_daily <- as.xts(idx_daily)
head(idx_daily)

barChart(idx_daily$stock)
barChart(idx_daily$bond)
barChart(idx_daily$bond_tr)
barChart(idx_daily$bond_co)
barChart(idx_daily$reit)

idx_3 <- idx_daily[,c(1, 6, 12, 5, 10, 14)]
head(idx_3)

## Download

# yahoo에서 지수 및 종목 다운로드

library(quantmod)

getSymbols("^GSPC", #S&P 500. 지수 티커에는 "^"
           src="yahoo", #디폴트. 생략 가능
           from = "2016/9/30",
           to = "2022/10/1",
           periodicity = "daily") #daily, weekly(월), monthly(1일) 가능

head(GSPC) #9/30부터. xts 형식 - 날짜가 rownames에 있음
tail(GSPC) #9/30까지. 10/1 빠짐

barChart(GSPC)
barChart(GSPC, multi.col = T, theme = "white")

candleChart(GSPC, theme = "white")

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

# 불러와서 xts로

library(readxl)
idx_daily <- read_excel("data/S&P DJ Indices_tr.xlsx", sheet = "idx_daily")
head(idx_daily) #날짜가 변수로 포함

library(tidyverse)
idx_daily <- idx_daily %>% column_to_rownames("date") #날짜를 행이름으로
idx_daily <- as.xts(idx_daily)
head(idx_daily)

barChart(idx_daily$stock)

# xts 가공

idx <- idx_daily[,c(1, 6, 12)]
head(idx)
tail(idx)

coredata(idx) #xts 형식에서 시간 index 없앤 행렬
index(idx) #xts 형식에서 시간 index만 추출한 벡터


list.of.packages <- c("quantmod", 'magrittr', 'xts')
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

library(quantmod)
library(magrittr)
library(xts)

options('getSymbols.warning4.0' = FALSE)

to <- '2021-11-25'
from <- '1962-01-01'

stocks <- 'IBM'

# Загрузите цену закрытия (Close) акций IBM с 1962-01-01 по 2021-11-25 с помощью функции getSymbols 
asset <- Lo(get(getSymbols(stocks, from = from, to = to)))

# Загрузите дивиденды по этой акции за тот же промежуток времени getDividends
dividends <- getDividends(stocks, from = from, to = to)

# Сделайте эджастмент цены на дивиденды используя difference метод из лекции
adjusted_asset <- rowSums(na.fill(merge(asset, dividends, join = 'left'), 0))
plot(adjusted_asset - asset)


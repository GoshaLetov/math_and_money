library(quantmod)
library(magrittr)
library(xts)

options('getSymbols.warning4.0' = FALSE)

to <- '2021-11-25'
from <- '1962-01-01'

stocks <- 'IBM'

# Загрузите цену закрытия (Close) акций IBM с 1962-01-01 по 2021-11-25 с помощью функции getSymbols 
asset <- Cl(get(getSymbols(stocks, from = from, to = to)))

# Загрузите дивиденды по этой акции за тот же промежуток времени getDividends
dividends <- getDividends(stocks, from = from, to = to)

# Сделайте эджастмент цены на дивиденды используя difference метод из лекции
adjusted_asset <- asset - lag(sum(dividends) - cumsum(dividends), -1)
plot(adjusted_asset - asset)

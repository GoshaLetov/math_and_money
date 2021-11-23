####################################################################################
# HW 4
# Изменяя holding.period, periods (их количество и значения) и, возможно, меняя логику функции getWeights
# подберите стратегию, которая как и в предыдущем задании не будет зависеть от точки начала и в ней 
# учтены издержки в таком же размере. Вероятно, потребуется перебрать много варинтов параметров.
# Для того, чтобы ускорить работу программы можно, например, воспользоваться библиотекой Rcpp и переписать код на плюсы.
# https://teuder.github.io/rcpp4everyone_en/
# В качестве ответа должна быть функция с подобранными параметрами, считающая прибыль/убыток за каждый день.
# прибыль/убыток в течение лукбэка не учитываются в подсчете шарпа. 
# Для стратегии из предыдущего задания лукбэк равен max(periods) + holding.period - 1. 
# holding.period - 1 появилось из-за того, что в нашем портфеле есть стратегии с таким доп отступом.

# Оценка
# Удовлетворительно Шарп 0.8
# Хорошо Шарп 0.9
# Отлично Шарп 1

# Переопределяю функцию подбора весов
getWeights_2 <- function(data, periods) {
  
  backtest.data.by.stock <- lapply(1:dim(data)[2], function(stock) {
    stock.batch.data <- lapply(periods, function(period) {
      period.batch.data <- data[, stock]
      period.batch.lenght <- length(period.batch.data)
      period.batch <- (period.batch.data[period.batch.lenght] / period.batch.data[period.batch.lenght - period]) - 1
      return(period.batch)
    })
    stock.batch.data <- do.call(cbind, stock.batch.data)
    stock.batch <- apply(stock.batch.data, 1, mean)
    return(stock.batch)
  })
  
  backtest.data.by.stock <- do.call(cbind, backtest.data.by.stock)
  backtest.data.by.stock <- set_colnames(backtest.data.by.stock, stocks)
  
  if (length(backtest.data.by.stock[backtest.data.by.stock > 1.005]) > 0) {
    result <- ifelse(1:length(stocks) %in% which(backtest.data.by.stock > 1.005), 1, 0)
    result <- result / length(result[result == 1])
  } else {
    result <- ifelse(1:length(stocks) == which.min(backtest.data.by.stock), 1, 0)
  }
  
  return(result)
}

max.value <- floor(dim(backtest.data)[1] * 0.3)
repeat {
  
  holding.period.temp <- sample(1:100, 1)
  periods.temp <- sort(sample(1:max.value, sample(1:10, 1)))
  
  pnl.full <- base.strategy(periods = periods.temp, holding.period = holding.period.temp, use.new_fun = TRUE)
  
  loockback.indent <- max(periods.temp) + 2
  pnl.lookback <- pnl.full[loockback.indent:length(pnl.full)]
  
  sharpe <- sharpe_fun(pnl.lookback)
  print(paste(periods.temp, collapse = ', '))
  print(holding.period.temp)
  print(sharpe)
  if (sharpe >= 1 || is.na(sharpe)) {
    print(sprintf('Periods = %s, holding.period = %s', paste(periods.temp, collapse = ', '), holding.period.temp))
    print(sprintf('Sharpe = %s', sharpe))
    break
  }
}


# Ответ
{
  periods.best <- c(22, 589, 649, 693, 1048, 1436, 1660)
  holding.period.best <- 4
  
  pnl.best <- base.strategy(periods = periods.best, holding.period = holding.period.best, use.new_fun = TRUE)
  sharpe.best <- sharpe_fun(pnl.best[(max(periods.best) + 2):length(pnl.best)])
  
  nav.best <- portfolio.value(money, pnl.best)[(max(periods.best) + 2):length(pnl.best)]
  plot(nav.best, type = 'l')
  
  print(sprintf('Periods = %s, holding.period = %s', paste(periods.best, collapse = ', '), holding.period.best))
  print(sprintf('Sharpe = %s', sharpe.best))
  print(sprintf('Lookback = %s', round(1 - length(nav.best) / dim(backtest.data)[1], 2)))
}

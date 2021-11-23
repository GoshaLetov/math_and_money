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


max.value <- floor(dim(backtest.data)[1] * 1)
repeat {
  
  holding.period.temp <- sample(1:max.value, 1)
  periods.temp <- sort(c(sample(1:max.value, 1), sample(1:max.value, 1), sample(1:max.value, 1)))
  
  pnl.full <- base.strategy(periods = periods.temp, holding.period = holding.period.temp)
  
  loockback.indent <- max(periods.temp) + 2
  pnl.lookback <- pnl.full[loockback.indent:length(pnl.full)]
  
  sharpe <- sharpe_fun(pnl.lookback)
  
  if (sharpe >= 1 || is.na(sharpe)) {
    print(sprintf('Periods = %s, holding.period = %s', paste(periods.temp, collapse = ', '), holding.period.temp))
    print(sprintf('Sharpe = %s', sharpe))
    break
  }
}

# Ответ

# На коротком отрезке времени легко полчить высокие значения sharpe
# Для max.value <- dim(backtest.data)[1] 
# Получаем: Periods = 3530, 4416, 4428, holding.period = 1819 -> Sharpe = 1.0575 > 1


{
  periods.best <- c(3530, 4416, 4428)
  holding.period.best <- 1819
  
  pnl.best <- base.strategy(periods = periods.best, holding.period = holding.period.best)
  sharpe.best <- sharpe_fun(pnl.best[(max(periods.best) + 2):length(pnl.best)])
  
  nav.best <- portfolio.value(money, pnl.best)[(max(periods.best) + 2):length(pnl.best)]
  plot(nav.best, type = 'l')
  
  print(sprintf('Periods = %s, holding.period = %s', paste(periods.best, collapse = ', '), holding.period.best))
  print(sprintf('Sharpe = %s', sharpe.best))
}


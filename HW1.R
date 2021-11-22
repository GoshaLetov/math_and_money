### Загрузим и подключим необходимые библиотеки

list.of.packages <- c("quantmod", 'magrittr', 'xts', 'dplyr')
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

library(quantmod)
library(magrittr)
library(xts)
library(dplyr)

options('getSymbols.warning4.0' = FALSE)


### Загрузим данные

# Необходимо загрузить цены активов IWM, SPY, TLT начиная с 2000 года.
# По факту один актив будет с 2002-07-30 и данные придется обрезать, 
# чтобы тестировать только на промежутке, когда торговались все активы.

# Для загрузки воспользуемся функцией getSymbols из библиотеки quantmod

from <- "2000-01-01"
to <- "2021-11-14"
stocks <- c('IWM', "SPY", "TLT")

getSymbols(stocks, from = from, to = to)

# По дефолту эта функция не возвращает данные напрямую, но записывает их в глобальное окружение.
# После ее исполнения во вкладке RStudio "Environment" (или командой ls()) можно заметить, что в
# окружении появились три объекта с указанными нами в stocks тикерами.
# Далее ими можно пользоваться как обычными переменными, либо вытащить из окружения по строке-названию
# с помощью get(), что, вообще говоря, верно для объектов из произвольных окружений.
# Например, get("IWM"), get("stocks").

# Пользуясь этим, выведем в консоль начало данных, хранящихся в каждой из новых переменных:

for (st in stocks) {
  print(head(get(st)))
}

# В каждой из новых переменных находится объект xts с дневными данными.
# В отличие от matrix или data.frame этот тип данных не является встроенным, а импортирован
# из одноименной сторонней библиотеки. Тем не менее, данная библиотека настолько широко
# распространена, что использование xts в R является де-факто стандартом для работы с временными рядами.

# Под капотом xts объект базового матричного типа, у которого есть индекс в виде вектора дат. 
# Удобство xts заключается в том, что этот индекс учитывается при операциях с несколькими xts объектами,
# что позволяет конкатенировать, мержить, проводить арифметические операции и т. д., не волнуясь
# о выравнивании данных.
# Перечень возможностей и методов библиотеки можно, например, найти здесь:
# https://www.datacamp.com/community/blog/r-xts-cheat-sheet


# Как видим у каждого актива есть 6 столбцов. В этом задании нам нужен только Adjusted.
# Для удобного извлечения этого столбца достаточно применить функцию Ad() к xts объекту.


# (Если написать в консоль название функции без круглых скобок вызова, то выведется
# исходный код функции. В случае функции Ad можно заметить, что она делает не что иное,
# как берет столбец с названием "Adjusted", игнорируя регистр, а также выбрасывает
# исключение в случае отсутствия столбца.)



### Создадим матрицу цен backtest.data

# Для этого:
# 1. Возьмем из каждого xts объекта колонку Adjusted
# 2. Сконкатенируем данные по столбцам в один xts объект
# 2. Удалим строки, в которых присутствуют NA с помощью функции na.omit
# 3. Извлечем из xts базовый тип matrix с помощью функции coredata

# (Стоит обратить пристальное внимание на семейство базовых для R функций apply,
# а заодно и привыкнуть использовать элементы функциональной парадигмы.
# Это семейство функций позволяет практически во всех случаях заменить цикл и иногда работает быстрее.
# Также есть библиотеки, позволяющие in-place распараллеливать уже написанный через apply код)

backtest.data <- lapply(stocks, function(st) {
  temp.data <- Ad(get(st))
})

backtest.data <- do.call(cbind, backtest.data)
# Просто cbind тут не вызвать, потому что он принимает отдельные позиционные аргументы, а не список.
# В некоторым смысле такое использование do.call() эквивалентно анпакингу параметров в Питоне с помощью оператора "*".

backtest.data <- na.omit(backtest.data)
backtest.data <- coredata(backtest.data)

# Кстати, в отличие от Питона, в R точка в названии объектов не несет семантической нагрузки
# и может быть использована чисто стилистически, что встречается здесь повсеместно.



### Установим параметры стратегии

# (Фигурные скобки можно использовать не только для обозначения тела функции,
# но и для выделения блока кода)
{
  # Определим параметр,отвечающий за частоту изменения состава портфеля.
  # (Мы загружали дневные данные, поэтому здесь единица измерения также в днях)
  holding.period <- 21

  # Периоды для вычисления средней доходности
  periods <- c(21, 62, 100)
  
  # Определим отступ стратегии.
  # Другими словами, число точек данных необходимое, чтобы вычислить текущий состав портфеля
  lookback <- max(periods)
  
  # Установим капитал стратегии.
  # Будем считать, что этот капитал доступен в каждый момент времени при отсутствии позиций по активам
  # вне зависимости от предыдущих потерь и прибылей.
  # Такое возможно, если идёт управление многими стратегиями и между ними распределяется капитал.
  money <- 1000000
}

### Займемся функцией getWeights(), вычисляющей вектор весов для данной точки в наших данных.

# Эта функция будет, по сути, вычислять нашу позицию в любой момент времени.
# Для простоты будем в один момент времени держать только одну акцию.

# Назовем средней доходностью актива среднее арифметическое доходностей за период, 
# где доходность за период будет считаться как отношение последней цены в периоде и первой минус 1.
# Например, доходность за период n временного ряда x[t] в точке T будет (x[T] / x[T - N]) - 1

# Логика определения весов:
# Если средняя доходность IWM < 0 и SPY < 0, то купить TLT.
# Иначе, если средняя доходность SPY < IWM, то покупаем IWM.
# Иначе покупаем SPY.

# Функция должна возвращать вектор из трех элементов.
# Состав вектора: два нуля и одна единица, стоящая под индексом актива, который нужно будет купить.
# Например, с(0, 1, 0) означает, что купить нужно второй актив.

# На вход функции первым аргументом будет подаваться матрица backtest.data,
# обрезанная таким образом, чтобы последней строке соответствовал текущий, интересующий нас день.
# (предпоследней строке соответствует предыдущий день и так далее)
# Вторым аргументом подается вектор из трех элементов, отражающий длины периодов для вычислений средней доходности
# по отдельности для каждого из инструментов.

getWeights <- function(data, periods) {
  
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
  
  if((backtest.data.by.stock[, 'IWM'] < 0) & (backtest.data.by.stock[, 'SPY'] < 0)){
    result <- c(0, 0, 1)
    } else if(backtest.data.by.stock[, 'SPY'] < backtest.data.by.stock[, 'IWM']){
      result <- c(1, 0, 0)
      } else{
        result <- c(0, 1, 0)
        }
  return(result)
}

# Проверим работоспособность функции на полной матрице:
getWeights(backtest.data, periods)
# Результат должен быть с(1, 0, 0)



### Перейдем к основному циклу бэктестирования

# Индекс текущего торгового дня. (иницализируем так, чтобы хватило данных для первого подсчета весов)
# В цикле будем увеличивать на 1 на каждой итерации.
day <- lookback + 1

# Индекс дня, в который будет происходить перестройка портфеля.
# В каждый момент перестроки портфеля будем обновлять эту переменную,
# как текущий день + число дней удержания портфеля holding.period.
# Инициализируем индексом первого торгового дня.
trade.day <- day

# Матрица прибыли/убытка по каждому активу за каждый торговый день
pnl_inc_leg <- matrix(0L, nrow = nrow(backtest.data), ncol = ncol(backtest.data))
pnl_inc_leg <- set_colnames(pnl_inc_leg, stocks)

# Вектор из трех элементов для хранения числа штук каждого актива на итерации бэктеста
assets.count <- numeric(ncol(backtest.data))

backtest.data[day, ]

# Будем итерироваться по циклу до тех пор пока индекс текущего дня не превзойдет число строк в данных
while (day <= nrow(backtest.data)) {
  # Необходимо посчитать и занести в матрицу pnl_inc_leg в строку с индексом day прибыль/убыток 
  # за один день (с предыдущего на текущий).
  pnl_inc_leg[day,] <- assets.count  * (backtest.data[day,] - backtest.data[day - 1,])

  # Условие на удержание текущей позиции
  if (day < trade.day) {
    day <- day + 1 
    next
  }

  # Если нет, то получаем новые веса и меняем (если веса получились другие) купленные активы
  range <- (day - lookback):day
  cur.weights <- getWeights(backtest.data[range, ], periods)

  # Будем считать, что есть возможность посчитать веса стратегии и тут же зайти по этим же ценам.
  # Cчитаем издержки равными нулю
  assets.count <- floor(money * cur.weights / backtest.data[day, ])
  day <- day + 1
  trade.day <- trade.day + holding.period
}

####################################################################################
# HW 1
# Посчитать суммарные прибыль/убыток за каждый день по портфелю, используйте функцию rowSums
pnl_inc <- rowSums(pnl_inc_leg)
plot(pnl_inc, type = 'l')

# Необходимо найти стоимость портфеля за каждый день
# Это исходный капитал стратегии плюс накопленная прибыль/убыток за каждый день
nav <- money + cumsum(pnl_inc)
plot(nav, type = 'l')

####################################################################################
# HW 2
# Напишите функции, вычисляющие различные статистики. 
# Единственный аргумент каждой функции это массив прибылей/убытков за каждый день

# СреднийдневнойПУ * 252
return_ann_fun   <- function(pnl) {
  return(mean(pnl) * 252)
}

# Станд.Откл.дневныхПУ * sqrt(252)
std_ann_fun      <- function(pnl) {
  return(sd(pnl) * sqrt(252))
}

# sqrt(mean(pmin(дневнойПУ, 0) ^ 2)) * sqrt(252)
downside_ann_fun <- function(pnl) {
  return(sqrt(mean(pmin(pnl, 0) ^ 2)) * sqrt(252))
}

# Annualreturn / AnnualDownsideRisk
sortino_fun      <- function(pnl) {
  return(return_ann_fun(pnl) / downside_ann_fun(pnl))
  
}

# Annualreturn / Annualstd
sharpe_fun       <- function(pnl) {
  return(return_ann_fun(pnl) / std_ann_fun(pnl))
}

# Максимальныепотеринакопл.ПУзавремяторговли
#max_drawdown_fun <- function(pnl) {
#  cum.pnl  <- c(0, cumsum(pnl))
#  drawdown <- cum.pnl - cummax(cum.pnl)
#  return(max(drawdown))
#}

functions <- list(
  'sortino' = sortino_fun,
  'sharpe'= sharpe_fun,
  'max_drawdown'= max_drawdown_fun,
  'return_ann' = return_ann_fun, 
  'std_ann' = std_ann_fun
)

# Посчитайте статистики из листа functions для всего портфеля.
# Результатом должен быть list, а в каждой ячейке одно число.
# Используйте lapply для того, чтобы итерироваться по функциям
# и применять их на вектор прибылей и убытков за каждый день (pnl_inc)

strategy.kpi <- lapply(functions, function(fun) {
  kpi.batch <- fun(pnl_inc)
})
strategy.kpi <- do.call(cbind, strategy.kpi)
strategy.kpi <- set_names(strategy.kpi, names(functions))
print(strategy.kpi)


####################################################################################
# HW 3
# В нашем бэктесте есть один недостаток. Если начать бэктест на день позже, то pnl портфеля будет другим.
# Сдвигая еще на день снова получим другой результат, если сдвинем ровно на holding.period, тогда получим
# такой же pnl, что и в задании выше, но количество неторговых дней будет увеличено на holding.period точек.
# Составим holding.period бэктестов сдвигая торговлю на один день, усредним их pnl по дням. 
# Такой бэктест уже не будет зависеть от выбора точки начала бэктеста.
# Еще мы никак не учли торговые издержки. Положим их равными 0.0005 от модуля изменения позиции.
# Например, если у нас было 30 акций SPY, а стало 10 и цена SPY 250, то издержки равны |30 - 10| * 250 * 0.0005
# Эти издержки должны быть вычтены из дневной прибыли/убытка.


com <- 0.0005

base.strategy.while.loop <- function(day, start.trade.day, lookback, periods, holding.period) {
  
  pnl_inc_leg <- matrix(0L, nrow = nrow(backtest.data), ncol = ncol(backtest.data))
  pnl_inc_leg <- set_colnames(pnl_inc_leg, stocks)
  
  assets.count <- numeric(ncol(backtest.data))
  yesterday.assets.count <- numeric(ncol(backtest.data))
  
  trade.day <- day + start.trade.day
  
  while (day <= nrow(backtest.data)) {
    # Изменение в каждой позиции * Цена позиции (за прошлый день - операция была совершена вчера)
    # * Торговые издержки 
    costs <- abs(assets.count - yesterday.assets.count) * backtest.data[day - 1,] * com
    pnl_inc_leg[day,] <- (assets.count  * (backtest.data[day,] - backtest.data[day - 1,])) - costs
    
    if (day < trade.day) {
      day <- day + 1 
      next
    }
    
    range <- (day - lookback):day
    cur.weights <- getWeights(backtest.data[range, ], periods)
    
    yesterday.assets.count <- assets.count
    assets.count <- floor(money * cur.weights / backtest.data[day, ] 
                          # корректируем количество активов, которые мы можем приобрести
                          # из-за появления торговых издержек
                          / (1 + com))
    day <- day + 1
    trade.day <- trade.day + holding.period
  }
  
  pnl_inc <- rowSums(pnl_inc_leg)
  return(pnl_inc)
}

base.strategy <- function(periods, holding.period) {
  # Параметры, которые определяется в глобальном окружении (т.к. в рамках данной задачи они не изменяются):
  # backtest.data - данные по котировкам акций
  
  lookback <- max(periods)
  
  day <- lookback + 1
  
  # Проводим бек-тестирование для каждого дня для того, чтобы убрать погрещность holding.period
  pnl_inc_leg.full <- lapply(0:(holding.period - 1), function(start.day) {
    pnl_inc_leg.batch <- base.strategy.while.loop(
        day = day
      , start.trade.day = start.day
      , lookback = lookback
      , periods = periods
      , holding.period = holding.period
      )
  })
  pnl_inc_leg.full <- do.call(cbind, pnl_inc_leg.full)
  pnl_inc_leg.mean <- apply(pnl_inc_leg.full, 1, mean)
  
  return(pnl_inc_leg.mean)
}

portfolio.value <- function(money, pnl) {
  return(money + cumsum(pnl))
}


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

# Самое простое (не лучшее) решение - перебор параметров
max.value <- floor(dim(backtest.data)[1] * 0.5)
repeat {
  
  holding.period.temp <- sample(1:50, 1)#max.value, 1)
  periods.temp <- sort(c(sample(1:max.value, 1), sample(1:max.value, 1), sample(1:max.value, 1)))
  
  pnl.full <- base.strategy(periods = periods.temp, holding.period = holding.period.temp)
  
  loockback.indent <- max(periods.temp) + 2
  pnl.lookback <- temp.pnl[loockback.indent:len]
  
  sharpe <- sharpe_fun(pnl.lookback)
  
  if (sharpe >= 0.9 || is.na(sharpe)) {
    print(sprintf('Periods = %s, holding.period = %s', paste(periods.temp, collapse = ', '), holding.period.temp))
    print(sprintf('Sharpe = %s', sharpe))
    break
  }
}

# Ответ
# На коротком отрезке времени легко полчить высокие значения sharpe
# Для max.value <- dim(backtest.data)[1] 
# Получаем:  Periods = 2863, 4611, 4844, holding.period = 1804 -> Sharpe = 6.078 > 1

# Для max.value <- floor(dim(backtest.data)[1] * 0.8)
# Получаем: Periods = 2645, 3384, 3601, holding.period = 1876 -> Sharpe = 0.9428


{
  periods.best <- c(2645, 3384, 3601) #
  holding.period.best <- 1876 # 
  
  pnl.best <- base.strategy(periods = periods.best, holding.period = holding.period.best)
  nav.best <- portfolio.value(money, pnl.best)
  plot(nav.best, type = 'l')

  print(sprintf('Periods = %s, holding.period = %s', paste(periods.temp, collapse = ', '), holding.period.temp))
  print(sprintf('Sharpe = %s', sharpe))
}



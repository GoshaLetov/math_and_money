hello <- 'Hello, world'
print(hello)
hello

a <- c(1, 3, 2)

0.1 + 0.1 == 0.2

0.1 + 0.05 == 1.5
all.equal(0.1 + 0.05, 0.15)

u <- seq(0, 1, 1/3)
v <- seq(0, 1, 1/7)

a <- (1:2)/2
a

unique(sort(c(u, v)))

get_fractions <- function(m, n) {
  return(unique(sort(c((0:m)/m, (0:n)/n), decreasing = TRUE)))
}

q <- 'Q'
a <- 'A'
paste(q, a, sep = '')

paste(c('1', '2', '3'), rep('.Adjusted', 3), sep = '')

get_fractions(3, 7)

as.logical(-1:1)

as.character(as.logical(-1:1))

as.numeric(as.logical(-1:1))

c <- c('function' = 1)
names(c)


install.packages()[,"Package"]


set.seed(1337)
x <- runif(1e6, min = -1, max = 1)

sum(ifelse(x > -0.2 & x < 0.3, 1, 0))


dice_roll <- function(n) {
  ifelse()
}

help('runif')

install.packages('randtoolbox', dependencies = TRUE)
library(randtoolbox)

install.packages('xts', dependencies = TRUE)
library(randtoolbox)

help(package = "xts")

length(c(1, 2, 4))

which.max(5)
which.max(1:10)
which.max(22:19)
which.max(c("A", "BBB", "Z"))
which.max(c("1", "99", "HI"))
max(c(TRUE, FALSE))


paste(letters[c(1, 23, 5, 19, 15, 13, 5)], collapse = '')


x <- c(0, 0, 3, 4, 4, 8)
y <- c(3:0, 1)
c <- x[-1] - x[-length(x)]


is_monotone <- function(x) {
  diff <- x[-1] - x[-length(x)]
  incr <- all((diff >= 0 || diff <= 0) & length(unique(x)) != 1)
  return(incr)
}

is_monotone(x)
is_monotone(y)
is_monotone(rep(10, 10))

fac <- function(x) {
  if(x == 1) return(1)
  return(x * fac(x - 1))
}

fac(5)

combin_count <- function(n, k, with_repretitions = FALSE) {
  fac <- function(x) {
    if(x == 1) return(1)
    return(x * fac(x - 1))
  }
  
  if(with_repretitions) {
    c <- fac(n + k - 1) / fac(k) / fac(n - 1)
  } else {
    c <- fac(n) / fac(k) / fac(n - k)
  }
  return(c)
}

combin_count(10, 5, with_repretitions = TRUE)

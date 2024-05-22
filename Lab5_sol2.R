
# Powtórka i wizualizacja wyników

# Zd 5
N <- 10000
n <- 10
mi <- 0
sigma <- 1
alfa <- 0.05
q <- qt(1 - alfa / 2, n - 1)
k <- 0
for(i in 1:N){
  x <- rnorm(n, mi, sigma)
  m <- mean(x)
  s <- sd(x)
  if( mi >= m - q * s / sqrt(n) & mi <= m + q * s / sqrt(n) ) k <- k + 1
}
paste0(k/N*100, "%")
k/N

# wizualizacja wyników


# funkcja replicate
replicate(5, 1 + 1)
mi <- 0
sigma <- 1
m <- 10000
prz.ufn = replicate(m,{
  x <- rnorm(n, mi, sigma)
  m <- mean(x)
  s <- sd(x)
  c( m - q * s / sqrt(n) , m + q * s / sqrt(n) )
})

lewy <- prz.ufn[1,]
prawy <- prz.ufn[2,]
plot(c(lewy[1],prawy[1]),c(1,1),type="l", xlim=c(-2,2),ylim=c(1,100),xlab=" " , ylab=" ")
abline(v=mi, col = 2)
for (i in 1:100) # dla i=1,2,...,m
{ 
  lines(c(lewy[i],prawy[i]),c(i,i))
}

# Zadanie 8 ---------------------------------------------------------------

k <- 578
n <- 1014

?binom.test
binom.test(k, n, conf.level = 0.95)$conf

## moze recznie?
#p_dach <- k/n
#c(p_dach + c(-1,1)*qnorm(0.975)*sqrt((p_dach*(1-p_dach))/n))


# Zadanie 9 ---------------------------------------------------------------

k <-  3
n <-  12
binom.test(k, n, conf.level = 0.95)$conf


# Zadanie 10 --------------------------------------------------------------

iris
vir <- iris$Petal.Length[iris$Species == "virginica"]


prz.ufn.mi <- function(x, alfa){
  n <- length(x)
  m <- mean(x)
  s <- sd(x)
  q <- qt(1 - alfa / 2, n - 1)
  prz <- m + c(-1, 1) * s * q / sqrt(n)
  prz
}
prz.ufn.war <- function(x, alfa) {
  n <- length(x)
  q1 <- qchisq(1 - alfa / 2, n - 1)
  q2 <- qchisq(alfa / 2, n - 1)
  prz <- (n - 1) * var(x) / c(q1, q2)
  prz
}
prz.ufn.odch <- function(x, alfa) {
  n <- length(x)
  q1 <- qchisq(1 - alfa / 2, n - 1)
  q2 <- qchisq(alfa / 2, n - 1)
  prz <- sqrt((n - 1) * var(x) / c(q1, q2))
  prz
}

prz.ufn.mi(vir, 0.01)
t.test(vir, conf.level = 0.99)$conf
prz.ufn.war(vir, 0.05)
library(TeachingDemos)
sigma.test(vir, conf.level = 0.95)$conf.int

# Zadanie 11 --------------------------------------------------------------

k <- 19
n <- 150
binom.test(k, n, conf.level = 0.96)$conf


# Zadanie 12 --------------------------------------------------------------

x <- chickwts$weight[chickwts$feed == "soybean"]
prz.ufn.war(x, 0.07)


# Zadanie 13 --------------------------------------------------------------

faithful
t.test(faithful$waiting, conf.level=0.99)$conf

prz.ufn.mi(faithful$waiting, 0.01)


# Zadanie 14 --------------------------------------------------------------

Orange
prz.ufn.odch(Orange$circumference, 0.01)
sqrt(sigma.test(Orange$circumference, conf.level = 0.99)$conf.int)

# Zadanie 15 --------------------------------------------------------------

library(MASS)
Pima.te
k <- length(Pima.te$age[Pima.te$type=="Yes"])
n <- length(Pima.te$age)

binom.test(k,n,conf.level = 0.95)$conf


k <- length(Pima.te$age[Pima.te$type=="Yes" & Pima.te$age >= 35])
n <- length(Pima.te$age[Pima.te$age >= 35])

binom.test(k,n,conf.level = 0.95)$conf


########## kartka 4 ##################

# Cel: weryfikacja hipotez ------------------------------------------------


# Zadanie 1 ---------------------------------------------------------------

wytrzymalosc <- c(1.36, 1.14, 1.27, 1.15, 1.20, 1.29, 1.27, 1.18, 1.23, 1.36,
                  1.38, 1.37, 1.30, 1.21, 1.33, 1.28, 1.32, 1.29, 1.33, 1.25)


# H: mu = mu0
# K: mu > mu0

mu0 <- 1.2
sigma <- 0.07
alpha <- 0.04
n <- length(wytrzymalosc)


### I sposób ----------------------------------------------

# statystyka testowa

t <- (mean(wytrzymalosc) - mu0) / sigma * sqrt(n)

# 4.823518

# obszar krytyczny 

qnorm(1 - alpha)

# [1.750686, Inf)

# Odp: t należy do obszaru krytycznego, zatem odrzucamy hipotezę

### II sposób ----------------------------------------------

# p-value 
# P(T > t) = 1 - P(T < t) 

1 - pnorm(t)

# p-value = 7.052399e-07 < alpha

# Odp: odrzucamy hipotezę

library(TeachingDemos)
z.test(wytrzymalosc, mu=1.20, stdev = 0.07, alternative = 'greater')$statistic

# Zadanie 2 ---------------------------------------------------------------

waga <- c(142, 151, 148, 151, 145, 150, 141)

# H: mu = mu0
# K: mu != mu0

mu0 <- 150
alpha <- 0.05
n <- length(waga)
s <- sd(waga)

### I sposób ----------------------------------------------

# statystyka testowa

t <- (mean(waga) - mu0) / s * sqrt(n)
t
# -1.970369

# obszar krytyczny 

qt(1 - alpha / 2, n - 1)

# (-Inf, -2.446912] + [2.446912, Inf)

# Odp: t nie należy do obszaru krytycznego, zatem nie ma podstaw
# do odrzucenia H

### II sposób ----------------------------------------------

# p-value 
# 2*min{P(T > t), P(T < t)} = 2*min{1 - P(T < t), P(T < t)} 

2*min(1 - pt(t, n - 1), pt(t, n - 1))

# p-value = 0.09630099 > alpha

# Odp: nie ma podstaw do odrzucenia H

### III sposób ----------------------------------------------

t.test(waga, mu = 150)$statistic
t.test(waga, mu = 150)$statistic
# p-value = 0.0963 > alpha

# Odp: nie ma podstaw do odrzucenia H


# Zadanie 4 ---------------------------------------------------------------

metoda1 <- scan(nlines = 1)
145 150 153 148 141 152 146 154 139 148

metoda2 <- scan(nlines = 1)
152 150 147 155 140 146 158 152 151 143 153

alpha <- 0.05

# H: mu1 = mu2
# K: mu1 < mu2

### I sposób ----------------------------------------------

# statystyka testowa

stat_testowa <- function(x, y){
  
  p1 <- mean(x) - mean(y)
  
  p2 <- ((length(x) - 1)*sd(x)^2 + (length(y) - 1)*sd(y)^2)/(length(x) + length(y) -2)
  
  p3 <- (length(x) + length(y))/(length(x)*length(y))
  
  p1/sqrt(p2*p3)  
  
}

stat_testowa(metoda1, metoda2)

# -0.9466366

# obszar krytyczny 

qt(-stat_testowa(metoda1, metoda2), length(metoda1) + length(metoda2) - 2)

# (-Inf, - 1.693319]

# Odp: t nie należy do obszaru krytycznego, zatem nie mamy podstaw 
# do odrzucenia H

### II sposób ----------------------------------------------

t.test(metoda1, metoda2, alt = "less", var.equal = TRUE)

# p-value = 0.1779 > alpha -> nie ma podstaw do odrzucenia H



# Zadanie 5 ---------------------------------------------------------------

umy <- scan(nlines = 1)
14 17 7 33 2 24 26 22 12

fiz <- scan(nlines = 1)
13 15 3 2 25 4 1 18 6 9 20 11 5 1 7

alpha <- 0.05

# Nie mamy informacji o wariancji, zatem przeprowadzamy test na równość wariancji

# H: wariancje są równe
# K: wariancje nie są równe 

var.test(umy, fiz)
# p-value = 0.3557 > alpha -> nie ma podstaw do odrzucenia H
# zakładamy równość wariancji
# H: mu1 = mu2
# K: mu1 > mu2
t.test(umy, fiz, alternative = "greater", var.equal = TRUE)
# p-value = 0.01587 < alpha -> odrzucamy H

# stat testowa
s1 <- sd(fiz)
s2 <- sd(umy)
s1
s2
Te <- s1^2 / s2^2
Te
qf(0.95, length(fiz) - 1, length(umy) - 1)
# nie ma podstaw do odrzucenia
# Zadanie 3 ---------------------------------------------------------------

czas <- c(2852, 3060, 2631, 2819, 2805, 2835, 2955, 2595, 2690, 2723, 2815, 2914)

# b)

mu0 <- 2900
alpha <- 0.05
n <- length(czas)

# H: próbka jest z rozkładu normalnego
# K: próbka nie jest z rozkładu normalnego

shapiro.test(czas)

# p-value = 0.9532 > alpha 

# Odp: nie ma podstaw do odrzucenia H

# zakładamy, że próbka jest z rozkładu normalnego

s <- sd(czas)

### I sposób ----------------------------------------------

# statystyka testowa

t <- (mean(czas) - mu0) / s * sqrt(n)

# -2.385525

# obszar krytyczny 

qt(1 - alpha, n - 1)

# (-Inf, -1.795885] 

# Odp: t należy do obszaru krytycznego, zatem odrzucamy H

### II sposób ----------------------------------------------

t.test(czas, mu = 2900, alternative = "less")

# p-value = 0.01807 < alpha

# Odp: odrzucamy H




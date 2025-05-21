

# Lista 3

# Zadanie 3 ---------------------------------------------------------------
# opcjonalnie 
# a)

N <- 5000
Y <- rnorm(N)

srednia <- cumsum(Y)/1:N
plot(1:N, srednia, type = "l")

mediana <- c()

for(i in 1:N){
  mediana[i] <- median(Y[1:i])
}

lines(1:N, mediana, lty = 1, col = 2)
abline(h = 0, col = 3)
#legend("bottomright", c("mean", "median", "mu = 0"), lty = 1, col = 1:3)
# w rnorm mamy estymatory dla sredniej i odchylenia -> one istnieja i sa rowne 0

# b)

N <- 5000
Y <- rnorm(N)

o_st <- c()
r <- c()

for(i in 2:N){
  o_st[i-1] <- sd(Y[1:i])
  r[i-1] <- IQR(Y[1:i])/1.35
}

plot(2:N, o_st, type = "l", log = 'y', ylim = c(0.5, max(o_st)+1))
lines(2:N, r, lty = 1, col = 2)
abline(h = 1, col = 3)
#legend("topleft", c("sd", "IQR/1.35", "sigma = 1"))
# tutaj again dobry estymator to prosta y = 1


# Zadanie 5 ---------------------------------------------------------------

## to juz bylo ############
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

### to mamy zrobic ###################################
# wizualizacja wyników

# funkcja replicate
?replicate
N <- 1000
n <- 10
mi <- 0
sigma <- 1
alfa <- 0.05
q <- qt(1 - alfa / 2, n - 1)
k <- 0
prz.ufn = replicate(N,{
  x <- rnorm(n, mi, sigma)
  m <- mean(x)
  s <- sd(x)
  c( m - q * s / sqrt(n) , m + q * s / sqrt(n) )
})

lewy <- prz.ufn[1,]
prawy <- prz.ufn[2,]
plot(c(lewy[1],prawy[1]),c(1,1),type="l", xlim=c(-2,2),ylim=c(1,100),xlab=" " , ylab=" ")
abline(v=mi, col = 2)
for (i in 2:100) 
{ 
  lines(c(lewy[i],prawy[i]),c(i,i))
}

# Zadanie 6 ---------------------------------------------------------------
# iad 

n <- 50
miu <- 28.4
sigma <- 4.75
alfa <- 0.05
q <- qnorm(1 - alfa / 2)
c(miu - q * sigma / sqrt(n), miu + q * sigma / sqrt(n))


# Zadanie 7 ---------------------------------------------------------------

temp <- scan(nlines = 3)
330.0 322.0 345.0 328.6 331.0 342.0
342.4 340.4 329.7 334.0 326.5 325.8
337.5 327.3 322.6 341.0 340.0 333.0
temp

prz.ufn.mi <- function(x, alfa){
  n <- length(x)
  m <- mean(x)
  s <- sd(x)
  q <- qt(1 - alfa / 2, n - 1)
  prz <- m + c(-1, 1) * s * q / sqrt(n)
  prz
}
prz.ufn.mi(temp, 0.05)

t.test(temp, conf.level = 0.95)
t.test(temp, conf.level = 0.95)$conf

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
prz.ufn.war(temp, 0.05)
prz.ufn.odch(temp, 0.05)

#install.packages("TeachingDemos")
library(TeachingDemos)
sigma.test(temp)$conf.int
# dla odch. standardowego
sqrt(sigma.test(temp)$conf.int)


# Zadanie 9 ---------------------------------------------------------------
# iad

k <-  3
n <-  12
binom.test(k, n, conf.level = 0.95)$conf


####
p_dach <- 30 / 120
q <- qnorm(1 - 0.05/2)
n <- 12
c(p_dach  - q * sqrt((p_dach * (1 - p_dach) ) / n), p_dach  + q * sqrt((p_dach * (1 - p_dach) ) / n))

prop.test(30, 120)$conf

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

prz.ufn.war(Orange$circumference, 0.01)$odch


# Zadanie 15 --------------------------------------------------------------

library(MASS)

k <- length(Pima.te$age[Pima.te$type=="Yes"])
n <- length(Pima.te$age)

binom.test(k,n,conf.level = 0.95)$conf


k <- length(Pima.te$age[Pima.te$type=="Yes" & Pima.te$age >= 35])
n <- length(Pima.te$age[Pima.te$age >= 35])

binom.test(k,n,conf.level = 0.95)$conf



############################## Kartka 4 #############################

# Cel: weryfikacja hipotez ------------------------------------------------


# Zadanie 1 ---------------------------------------------------------------

wytrzymalosc <- c(1.36, 1.14, 1.27, 1.15, 1.20, 1.29, 1.27, 1.18, 1.23, 1.36,
                  1.38, 1.37, 1.30, 1.21, 1.33, 1.28, 1.32, 1.29, 1.33, 1.25)

# H_0: mu = mu0
# H_1: mu > mu0

mu0 <- 1.2
sigma <- 0.07
alpha <- 0.04
n <- length(wytrzymalosc)

### I sposób ----------------------------------------------
# statystyka testowa

t <- (mean(wytrzymalosc) - mu0) / sigma * sqrt(n)
t
# 4.823518

# obszar krytyczny 
qnorm(1 - alpha)
# [1.750686, Inf)

# Odp: t należy do obszaru krytycznego, zatem odrzucamy hipotezę H_0


# Zadanie 2 ---------------------------------------------------------------

waga <- c(142, 151, 148, 151, 145, 150, 141)

# H_0: mu = mu0
# H_1: mu != mu0

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
# do odrzucenia H_0


# Cel: weryfikacja hipotez ------------------------------------------------


# Zadanie 1 ---------------------------------------------------------------

wytrzymalosc <- c(1.36, 1.14, 1.27, 1.15, 1.20, 1.29, 1.27, 1.18, 1.23, 1.36,
                  1.38, 1.37, 1.30, 1.21, 1.33, 1.28, 1.32, 1.29, 1.33, 1.25)

# H0: mu = mu0
# H1: mu > mu0

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
# Odp: t należy do obszaru krytycznego, zatem odrzucamy hipotezę

### II sposób ----------------------------------------------
# p-value 
# P(T > t) = 1 - P(T < t) 
1 - pnorm(t)

# p-value = 7.052399e-07 < alpha
# Odp: odrzucamy hipotezę

# sposob dodatkowy 
library(TeachingDemos)
z.test(wytrzymalosc, mu=1.20, stdev = 0.07, alternative = 'greater')$statistic
z.test(wytrzymalosc, mu=1.20, stdev = 0.07, alternative = 'greater')
?z.test

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
# do odrzucenia H

### II sposób ----------------------------------------------

# p-value 
# 2*min{P(T > t), P(T < t)} = 2*min{1 - P(T < t), P(T < t)} 
2*min(1 - pt(t, n - 1), pt(t, n - 1))
# p-value = 0.09630099 > alpha
# Odp: nie ma podstaw do odrzucenia H

### III sposób ----------------------------------------------

t.test(waga, mu = 150)
# p-value = 0.0963 > alpha
# Odp: nie ma podstaw do odrzucenia H


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



# Zadanie 4 ---------------------------------------------------------------

metoda1 <- scan(nlines = 1)
145 150 153 148 141 152 146 154 139 148

metoda2 <- scan(nlines = 1)
152 150 147 155 140 146 158 152 151 143 153

alpha <- 0.05

# H0: mu1 = mu2
# H1: mu1 < mu2

### I sposób ----------------------------------------------

# statystyka testowa
stat_testowa <- function(x, y){
  p1 <- mean(x) - mean(y)
  p2 <- ((length(x) - 1)*sd(x)^2 +
           (length(y) - 1)*sd(y)^2)/(length(x) + length(y) -2)
  p3 <- (length(x) + length(y))/(length(x)*length(y))
  p1/sqrt(p2*p3)  
}

stat_testowa(metoda1, metoda2)
# -0.9466366
t.test(metoda1, metoda2, alternative = "less", var.equal = TRUE)$statistic

# obszar krytyczny 
qt(-stat_testowa(metoda1, metoda2), length(metoda1) + length(metoda2) - 2)
# (-Inf, - 1.693319]

# Odp: t nie należy do obszaru krytycznego, zatem nie mamy podstaw 
# do odrzucenia H

### II sposób ----------------------------------------------

t.test(metoda1, metoda2, alt = "less", var.equal = TRUE)
# p-value = 0.1779 > alpha -> nie ma podstaw do odrzucenia H
pt(stat_testowa(metoda1, metoda2), length(metoda1)+length(metoda2)-2)

# Zadanie 5 ---------------------------------------------------------------

umy <- scan(nlines = 1)
14 17 7 33 2 24 26 22 12

fiz <- scan(nlines = 1)
13 15 3 2 25 4 1 18 6 9 20 11 5 1 7

alpha <- 0.05

# Nie mamy informacji o wariancji, zatem przeprowadzamy test na równość wariancji

# H0: wariancje są równe
# H1: wariancje nie są równe 
var.test(umy, fiz)
# p-value = 0.3557 > alpha -> nie ma podstaw do odrzucenia H0
# zakładamy równość wariancji

# jak zrobic ten test recznie?
stat_test <- sd(umy)^2 / sd(fiz)^2
stat_test
qf(0.95, length(fiz) - 1, length(umy) - 1)
# nie ma podstaw do odrzucenia H_0
1 - pf(stat_test, length(umy) - 1, length(fiz) - 1)
2 * (1 - pf(stat_test, length(umy) - 1, length(fiz) - 1))

# H: mu1 = mu2
# K: mu1 > mu2
t.test(umy, fiz, alternative = "greater", var.equal = TRUE)
# p-value = 0.01587 < alpha -> odrzucamy H

# recznie 
n1 <- length(umy)
n2 <- length(fiz)
s_test <- ( mean(umy) - mean(fiz) ) /
  sqrt( (( (n1-1)*sd(umy)^2+(n2-1)*sd(fiz)^2 ) / (n1+n2-2)) * ((n1 + n2)/(n1*n2)) )
s_test
# sprawdzenie
t.test(umy, fiz, alternative = "greater", var.equal = TRUE)$statistic

# obszar kryt
qt(0.95, n1+n2-2)
# 2.29 <- stest, wpada do obszar krytyczny, odrzucamy
# pvalue
1 - pt(s_test, n1+n2-2)


# Zadanie 6 ---------------------------------------------------------------

przed <- scan(nlines = 1)
27 21 34 24 30 27 33 31 22 27

po <- scan(nlines = 1)
29 32 29 27 31 26 35 30 29 28

alpha <- 0.05
var.test(przed, po)
### I sposób -------------------------------------------------------

# z_i = przed_i - po_i

# H0: mu_z = 0
# H1: mu_z < 0

# test na sprawdzenie czy rozkład jest normalny -> 
#nie ma podstaw do odrzucenia H0
shapiro.test(przed - po)
# p-value = 0.504 > alpha

# zakładamy, że rozkład jest normalny

t.test(przed - po, mu = 0, alternative = "less")
# p-value = 0.09322 > alpha -> nie ma podstaw do odrzucenia H0


### II sposób -------------------------------------------------------

# H: mu_przed = mu_po
# K: mu_przed < mu_po

shapiro.test(przed) # p-value = 0.6694 > alpha -> nie ma podstaw do odrzucenia H
shapiro.test(po) # p-value = 0.666 > alpha -> nie ma podstaw do odrzucenia H

# zakładamy, że rozkład "przed" i "po" jest normalny

t.test(przed, po, alternative = "less", paired = TRUE)
# p-value = 0.09322 > alpha -> nie ma podstaw do odrzucenia H

# statystyka recznie
z <- przed - po
s <- sd(z)
n <- length(z)
mean(z) * sqrt(n) / s


# Zadanie 8 ---------------------------------------------------------------

# H0: p = 0.04
# H1: p > 0.04

p0 <- 0.04
p_d <- k/n
n <- 200
k <- 14
alpha <- 0.05

### I sposób -------------------------------------------------------------

# statystyka testowa
t <- (p_d - p0) / sqrt((p0 * (1 - p0))/n)
# 2.165064

# obszar krytyczny
qnorm(1 - alpha)
# [1.644854, Inf)
# Odp: t należy do obszaru krytycznego, zatem odrzucamy H

### II sposób -------------------------------------------------------------

prop.test(k, n, p = 0.04, alt = "greater")
# p-value = 0.02359 < alpha -> odrzucamy H

binom.test(k, n, p = 0.04, alt = "greater")
# p-value = 0.03121 < alpha -> odrzucamy H



# Zadanie 9 ---------------------------------------------------------------

# H: p_tech = p_lic
# K: p_tech > p_lic


### I sposób ---------------------------------------------------------

k_tech <- 455
n_tech <- 700
p_d_tech <- k_tech / n_tech

k_lic <- 517
n_lic <- 1320
p_d_lic <- k_lic / n_lic

p <- (k_tech + k_lic) / (n_tech + n_lic)
n <- (n_tech * n_lic) / (n_tech + n_lic)

# statystyka testowa

t <- (p_d_tech - p_d_lic) / (sqrt((p * (1 - p)) / n))

# 11.05804

# obszar krytyczny

qnorm(1 - alpha)

# [1.644854, Inf)

# Odp: t należy do obszaru krytycznego -> odrzucamy H

### II sposób ---------------------------------------------------------

prop.test(c(455, 517), c(700, 1320), alt = "gr")

# p-value < 2.2e-16 < alpha -> odrzucamy H



# Zadanie 10 --------------------------------------------------------------

# a) 

y <- 0:20
p <- 1 - pbinom(y, 20, 0.2)
y[9]

# b) 

N <- 100000

wyniki <- replicate(N, {rbinom(1, 20, 0.2) > 8})
rozmiar <- mean(wyniki)
rozmiar

# lub

mean(rbinom(N, 20, 0.2) > 8)

# c)

alfa <- 0.02

gamma <- (0.02 - rozmiar)/dbinom(8, 20, 0.2)
gamma


################## zadanie 10 ###################

# Niech x1,...xn b?dzie pr?b? z rozk?adu dwupunktowego b(1,p). 
# P(X=1)=p, p(X=0)=1-p
#a) Przyjmuj?c, ?e n=20, poda? posta? najmocniejszego testu na 
# poziomie istotno?ci 0.02 do weryfikacji hipotezy 
# H0:p=0.2 wobec hipotezy alternatywnej H1:p=0.3
#b) Obliczy? rozmiar tego testu. 
#c) Wyznaczy? metod? symulacyjn? rozmiar rozwa?anego testu. 
#d) Poda? posta? testu zrandomizowanego rozmiaru 0,02. 
#e) Zbada? metod? symulacyjn?, czy rozmiar testu zrandomizowanego 
#wynosi faktycznie 0.02. 

# z lematu Neymana-Pearsona
# test JNM na poziomie istotnosi alpha
# fi(x1,...xn)=1 gdy T>c
# fi(x1,...xn)=0 gdy T<c
# gdzie T= x1+...+xn ma rozklad b(n,p0), gdy H0 prawdziwa
# oraz P(T>c)<=alpha, gdzie P=P_p0

n=20
p0=0.2
alpha=0.02

# czyli szukamy c takiego, ?e:
# P(T>c)<=alpha, gdzie P=P_p0

# co jest rownowa?ne: 
#F(c)>=1-alpha=0.98, gdzie F to dystrybuanta b(n,p0)


k <- 0:n 
F1 <- pbinom(k,n, p0) 
F1
Fk <- data.frame(k,F1=round(F1, 10)) 
Fk
Fk[8:9,]

#czyli widac, ze
# F(7)=0.9678573<0.98<F(8)=0.9900182


# Zatem bierzemy c=8 i otrzymujemy test 
# fi(x1,...x20)=1 gdy T>8
# fi(x1,...x20)=0 gdy T<8

#rozmiar tego testu to P(T>c)= 1- F(c)
c <- 8
1 - pbinom(c,n, p0) 

# wyznaczamy rozmiar metod? symulacyjn?
?rbinom
m=10000 
rbinom(n, 1, p0)
wyniki=replicate(m, 
                 { 
                   x=rbinom(n, 1, p0) #n-eltowa proba z rozk?adu b(1,p0)
                   T=sum(x)
                   T > c
                 }) 

rozmiar=sum(wyniki)/m
rozmiar

#randomizacja
# fi(x1,...x20)=1 gdy T>8
# fi(x1,...x20)=gamma gdy T=8
# fi(x1,...x20)=0 gdy T<8

#gdzie gamma tak dobrane, by test miar rozmiar alpha=0.02

#alpha=P(T>8) + gamma * P(T=8)
#czyli: gamma=(alpha - P(T>8))/P(T=8)

gamma=(alpha-(1- pbinom(c,n, p0)))/dbinom(c,n, p0)
gamma

#czyli z p-stwem gamma odrzucamy H0 gdy T=8

# wyznaczamy rozmiar tego testu metod? symulacyjn?

m=10000 
wyniki=replicate(m, 
                 { 
                   x=rbinom(n, 1, p0) #n-eltowa proba z rozk?adu b(1,p0)
                   T=sum(x)
                   (T > c) | (T==c & runif(1)<gamma)
                 }) 

rozmiar=sum(wyniki)/m
rozmiar



# Zadanie 11 --------------------------------------------------------------

# H: mu = 2
# K: mu > 2

mu <- seq(1, 4, 0.1)

moc <- c()
k <- qt(.95, 9)
N <- 1000

for(i in 1:length(mu)){
  wynik <- replicate(N, {
    x <- rnorm(10, mu[i], 1)
    t <- (mean(x) - 2)/sd(x)*sqrt(10)
    t > k 
  })
  moc[i] <- mean(wynik)
} 

plot(mu, moc, las = 1, type = "l",
     xlab = expression(mu),
     ylab = expression(M(mu)),
     ylim = c(0, 1))

abline(v = 2, lty = 2)
text(1.2, 0.08, expression(alpha==0.05))
abline(h = 0.05, lty = 2)


# b)

mu <- seq(1, 4, 0.1)

moc <- c()
k <- qt(.975, 9)
N <- 1000

for(i in 1:length(mu)){
  wynik <- replicate(N, {
    x <- rnorm(10, mu[i], 1)
    t <- (mean(x) - 2)/sd(x)*sqrt(10)
    t > k | t < -k
  })
  moc[i] <- mean(wynik)
} 

plot(mu, moc, las = 1, type = "l",
     xlab = expression(mu),
     ylab = expression(M(mu)),
     ylim = c(0, 1))

abline(v = 2, lty = 2)
text(1.2, 0.08, expression(alpha==0.05))
abline(h = 0.05, lty = 2)



################## zadanie 12 ##################
# Zmienne age oraz height (znajduj?ce si? w ramce danych Loblolly) opisuj?, 
# odpowiednio, wiek (w latach) oraz wysoko?? drzew (w stopach). 
# Zak?adamy, ?e rozk?ad wysoko?ci jest normalny. Na poziomie istotno?ci 0.05
# zweryfikowa? hipotez?, ?e ?rednia wysoko?? 15-letnich drzew wynosi 40 st?p.

data(Loblolly)
head(Loblolly)

# H0: mu = 40
# H1: mu != 40

x <- Loblolly$height[Loblolly$age==15]
t.test(x, mu=40)

p <- t.test(x, mu=40)$p.value
p
# 
alpha=0.05
p < 0.05
# pvalue wieksze od alpha zatem brak podstaw do odrzucenia H0

################## zadanie 15 ##################
#Ramka danych Orange zawiera mi?dzy innym dane dotycz?ce obwodu drzewek 
#pomara?czowych (zmienna circumference). Zak?adaj?c, ?e zmienna ta ma 
#rozk?ad normalny, zweryfikowa? hipotez?, ?e ?redni obw?d drzew jest mniejszy 
#ni? 130mm. Przyj?? poziom istotno?ci 0.1. 

data(Orange)
head(Orange)

# H0: mu = 130
# H1: mu < 130

x= Orange$circumference
t.test(x, mu=130,alt="less")

p=t.test(x, mu=130,alt="less")$p.value
p

alpha=0.1

p<alpha






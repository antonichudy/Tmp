

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
wyniki <- replicate(m, 
                    { 
                      x <- rbinom(n, 1, p0) #n-eltowa proba z rozk?adu b(1,p0)
                      Te <- sum(x)
                      Te > c
                    }) 

rozmiar <- sum(wyniki)/m
rozmiar
##############################  i tutaj to pokazalismy
rbinom( n, 1, p0)
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
                   Te2=sum(x)
                   (Te2 > c) | (Te2==c & runif(1)<gamma)
                 }) 

rozmiar=sum(wyniki)/m
rozmiar



# Zadanie 11 --------------------------------------------------------------

# H: mu = 2
# K: mu > 2

mu <- seq(1, 4, 0.1)
mu
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


################## zadanie 13 ###################

#H0: p=0.35
#H1: p>0.35

k=15+32
k
n=120
p=k/n
p
p0=0.35

prop.test(k,n,p0,alternative="greater")
p=prop.test(k,n,p0,alternative="greater")$p.value
alpha=0.03
p<alpha

prop.test(k,n,p0,alternative="greater",correct=F)

binom.test(k,n,p0,alternative="greater")


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


################## zadanie 18 ###################
#H0: p=0.45
#H1: p>0.45

k=226
n=500
p=k/n
p

p0=0.45

prop.test(k,n,p0,alternative="greater")
p=prop.test(k,n,p0,alternative="greater")$p.value
alpha=0.05
p<alpha

prop.test(k,n,p0,alternative="greater",correct=F)

binom.test(k,n,p0,alternative="greater")

################## zadanie 14  ###################
# X~N(mu1,sigma1), Y~N(mu2,sigma2), sigma1=sigma2
# H0: mu1 = mu2
# H1: mu1 > mu2 
# pr?by niezale?ne

data(chickwts)
head(chickwts)
x = chickwts$weight[chickwts$feed=='meatmeal']
y = chickwts$weight[chickwts$feed=='casein']

#test t dla dw?ch pr?b

t.test(x,y,var.equal = TRUE, alt='greater')
p=t.test(x,y,var.equal = TRUE, alt='greater')$p.value
alpha=0.05
p<alpha

################## zadanie 17  ###################
# X~N(mu1,sigma1), Y~N(mu2,sigma2), sigma1=sigma2
# H0: mu1 = mu2
# H1: mu1 < mu2 
# pr?by niezale?ne
library(MASS)
data(crabs)
x=crabs$CW[crabs$sp=="B"]
y=crabs$CW[crabs$sp=="O"]
t.test(x,y,var.equal=TRUE, alt='less')

p=t.test(x,y,var.equal=TRUE, alt='less')$p.value
alpha=0.04
p<alpha


################## zadanie 16  ###################
#X~N(mu1,sigma1), Y~N(mu2,sigma2)
# H0: sigma1=sigma2
# H1: sigma1!=sigma2

library(MASS)
data(Pima.te)
head(Pima.te)
x = Pima.te$glu[Pima.te$type=='No']   #zdrowe      
y = Pima.te$glu[Pima.te$type=='Yes']   #chore
var.test(x,y)

p=var.test(x,y)$p.value
alpha=0.05
p<alpha
# i nie mozemy iss dalej
# nie wiemy co z hipoteza

################## zadanie 20  ###################
# X~N(mu1,sigma1), Y~N(mu2,sigma2)
# H0: mu1 = mu2
# H1: mu1 < mu2 
# obserwacje zalezne w parach!!!!

x = c(3.5, 4.0, 3.7, 4.6, 3.0, 3.9)
y = c(4.2, 3.9, 3.8, 4.5, 3.4, 4.2)


t.test(x,y, paired=TRUE, alt='less')
p=t.test(x,y, paired=TRUE, alt='less')$p.value
alpha=0.1
p<alpha








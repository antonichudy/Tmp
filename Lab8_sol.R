################### kartka 4 #########################################


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

################## zadanie 17  ###################  nie bo kartkowka 
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


################## zadanie 16  ################### NNNIEEEEEEEEEEEEEEEEE
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







### kartka 5   ############################################################################## 

###########################################
###       STATYSTYKA MATEMATYCZNA       ###
###           LABORATORIUM 8            ###
###########################################



# Cel: testowanie zgodności, jednorodności i niezależności ----------------


# ZESTAW 5 ----------------------------------------------------------------


# Zadanie 1 ---------------------------------------------------------------

n <- rnorm(200)
c <- rcauchy(200)
u <- runif(200)
w <- rexp(200)
aw <- w*(-1)

par(mfrow = c(1,1))
qqnorm(n, main = "N(0,1)")
qqline(n)
qqnorm(c, main = "C(0,1)")
qqline(c)
qqnorm(u, main = "U([0,1])")
qqline(u)
qqnorm(w, main = "Exp(1)")
qqline(w)
qqnorm(aw, main = "NegExp(1)")
qqline(aw)



# Zadanie 2 # tego nie robie

par(mfrow = c(1, 2))
N <- 200
u <- runif(N)
qu <- qunif(ppoints(N))
qqplot(qu, u)

u <- rexp(N)
qu <- qexp(ppoints(N))
qqplot(qu, u)

ppoints(5, a = 1 / 2)
? ppoints



# Zadanie 3 ---------------------------------------------------------------

# a)

# H0: p1 = p2 = p3 = p4 = 1/4
# H1: !H

k <- 4
N <- 200
n <- c(73, 74, 34, 19)
p <- n/N
alpha <- 0.05
p

# I sposób 

# statystyka testowa

t <- sum((n - N * 1/4) ^ 2 /(N  * 1/4))

#  46.44

# obszar krytyczny

qchisq(1 - alpha, k - 1)

# [7.814728, Inf)

# t należy do obaszru krytycznego -> odrzucamy hipotezę 

# II sposób 

chisq.test(n, p = rep(0.25, 4))

# p-value = 4.572e-10 < alpha -> odrzucamy H0

# b) #########################################################################

# H: p1 = 0.367, p2 = 0.378, p3 = 0.186, p4 = 0.076
# K: !H

p <- c(0.367, 0.371, 0.186, 0.076)
chisq.test(n, p = p)

# p-value = 0.7463 > alpha -> nie ma podstaw do odrzucenia H
n

# recznie

# statystyka testowa

t <- sum((n - N * p) ^ 2 /(N  * p))
t
#  1.23

# obszar krytyczny

qchisq(1 - alpha, k - 1)
# [7.81, + INF)

# jest ok




# Zadanie 4 ---------------------------------------------------------------

proba <- c(10, 26, 56, 64, 30, 14)

# H: proba z rozkladu normalnego
# K: !H


# lewy kraniec przedziału 
w1 <- 19:24
# środek przedziału
w2 <- 19:24 + 0.5
# prawy kraniec przedziału
w3 <- 20:25

# estymujemy mu i sigma 

# estymator mu - średnia

# średnia 

mu <- sum(w2 * proba) / sum(proba)
mu
# 22.1

# estymator sigma - odchylenie

sigma <- sqrt(sum(proba * (w2 - mu) ** 2) / (sum(proba) - 1))

# 1.235977
pr <- pnorm(w3, mu, sigma) - pnorm(w1, mu, sigma)
pr[1] <- pnorm(w3[1], mu, sigma)
pr[length(pr)] <- 1-pnorm(w1[length(pr)], mu, sigma)

chisq.test(proba, p=pr) 


# p-value = 0.9295 > alpha -> nie ma podstaw do odrzucenia H



# Zadanie 5 ---------------------------------------------------------------

prop.test(c(61, 34, 38, 35), c(206, 164, 98, 102))

# p-value = 0.0104 < alpha -> odrzucamy H


# Zadanie 6 ---------------------------------------------------------------

#test Kolmogorowa-Smirnova

dane <- c(2.5, 1.8, 6.0, 0.5, 8.75, 1.2, 3.75)
ks.test(dane, 'pexp', 1/4)

# Zad 14 

# Dane wejściowe
dane <- matrix(c(55, 32, 47, 21,
                 60, 43, 35, 37),
               nrow = 2,
               byrow = TRUE)
dane

test <- chisq.test(dane)
test




## zadanie 14


14:11

coke<-c(55,60)
pepsi<-c(32,43)
sevenup<-c(47,35)
drpepper<-c(21,37)

df<-data.frame(coke=coke,
               pepsi=pepsi,
               sevenup=sevenup,
               drpepper=drpepper)
df

row_sums <- rowSums(df)
row_sums
col_sums <- colSums(df)
col_sums
n<- sum(df)

# Obliczanie E
outer(row_sums, col_sums)
expected <- outer(row_sums, col_sums) /n
expected


observed <- as.matrix(df)
chi_square <- sum((observed - expected)^2 / expected)
chi_square

# Stopnie swobody
df_chi <- (nrow(df) - 1) * (ncol(df) - 1)
df_chi

# Wartość krytyczna chi-kwadrat dla poziomu istotności 0.05
critical_value <- qchisq(0.95, df_chi)
critical_value

#Q=6.813521
#[7.814728, +infty)


chisq.test(df)
x<-chisq.test(df)
x$expected









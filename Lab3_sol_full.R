

# Zadanie 5 ---------------------------------------------------------------


czynsz <- c(334, 436, 425, 398, 424, 429, 392, 428, 339, 389,
            352, 405, 392, 403, 344, 400, 424, 443, 378, 387,
            384, 498, 374, 389, 367, 457, 409, 454 ,345, 422)

summary(czynsz)
var(czynsz)
sd(czynsz)
IQR(czynsz)
diff(range(czynsz))
library(moments)
skewness(czynsz)
kurtosis(czynsz)
b=boxplot(czynsz)
b$out


#read.csv(): Ta funkcja jest używana do odczytywania plików CSV, gdzie przecinek (,) jest używany jako separator 
#dziesiętny, a średnik (;) jako separator pól. Jest to najczęściej spotykany format plików CSV w wielu regionach.

#read.csv2(): Natomiast funkcja read.csv2() jest przeznaczona do odczytywania plików CSV, gdzie średnik (;)
#jest używany jako separator dziesiętny, a przecinek (,) jako separator pól.
#Jest to format spotykany w niektórych krajach, szczególnie w Europie.

# Zadanie 6 ---------------------------------------------------------------

samochody <- read.csv2("http://pages.mini.pw.edu.pl/~grzegorzewskip/www/?download=samochody.csv")

samochody$zp <- 1 / samochody$mpg * 3.7851 / 1.609 * 100 
#mean(samochody$zp)
#sum(is.na(samochody$zp))
#mean(samochody$zp, na.rm = TRUE)
# czyli sa braki danych
#x <- samochody$zp
#is.na(x)
#sum(is.na(x))

# b)
stem(samochody$zp)
1609/1.609
# c)
hist(samochody$zp)

# d)
# dodac o jadrach ######################### !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
zp2 <- na.omit(samochody$zp)
hist(samochody$zp)
density(zp2, kernel = "epanechnikov")
par(mfrow = c(1, 2))
plot(density(zp2, kernel = "epanechnikov"))
hist(samochody$zp, breaks = 37)
plot(density(zp2, kernel = "epanechnikov", bw = 0.1)) # wspolczynnik wygladzajacy
?density
par(mfrow = c(2, 3))
plot(density(zp2, kernel = "gaussian"))
plot(density(zp2, kernel = "epanechnikov"))
plot(density(zp2, kernel = "rectangular"))
plot(density(zp2, kernel = "triangular"))
plot(density(zp2, kernel = "cosine"))
########################################################################################################

# e)
par(mfrow = c(1, 1))
boxplot(samochody$zp)


# f)
tmp <-na.omit(samochody)
zp <- na.omit(samochody$zp)

mean(samochody$zp, na.rm = TRUE)
median(samochody$zp, na.rm = TRUE)
var(samochody$zp, na.rm = TRUE)
sd(samochody$zp, na.rm = TRUE)
range(samochody$zp, na.rm = TRUE)
diff(range(samochody$zp, na.rm = TRUE))
max(samochody$zp, na.rm = T)
quantile(samochody$zp, na.rm = TRUE, c(0.25, 0.75, 0.9))
IQR(samochody$zp, na.rm = TRUE)
library(moments)
kurtosis(samochody$zp, na.rm = TRUE)
skewness(samochody$zp, na.rm = TRUE)

# g)
quantile(samochody$zp, c(0.05, 0.1, 0.9, 0.95), na.rm = TRUE)


# h)
mean(samochody$zp, na.rm = TRUE, trim = 0.05)



# Zadanie 7 ---------------------------------------------------------------

#dane=read.csv2("http://pages.mini.pw.edu.pl/~grzegorzewskip/www/?download=samochody.csv")
#dane$zp=378.5/(1.609*dane$mpg)
x <- samochody$zp

# podzial na klasy
malo <- x[x<=7]
malo
srednio <- x[x>7 & x<=10]
srednio
duzo <- x[x>10]
duzo

#wykres slupkowy
kategoria=c("malo","srednio","duzo")
licznosci=c(length(malo),length(srednio),length(duzo))
barplot(licznosci,names=kategoria)

100*licznosci/sum(licznosci)

#kodowanie inaczej: funkcja cut
x.kod <- cut(x,breaks=c(-Inf,7,10,Inf))
head(x, 10)
head(x.kod, 10)
x.kod
# nadajemy etykietki "malo", "srednio", "duzo"
x.kod=cut(x,breaks=c(-Inf,7,10,Inf),labels=c("malo", "srednio", "duzo"))
x.kod
t=table(x.kod)
t
prop.table(t)
barplot(t)


# Zadanie 8 ---------------------------------------------------------------

zpA=samochody$zp[samochody$producent==1]
zpE=samochody$zp[samochody$producent==2]
zpJ=samochody$zp[samochody$producent==3]
mean(zpA)
sd(zpA)
mean(zpE)
is.na(zpE)
mean(na.omit(zpE))
mean(zpE,na.rm=T)
sd(na.omit(zpE))
mean(zpJ)
sd(zpJ)
boxplot(zpA,zpE,zpJ,horizontal=T,names=c("A","E","J"))
boxplot(samochody$zp~samochody$producent,horizontal=T,names=c("A","E","J"))
b <- boxplot(zpA,zpE,zpJ,horizontal=T,names=c("A","E","J"))

#inaczej: funkcja tapply

#tapply(X, INDEX, FUN = NULL)
#Arguments:
#  -X: An object, usually a vector
#-INDEX: A list containing factor
#-FUN: Function applied to each element of x


tapply(samochody$zp,samochody$producent,length)
tapply(samochody$zp,samochody$producent,summary)
tapply(samochody$zp,samochody$producent,sd, na.rm=T)

data(iris)
tapply(iris$Sepal.Width, iris$Species, median)


# Zadanie 9 ---------------------------------------------------------------

boxplot(samochody$zp)
boxplot(samochody$zp ~ samochody$cylindry)

####
library(dplyr)
tmp <- samochody %>% 
  group_by(cylindry) %>% 
  select(zp, cylindry)
tmp
### ggplot ###
#install.packages("ggplot2")
library(ggplot2)
ggplot(samochody, aes(x = factor(cylindry), y = zp)) + 
  geom_boxplot()

###

# Zadanie 10 ---------------------------------------------------------------

zp10 <- samochody$zp[samochody$waga < 2500]
mean(zp10)
median(zp10)
sd(zp10)
skewness(zp10)

### dplyr ###

samochody %>% 
  filter(waga < 2500) %>% 
  summarise(srednia = mean(zp),
            mediana = median(zp),
            sd = sd(zp),
            skewness = skewness(zp))

###

# Zadanie 11 ---------------------------------------------------------------

mocrok=samochody$moc[samochody$rok>=79 & samochody$rok<=81]
tmp <- samochody[samochody$rok>=79 & samochody$rok<=81, 'moc']


#a
summary(mocrok)
summary(tmp)
boxplot(mocrok,horizontal=T)
sort(mocrok)

#b

quantile(mocrok,0.95)
quantile(mocrok,0.95,na.rm=T)



# Zadanie 12 ---------------------------------------------------------------

przyspieszenie <- samochody$przysp[samochody$waga > 2500 & samochody$waga < 3000]

# a)

boxplot(przyspieszenie)

# b)
# 75% mniejsze
quantile(przyspieszenie, 0.75)


# Zadanie 13 ---------------------------------------------------------------

waga <- samochody$waga[samochody$mpg >= 26]

# a)

boxplot(waga)

# b)

quantile(waga, 0.95, na.rm = TRUE)



# Zadanie 14 ---------------------------------------------------------------

boxplot(samochody$przysp~samochody$prod)
boxplot(przyspA, przyspJ, names = c("a", "b"))
?boxplot
przyspA=samochody$przysp[samochody$producent==1]
przyspJ=samochody$przysp[samochody$producent==3]
summary(przyspA)
summary(przyspJ)

### ggplot ###  # NIE RYSUJE !!!
library(dplyr)
samochody %>% 
  filter(producent != 2) %>%
  ggplot(aes(x = producent, y = przysp, group = producent)) + 
  geom_boxplot()
###
   

#############################  LISTA 3 ###################################



# Zadanie 1 ---------------------------------------------------------------

# co to jest dystrybuanta empiryczna? 
x=c(2,2,4,3,1,2,1,5,2,3)
x
table(x)
prop.table(table(x))
plot(ecdf(x))
cumsum(prop.table(table(x)))

# wracamy do zadania 
x1 <- rnorm(20)
x2 <- rnorm(100)
plot(ecdf(x1))
plot(ecdf(x2), add = TRUE, col = 2)
curve(pnorm(x), add = TRUE, col = 3, lw = 3)
#legend("topleft", c("N(0,1) - 20","N(0,1) - 100", "N(0,1) - teoretyczna"), lty=1, col=1:3, text.col=1)


x3 <- rnorm(200)
plot(ecdf(x1))
plot(ecdf(x3), add = TRUE, col = 2)
curve(pnorm(x), add = TRUE, col = 3, lw = 3)
#legend("topleft", c("N(0,1) - 20","N(0,1) - 200", "N(0,1) - teoretyczna"), lty=1, col=1:3, text.col=1)


x4 <- rnorm(1000)
plot(ecdf(x3))
plot(ecdf(x4), add = TRUE, col = 2)
curve(pnorm(x), add = TRUE, col = 3, lw = 3)
#legend("topleft", c("N(0,1) - 220","N(0,1) - 1000", "N(0,1) - teoretyczna"), lty=1, col=1:3, text.col=1)


# Zadanie 2 ---------------------------------------------------------------

# a)

N <- 500
Y <- rcauchy(N)

med <- c()
sre <- c()
for(i in 1:N) {
  med[i] <- median(Y[1:i])
  sre[i] <- mean(Y[1:i])
}

plot(1:N, sre, type = "l", col = 1)
lines(1:N, med, type = "l", col = 2)
abline(h = 0, col = 3)
#legend("topleft", c("mean","median", "a = 0"), lty = 1, col = 1:3, text.col = 1)

# b)

N <- 500
Y <- rcauchy(N)

st <- c()
r <- c()
for(i in 2:N){
  st[i-1] <- sd(Y[1:i])
  r[i-1] <- IQR(Y[1:i])/2
}

plot(2:N, st, type = "l", log = 'y', ylim = c(0.5, max(st)+1), col = 1)
lines(2:N, r, lty = 1, col = 2)
abline(h = 1, col = 3)
#legend("topleft", c("sd","SQR", "b = 1"), lty = 1, col = 1:3, text.col = 1)


# Zadanie 3 ---------------------------------------------------------------

# a)
# a) Estymacja parametru po?o?enia a=0
N <- 500
Y <- rnorm(N)

srednia <- cumsum(Y)/1:N
plot(1:N, srednia, type = "l")

med <- c()

for(i in 1:N){
  med[i] <- median(Y[1:i])
}

lines(1:N, med, lty = 1, col = 2)
abline(h = 0, col = 3)
legend("bottomright", c("mean", "median", "mu = 0"), lty = 1, col = 1:3)

# b) # b) Estymacja parametru rozproszenia b=1

N <- 500
Y <- rnorm(N)

st <- c()
r <- c()

for(i in 2:N){
  st[i-1] <- sd(Y[1:i])
  r[i-1] <- IQR(Y[1:i])/1.35
}

plot(2:N, st, type = "l", log = 'y', ylim = c(0.5, max(st)+1))
lines(2:N, r, lty = 1, col = 2)
abline(h = 1, col = 3)
legend("topleft", c("sd", "IQR/1.35", "sigma = 1"))


# Zadanie 4 ---------------------------------------------------------------

n <- 10000
theta <- 1

theta_M <- c()
theta_NW <- c()

for (i in 1:n){
  proba <- runif(20, 0, theta)
  theta_M[i] <- 2*mean(proba)
  theta_NW[i] <- max(proba)
}

b_M <- mean(theta_M)-theta
b_NW <- mean(theta_NW)-theta
mse_M <- sum((theta_M-theta)^2)/n
# lub: var(theta_M)+b_M^2
mse_NW <- sum((theta_NW-theta)^2)/n
# lub: var(theta_NW)+b_NW^2


# Zadanie 5 ---------------------------------------------------------------

N <- 10000
n <- 10
mi <- 9
sigma <- 3
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






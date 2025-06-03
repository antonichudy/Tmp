###############################################
###############################################
################  ZAJ?CIA 7  ##################
###############################################
###############################################

###################################################
#############    SM I MiNI 2020-2021   ############
################################################### 


#################################################
############        zestaw 4         ############
#################################################

# czyszczenie pami?ci:

rm(list=ls())
ls()

############# weryfikacja hipotez #############

############# testy parametryczne dla jednej pr?by #############

################## model normalny ##################

################## zadanie 1 ###################
#Wytrzyma?o?? na ci?nienie wewn?trzne szk?a butelek jest ich wa?n? 
#charakterystyk? jako?ciow?. Pewna rozlewnia zainteresowana jest butelkami, 
#kt?rych ?rednia wytrzyma?o?? przewy?sza 1,20 N/mm2 . Na podstawie 
#dotychczasowych do?wiadcze? wiadomo, ?e rozk?ad ci?nienia jest normalny 
#z odchyleniem standardowym 0.07 N/mm2 . Pobrano pr?b? losowa 20 butelek, 
#kt?re nast?pnie umieszczono w maszynie hydrostatycznej, zwi?kszaj?c ci?nienie 
#a? do zniszczenia butelki. Otrzymano nast?puj?ce wyniki: 
#1.36, 1.14, 1.27, 1.15, 1.20, 1.29, 1.27, 1.18, 1.23, 1.36, 1.38, 1.37, 1.30, 
#1.21, 1.33, 1.28, 1.32, 1.29, 1.33, 1.25. 
#Na poziomie istotno?ci 0.04 stwierdzi?, czy dana partia butelek spe?nia 
#postawione wymagania jako?ciowe. 


# model normalny ze znan? wariancj? >> Model I
# test dla warto?ci oczekiwanej

x=c(1.36, 1.14, 1.27, 1.15, 1.20, 1.29, 1.27, 1.18, 1.23, 1.36, 1.38, 1.37, 1.30, 1.21, 1.33, 1.28, 1.32, 1.29, 1.33, 1.25) 

mean(x)

# ========================================================================
# H0 : mu = 1.20
# H1 : mu > 1.20
# ========================================================================


mu0 = 1.20
sigma = 0.07

# statystyka testowa
T=(mean(x)-mu0)/sigma*sqrt(length(x)) # statystyka testowa
T

# zbior krytyczny: [z(1-alpha);+Inf)
alpha = 0.04
kw=qnorm(1-alpha)
kw

# ========================================================================
# jesli warto?? T zawiera si? w zb.krytycznym, to odrzucamy H0 i przyjmujemy H1
# jesli warto?? T nie zawiera si? w zb.krytycznym, to pozostajemy przy H0
# ========================================================================

T>kw

# inaczej - korzystamy z gotowej funkcji z.test
# z biblioteki TeachingDemos
# install.packages("TeachingDemos")

# ========================================================================
# je?li p-value < alpha, to odrzucamy H0 i przyjmujemy H1
# je?li p-value >= alpha, to pozostajemy przy H0
# ========================================================================


library(TeachingDemos)
z.test(x, mu=1.20, stdev = 0.07, alternative = 'greater')
z.test(x, mu=1.20, stdev = 0.07, alt = 'greater')

z.test(x, mu=1.20, stdev = 0.07, alt = 'less') # test lewostronny

z.test(x, mu=1.20, stdev = 0.07) # test obustronny

p=z.test(x, mu=1.20, stdev = 0.07, alt = 'greater')$p.value 
p


p<alpha

#wyznaczmy sami p-warto??
# p =P(x>t)= 1-P (T=t)
# gdzie X i T to zmienne losowe i.i.d. N(9)0,1)
# zas t to zaobserwowana wartosc T

#wartosc t wyznaczylismy wczesniej sami
t=T
t
#mozemy tez wczytac ja z f-cji z.test
t=z.test(x, mu=1.20, stdev = 0.07, alt = 'greater')$statistic
t

1-pnorm(t)

curve(dnorm(x),-6,6)
abline(v=t,col="red")


################## zadanie 2 ###################
#Nominalna waga netto kawy sprzedawanej w opakowaniu szklanym 
#powinna wynosi? 150 g. 
#Pr?ba losowa siedmiu s?oiczk?w kawy konkretnej marki 
#wykaza?a nast?puj?ce wagi netto  (w gramach): 
#142,  151,  148,  151,  145,  150,  141. 
#Zak?adaj?c normalno?? rozk?adu wagi, przetestowa? hipotez? g?osz?c?, 
#?e waga netto tej marki kawy wynosi faktycznie 150 g. 
#Przyj?? poziom istotno?ci 0.05 


# model normalny z nieznan? wariancj? >> Model II
# test dla warto?ci oczekiwanej


x = c(142,151,148,151,145,150,141)
mean(x)

# H0: mu =150
# H1: mu!=150


mu0=150
T=(mean(x)-mu0)*sqrt(length(x))/sd(x) # statystyka testowa
T

alpha=0.05
kw=qt(1-alpha/2, length(x)-1)  # kwantyl
kw

T<(-1)*kw | T>kw

# Inaczej - korzystamy z gotowej funkcji t.test
t.test(x, mu=150)
p=t.test(x, mu=150)$p.value #p-warto??
p


p<alpha

#wyznaczmy sami p-warto??
# p =P(X<t) + P(x>-t)= 2*P (T<t)
# gdzie X i T to zmienne losowe i.i.d. t(n-1)
# zas t to zaobserwowana wartosc T

t=t.test(x, mu=150)$statistic
t
n=length(x)
2*pt(t,n-1)

curve(dt(x,n-1),-4,4)
abline(v=t,col="red")
abline(v=-t,col="red")
# zr?bmy dodatkowo test dla odch. std.
# H0: sigma =4
# H1: sigma!=4

sig0=4

# test dla wariancji  >> Model I
T=(length(x)-1)*var(x)/(sig0^2)  # statystyka testowa
T
kw1=qchisq(alpha/2,length(x)-1) # kwantyl 1
kw1
kw2=qchisq(1-alpha/2,length(x)-1) # kwantyl 2
kw2

T<kw1 | T>kw2

# inaczej- korzystamy z gotowej funkcji sigma.test
# z biblioteki TeachingDemos
# library(TeachingDemos)

sigma.test(x, sigma = 4)

p=sigma.test(x, sigma = 4)$p.value
p


p<alpha

################## zadanie 3 ###################
#Wylosowana niezale?nie z partii ?ar?wek 12 elementowa pr?ba da?a 
#nast?puj?ce wyniki pomiar?w czasu ?wiecenia  (w godzinach): 
#2852, 3060, 2631, 2819, 2805, 2835, 2955, 2595, 2690, 2723, 2815, 2914. 
#a) Wyznaczy? 97% przedzia? ufno?ci dla ?redniego czasu ?wiecenia ?ar?wek 
#oraz dla odchylenia standardowego czasu ?wiecenia ?ar?wek. 
#b) Czy ?redni czas ?wiecenia ?ar?wek jest istotnie kr?tszy od 2900 godzin?  


# tutaj trzeba dopisac zalozenie o normalnosci rozkladu badanej cechy!!!
# wtedy mamy model normalny z nieznan? wariancj? >> Model II

# test dla warto?ci oczekiwanej 

x= c(2852, 3060, 2631, 2819, 2805, 2835, 2955, 2595, 2690, 2723, 2815, 2914)

mean(x)

alpha=0.03

# H0: mu =2900
# H1: mu<2900

t.test(x, mu=2900,alt="less")

p=t.test(x, mu=2900,alt="less")$p.value
p


p<alpha

# 97% przedzia? ufno?ci dla ?redniej 
t.test(x, conf.level = 0.97)$conf.int

# 97% przedzia? ufno?ci dla odchlenia standardowego 
# library(TeachingDemos)
sqrt(sigma.test(x, conf.level = 0.97)$conf.int)

# Jak sprawdzi? czy rozk?ad jest normalny?
# H0: X ma rozk?ad normalny
# H1: X nie jestz rozk?adu normalnego

qqnorm(x); qqline(x)
shapiro.test(x)

# p-value = 0.9532 czyli brak podstaw do odrzucenia H0 -> zak?adamy ?e pr?ba
# jest z rozk?adu normalnego

################## zadanie 12 ##################
# Zmienne age oraz height (znajduj?ce si? w ramce danych Loblolly) opisuj?, 
# odpowiednio, wiek (w latach) oraz wysoko?? drzew (w stopach). 
# Zak?adamy, ?e rozk?ad wysoko?ci jest normalny. Na poziomie istotno?ci 0.05
# zweryfikowa? hipotez?, ?e ?rednia wysoko?? 15-letnich drzew wynosi 40 st?p.

data(Loblolly)
head(Loblolly)

# H0: mu = 40
# H1: mu != 40


x= Loblolly$height[Loblolly$age==15]
t.test(x, mu=40)

p=t.test(x, mu=40)$p.value
p

alpha=0.05

p<alpha

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

################## zadanie 11 ###################
#Niech x1,...xn b?dzie pr?b? niezale?nych obserwacji z tego samego 
#rozk?adu normalnego. Wyznaczy? metod? symulacyjn? funkcj? mocy testu 
#do weryfikacji hipotezy H0 : mu= 2 wzgl?dem H1: mu>2
#na poziomie istotno?ci 0.05, zak?adaj?c, ?e n=10. 
#Por?wna? funkcj? mocy tego testu z funkcj? mocy otrzyman? 
#dla dwustronnej alternatywy H1: mu!=2.

# H0 : mu= 2 
# H1: mu>2

n=10
mu0=2
alpha=0.05

# stat. testowa: T = (mean(x)-mu0)*sqrt(n)/sd(x)
# zbi?r krytyczny: (kw,+Inf), gdzie kw=qt(1-alpha, n-1) 
# f-cja mocy: f(mu)=P( T > kw),  gdzie P=P_mu 
# mozna pokazac, ze f-cja ta jest postaci
# f(mu)=1-F(kw + [sqrt(n)/S]*(mu0 - mu)), gdzie F to dystrybuanta t(n-1)

# wszczegolnosci widac, ze
# f(mu0)=1-F(kw)=alpha

mu = seq(0, 4, 0.1) # dla tych mu wyznaczamy wartosc f-cji mocy 

moce=numeric() # tu b?dziemy przechowywa? wartosci f-cji mocy 

kw = qt(1-alpha, n-1) 

m = 1000 

for (i in 1:length(mu)) 
{ 
  wynik = replicate(m, 
                    { x = rnorm(n, mu[i], 1) 
                    T = (mean(x)-mu0)*sqrt(n)/sd(x) 
                    T > kw 
                    })
  
  moce[i] = sum(wynik)/m 
} 

moce

#wykres

plot(mu, moce, type="l", ylim=c(0,1)) 
abline(v=2, lty=2, col="red") 
abline(h=0.05, lty=2, col="red") 


### to samo inaczej :-)
### wykorzystamy p-warto??

n=10
mu0=2
alpha=0.05
mu = seq(0, 4, 0.1) # dla tych mu wyznaczamy wartosc f-cji mocy 

moce=numeric() # tu b?dziemy przechowywa? wartosci f-cji mocy 
m = 1000 

for (i in 1:length(mu)) 
{ 
  wynik = replicate(m, 
                    { x = rnorm(n, mu[i], 1) 
                    p=t.test(x, mu=mu0,alt="greater")$p.value
                    p<alpha
                    })
  
  moce[i] = sum(wynik)/m 
} 

moce

#wykres

plot(mu, moce, type="l", ylim=c(0,1)) 
abline(v=2, lty=2, col="red") 
abline(h=0.05, lty=2, col="red") 


# H0 : mu= 2 
# H1: mu!=2

n=10
mu0=2
alpha=0.05

# stat. testowa: T = (mean(x)-mu0)*sqrt(n)/sd(x)
# zbi?r krytyczny: (-Inf,-kw) suma (kw,+Inf), gdzie kw=qt(1-alpha/2, n-1) 
# f-cja mocy: f(mu)=P( T > kw lub T<-kw), gdzie P=P_mu 


mu = seq(0, 4, 0.1) # dla tych mu wyznaczamy wartosc f-cji mocy 

moce=numeric() # tu b?dziemy przechowywa? wartosci f-cji mocy 

kw = qt(1-alpha/2, n-1) 

m = 1000 

for (i in 1:length(mu)) 
{ 
  wynik = replicate(m, 
                    { x = rnorm(n, mu[i], 1) 
                    T = (mean(x)-mu0)*sqrt(n)/sd(x) 
                    T < (-1)*kw | T > kw 
                    })
  
  moce[i] = sum(wynik)/m 
} 

moce

plot(mu, moce, type="l", ylim=c(0,1)) 
abline(v=2, lty=2, col="red") 
abline(h=0.05, lty=2, col="red") 

### teraz to samo z wykorzystaniem p-warto?ci

n=10
mu0=2
alpha=0.05
mu = seq(0, 4, 0.1) # dla tych mu wyznaczamy wartosc f-cji mocy 

moce=numeric() 
m = 1000 

for (i in 1:length(mu)) 
{ 
  wynik = replicate(m, 
                    { x = rnorm(n, mu[i], 1) 
                    p=t.test(x, mu=mu0)$p.value
                    p<alpha
                    })
  
  moce[i] = sum(wynik)/m 
} 

#wykres

plot(mu, moce, type="l", ylim=c(0,1)) 
abline(v=2, lty=2, col="red") 
abline(h=0.05, lty=2, col="red") 



############ testy dla wska?nika struktury ##############


################## zadanie 8 ###################
#H0: p=0.04
#H1: p>0.04

k=14
n=200
p=k/n
p

p0=0.04

prop.test(k,n,p0,alternative="greater")
p=prop.test(k,n,p0,alternative="greater")$p.value
alpha=0.05
p<alpha

prop.test(k,n,p0,alternative="greater",correct=F)

binom.test(k,n,p0,alternative="greater")

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

################## zadanie 10 ###################
#Niech x1,...xn b?dzie pr?b? z rozk?adu dwupunktowego b(1,p). 
# P(X=1)=p, p(X=0)=1-p
#a) Przyjmuj?c, ?e n=20, poda? posta? najmocniejszego testu na 
#poziomie istotno?ci 0.02 do weryfikacji hipotezy 
#H0:p=0.2 wobec hipotezy alternatywnej H1:p=0.3
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


k = 0:n 
F =pbinom(k,n, p0) 

Fk=data.frame(k,F=round(F, 10)) 
Fk
Fk[8:9,]

#czyli widac, ze
# F(7)=0.9678573<0.98<F(8)=0.9900182


# Zatem bierzemy c=8 i otrzymujemy test 
# fi(x1,...x20)=1 gdy T>8
# fi(x1,...x20)=0 gdy T<8

#rozmiar tego testu to P(T>c)= 1- F(c)
c=8
1- pbinom(c,n, p0) 

# wyznaczamy rozmiar metod? symulacyjn?

m=10000 
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


##############################################################
############# testy parametryczne dla dw?ch pr?b #############
##############################################################


################## zadanie 4  ###################
# X~N(mu1,sigma1), Y~N(mu2,sigma2), sigma1=sigma2
# H0: mu1 = mu2
# H1: mu1 < mu2 
# pr?by niezale?ne

x=c(145,150,153,148,141,152,146,154,139,148)
y=c(152,150,147,155,140,146,158,152,151,143,153)

#test t dla dw?ch pr?b

t.test(x,y,alternative="less",var.equal=T)
p=t.test(x,y,alternative="less",var.equal=T)$p.value
alpha=0.05
p<alpha

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

################## zadanie 16  ###################
#X~N(mu1,sigma1), Y~N(mu2,sigma2)
# H0: sigma1=sigma2
# H1: sigma1>sigma2

data(iris)
head(iris)
x =iris$Petal.Length[iris$Species=='virginica']
y =iris$Petal.Length[iris$Species=='versicolor']


var.test(x,y,alt="greater")

p=var.test(x,y,alt="greater")$p.value
alpha=0.05
p<alpha




################## zadanie 5  ###################
# X~N(mu1,sigma1), Y~N(mu2,sigma2)
# X - sta? pracy pracownik?w umys?owych
# Y - sta? pracy pracownik?w fizycznych
# H0: mu1 = mu2
# H1: mu1 > mu2 
# pr?by niezale?ne

# test t wymaga za?o?enia r?wno?ci wariancji!!!
# testujemy najpierw hipotez? H0: sigma1=sigma2
# przeciw H1: sigma1!=sigma2


x=c(14,17,7,33,2,24,26,22,12) 
y=c(13,15,3,2,25,4,1,18,6,9,20,11,5,1,7)


var.test(x,y)

p=var.test(x,y)$p.value
alpha=0.05
p<alpha

#zatem nie ma podstaw by odrzucic H0 i mo?emy uzyc testu t

# X~N(mu1,sigma1), Y~N(mu2,sigma2), sigma1=sigma2
# H0: mu1 = mu2
# H1: mu1 > mu2 

t.test(x,y,alternative="greater",var.equal=T)


p=t.test(x,y,alternative="greater",var.equal=T)$p.value
alpha=0.05
p<alpha



################## zadanie 6  ###################
# Dodatkowo zak?adamy, ?e obie proby pochdz? z populacji normalnych!!!!
# X~N(mu1,sigma1), Y~N(mu2,sigma2)
# H0: mu1 = mu2
# H1: mu1 < mu2 
# obserwacje zalezne w parach!!!!

przed=c(27,21,34,24,30,27,33,31,22,27)
po=c(29,32,29,27,31,26,35,30,29,28)

z = przed-po
z

t.test(przed,po,alternative="less",paired=T)

p=t.test(przed,po,alternative="less",paired=T)$p.value
alpha=0.05
p<alpha

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


################## zadanie 9  ###################
# X~b(1,p1), Y~b(1,p2)
# H0: p1 = p2
# H1: p1 > p2 
# pr?by niezale?ne

k1=455
n1=700
k1/n1

k2=517
n2=1320
k2/n2

prop.test(c(k1,k2),c(n1,n2),alternative="greater")

p=prop.test(c(k1,k2),c(n1,n2),alternative="greater")$p.value
alpha=0.05
p<alpha
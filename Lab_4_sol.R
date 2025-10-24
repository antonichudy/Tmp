###########################################
###    TECHNIKI WIZUALIZACJI DANYCH     ###
###           LABORATORIUM 4            ###
###########################################


library(dplyr)
library(ggplot2)
library(SmarterPoland)

countries[countries$country == "Niue", ] # https://en.wikipedia.org/wiki/Niue
sum(countries$population) # `population` jest w tysiącach

## Zadanie 1
# 1. Ograniczyć zbiór krajów do tych, których nazwa jest krótsza niż 8 znaków (nchar).
# 2. Stworzyć zmienną logarytm populacji (*1000) 
#    i posortować względem niej poziomy zmiennej kraju (forcats::fct_reorder).
# 3. Zrobić wykres słupkowy pokazujący logarytm populacji w poszczególnych krajach 
#    i zaznaczyć kolorem kontynent (wykres poziomy).

df <- countries %>% 
  filter(nchar(country) < 8) %>% 
  mutate(log_population = log(1000*population),
         country = forcats::fct_reorder(country, log_population))

p_col <- ggplot(df, aes(y = country, x = log_population, fill = continent)) + 
  geom_col()
p_col


### Skale (scale)


## Osie (x & y) 

p_col + scale_y_discrete(position = "right")
?guide_axis

p_col + scale_y_discrete(guide = guide_axis(n.dodge = 2,
                        title = "Country", angle = 5))

#n.dodge = 2	„Rozsuwanie” etykiet na osi w dwóch rzędach,
#aby się nie nakładały (przy dużej liczbie kategorii).
#title = "Country"	Ustawia tytuł osi Y na „Country”.
#angle = 5	Obraca etykiety osi o 5 stopni, co może pomóc 
#w ich czytelności.


p_point <- ggplot(countries, aes(x = birth.rate,
                                 y = death.rate, color = continent)) + 
  geom_point()

p_point
p_point + scale_y_continuous(expand = c(0, 0))
p_point + scale_x_continuous(position = "top")

p_point + scale_y_reverse() + scale_x_reverse()

p <- ggplot(countries, aes(x = birth.rate,
                           y = population, color = continent)) +
  geom_point()

p + scale_y_log10()
p + scale_y_sqrt()

#Używasz geom_bar() wtedy, gdy nie masz gotowych wartości wysokości słupków
#— chcesz, żeby ggplot2 sam policzył, ile wystąpień ma każda kategoria.
#Używasz geom_col() wtedy, gdy sam już masz policzone wartości (np. średnie, sumy, procenty).
#Tutaj musisz podać zarówno x, jak i y.


## Kolor (color & fill)

p_point

p_point + scale_color_manual(values = c("black", "orange", "darkgreen", "blue", "red"))

p_point + scale_color_manual(
  values = c("#feedde", "#fdbe85", "#fd8d3c", "#e6550d", "#a63603")
)

# color brewer http://colorbrewer2.org/#type=sequential&scheme=BuGn&n=3
# install.packages("RColorBrewer")
library(RColorBrewer)
RColorBrewer::brewer.pal(n = 5, name = "Blues")


p_point + scale_color_manual(
  values = c("#feedde", "#6BAED6", "black", "#e6550d", "#a63603")
)


ggplot(countries, aes(x = log(population), y = death.rate,
                      color = birth.rate)) + 
  geom_point(size = 2) +
  scale_color_gradient(low = "lightblue", high = "red")


#scale_color_gradient(low = "lightblue", high = "red")
#Określa, jak wartości zmiennej birth.rate są mapowane na kolory:
#małe wartości (niski wskaźnik urodzeń) → jasnoniebieskie,
#duże wartości → czerwone.
#Tworzy gradient barwny, zamiast dyskretnych kolorów.









## Zadanie 2
# 1. Ograniczyć zbiór krajów, do tych z Azji i Europy (można wykorzystać dane z Zad1).
# 2. Policzyć stosunek współczynnika zgonów do współczynnika urodzeń.
# 3. Zrobić wykres słupkowy pokazujący logarytm populacji w poszczególnych krajach 
#    i zaznaczyć kolorem wskaźnik.

df2 <-  df %>% 
  filter(continent %in% c("Europe", "Asia")) %>% 
  mutate(index = death.rate / birth.rate)

p_col2 <- ggplot(df2, aes(y = country, x = log(population*1000), fill = index)) +
  geom_col()


p_col2 + scale_fill_gradient2(
  low = "navyblue", high = "red", mid = "white",
  limits = c(0, 1.5), midpoint = 0.5
)

### Legenda (theme & legend)

p_point

p_point + theme(legend.position = "bottom")
p_point + theme(legend.position = "none")
p_point + theme(legend.title = element_blank())


p_point +
  theme(legend.title = element_text(color = "blue", size = 15),
        legend.text = element_text(color = "red", face = "bold"))
p_col2

p_col2 + 
  labs(title = "Wykres", x = "Logarytm populacji", fill = "tytuł legendy")

### Koordynaty (coord)

p_point + coord_flip() +scale_y_reverse()
# Zamienia osie X i Y miejscami
# Odwraca kierunek osi Y — 
#wartości rosną teraz od góry do dołu (zamiast od dołu do góry)

p_point + coord_polar() + coord_cartesian()
#Zmienia układ współrzędnych z prostokątnego
#(kartezjańskiego) na biegunowy.
#Oznacza, że dane są rysowane w okręgu, a nie na płaskim układzie XY.
##Wykres punktowy zamienia się w okrągły wykres,
#podobny do „radar chart” lub wykresu kołowego
#Oś X staje się kątem, a oś Y – odległością od środka.


#to zwykły, prostokątny układ współrzędnych (domyślny w ggplot2).



# wykres kołowy (DANGER ZONE)
ggplot2::geom_pie()

tmp <-  data.frame(table(countries$continent))

ggplot(tmp, aes(x = "", y = Freq, fill = Var1)) +
  geom_col(width = 1) +
  coord_polar("y", start = 0)



ggplot(mtcars, aes(x = cyl, fill = factor(cyl))) +
  geom_bar()+
  coord_polar(theta = "y")
ggplot(mtcars, aes(x = factor(1), fill = factor(cyl))) +
  geom_bar() +
  coord_polar(theta = "y")

## Zadanie 3
# 1. Stworzyć zmienną wielkość kraju, która przyjmuje K wartości w zależności
#    od podziału zmiennej populacji (np. K = 3, można wykorzystać dane z Zad1).
# 2. Zrobić wykres punktowy pokazujący zależność death.rate od birth.rate 
#    i zaznaczyć kolorem wielkość kraju.

df3 <- df %>% mutate(
  size = case_when(
    log_population <= quantile(log_population, 0.33) ~ "small",
    log_population > quantile(log_population, 0.33) & 
      log_population <= quantile(log_population, 0.66) ~ "medium",
    log_population > quantile(log_population, 0.33) ~ "large",
  )
)

?cut_number

p <-  ggplot(df3, aes(x = birth.rate, y = death.rate, color = size)) + 
  geom_point()



#To, co pokazujesz, dotyczy paneli (facets) w ggplot2, czyli sposobu dzielenia
#wykresu na kilka mniejszych pod-wykresów (paneli) — każdy dla innej grupy
#danych (np. kontynentu, płci, kategorii itp.).

### Panele (facet)

p + facet_wrap(~continent)
p + facet_wrap(~continent, scales = "free_x")
p + facet_wrap(~continent, scales = "free_y")
p + facet_wrap(~continent, scales = "free")

p + facet_grid(size~continent)
p + facet_wrap(size~continent, ncol = 3)

### How to plot? --->>> https://www.r-graph-gallery.com


df_mean <- countries %>%
  group_by(continent) %>%
  summarise(
    mean_birth = mean(birth.rate, na.rm = TRUE),
    mean_death = mean(death.rate, na.rm = TRUE)
  )

ggplot(df_mean, aes(x = continent, y = mean_birth, fill = continent)) +
  geom_col() +
  facet_wrap(~continent)
show(countries)


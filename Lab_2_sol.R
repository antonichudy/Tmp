
###########################################
###    TECHNIKI WIZUALIZACJI DANYCH     ###
###           LABORATORIUM 2            ###
###########################################

library(dplyr) # https://dplyr.tidyverse.org/

# Dane
starwars
?starwars
starwars_new <- starwars[, c(1, 2, 3, 4, 5, 7, 8)]

# Informacje o zbiorze danych
str(starwars_new)

# Podgląd tabeli
View(starwars_new)

# Określamy typy zmiennych:
# name - jakościowa, nominalna
# height - ilościowa, ilorazowa
# mass - ilościowa, ilorazowa
# hair_color - jakościowa, nominalna 
# skin_color - jakościowa, nominalna
# birth_year - ilościowa, przedziałowa
# sex - jakościowa, binarna/nominalna

# 1) Wybór wierszy i kolumn w dplyr

# a) wybór kolumn ---> select()

select(starwars, name)

#is.na(select(starwars, name))

select(starwars, name, gender, mass)
select(starwars, gender, name, mass)

select(starwars, -name)
select(starwars, -name, -skin_color, -species)
select(starwars, -c(name, skin_color, species))

wybierz <- c("name", "gender")
select(starwars, wybierz)

wybierz <- c(1,2,3,4)
select(starwars, wybierz)


#👉 Nie wywoła błędu, jeśli w wektorze wybierz są nazwy kolumn,
#których nie ma w danych.
#Działa elastycznie — wybiera wszystkie kolumny, które faktycznie
#istnieją spośród tych
#podanych w wybierz, a te nieistniejące po prostu ignoruje.
any_of()
select(starwars, any_of(wybierz))

#👉 Wymaga, żeby wszystkie podane w wybierz kolumny istniały w danych.
#
#Jeśli choć jedna kolumna nie istnieje — zwróci błąd.
all_of()
select(starwars, all_of(wybierz))

# b) wybór wierszy ---> filter()

filter(starwars, eye_color == "blue")

# i
filter(starwars, eye_color == "blue" & hair_color == "blond")
filter(starwars, eye_color == "blue", hair_color == "blond")

# lub
filter(starwars, eye_color == "blue" | hair_color == "blond")

# 2) pipes %>% (skrót Ctrl + Shift + m)

starwars %>%
  filter(eye_color == "blue") %>% 
  select(name) %>% 
  head()



# Zadanie 1
# Używając funkcji z pakietu dplyr() wybierz te postacie, których gatunek to 
#Droid, 
# a ich wysokość jest większa niż 100.

starwars %>% 
  filter(species == "Droid", height > 100) %>% 
  select(name)

# Zadanie 2 
# Używając funkcji z pakietu dplyr() wybierz te postacie, 
# które nie mają określonego koloru włosów.

starwars %>% 
  filter(is.na(hair_color)) %>% 
  select(name)

# c) sortowanie wierszy ---> arrange()

starwars %>% 
  filter(is.na(hair_color)) %>% 
  arrange(height)

starwars %>% 
  filter(is.na(hair_color)) %>% 
  arrange(-height)

starwars %>% 
  filter(is.na(hair_color)) %>% 
  arrange(desc(height))
# malejaco

# Zadanie 3
# Używając funkcji z pakietu dplyr() wybierz postać o największej masie.

starwars %>% 
  arrange(desc(mass)) %>% 
  head(1)

starwars %>% 
  top_n(1, mass)
?top_n

# d) transformacja zmiennych ---> mutate()

starwars %>% 
  mutate(height_m = height/100) %>% 
  View()

# e) transformacja zmiennych ---> transmute()

starwars %>% 
  transmute(height_m = height/100)

# Zadanie 4
# Używając funkcji z pakietu dplyr() wylicz wskaźnik BMI (kg/m^2) i wskaż postać, która ma największy wskaźnik.

starwars %>% 
  mutate(height_m = height/100,
         BMI = mass/height_m^2) %>% 
  top_n(1, BMI) %>% 
  select(name)

# f) kolejność kolumn ---> relocate()

starwars %>% 
  relocate(sex:homeworld, .before = height)

starwars %>% 
  relocate(sex:homeworld, .after = height)

starwars %>% 
  relocate(where(is.numeric), .after = where(is.character))

# g) dyskretyzacja ---> ifelse(), case_when()

starwars %>% 
  mutate(species_new = ifelse(species == "Human", "Human", "Other")) %>% 
  select(name, species, species_new)

starwars %>% 
  mutate(species_new = case_when(species == "Human" ~ "Human",
                                 species == "Droid" ~ "Droid",
                                 TRUE ~ "Other")) %>% 
  select(name, species, species_new) %>% 
  tail()

# h) funkcje agregujące ---> summarise(), n(), mean, median, min, max, sum, sd, quantile

summary(starwars$height)

starwars %>% 
  summarise(mean_mass = mean(mass))

starwars %>% 
  summarise(mean_mass = mean(mass, na.rm = TRUE))

starwars %>% 
  filter(hair_color == "blond") %>% 
  summarise(n = n())

# i) grupowanie ---> group_by() + summarise()

starwars %>% 
  group_by(species) %>% 
  summarise(mediana = median(mass, na.rm = TRUE))

starwars %>% 
  group_by(skin_color, eye_color) %>% 
  summarise(n = n())


# 3) Przekształcenie ramki danych w tidyr
library(tidyr) # https://tidyr.tidyverse.org

# j) pivot_longer()

?relig_income
relig_income
relig_income %>% 
  pivot_longer(!religion, names_to = "income", values_to = "count")

# k) pivot_wider()

?fish_encounters
fish_encounters
fish_encounters %>% 
  pivot_wider(names_from = station, values_from = seen)

fish_encounters %>% 
  pivot_wider(names_from = station, values_from = seen, values_fill = 0)


# 4) Praca z faktorami (szczególnie w wizualizacji)
library(forcats) # https://forcats.tidyverse.org
library(ggplot2) # https://ggplot2.tidyverse.org

starwars %>%
  ggplot(aes(x = eye_color)) + 
  geom_bar() + 
  coord_flip()

starwars %>% 
  select(eye_color) %>%
  pull %>%
  table %>%
  sort

# l) kolejność poziomów ---> fct_infreq()

starwars %>%
  mutate(eye_color = fct_infreq(eye_color)) %>%
  ggplot(aes(x = eye_color)) + 
  geom_bar() + 
  coord_flip()

# m) scalanie poziomów ---> fct_lump()

starwars %>%
  mutate(species = fct_lump(species, n = 5)) %>%
  count(species)

starwars %>%
  mutate(species = fct_lump(species, n = 5)) %>%
  ggplot(aes(x = species)) + 
  geom_bar() + 
  coord_flip()


# n) kolejność poziomów na podstawie innej zmiennej ---> fct_reorder()
starwars %>%
  filter(!is.na(species)) %>%
  summarise(mean_mass = mean(mass, na.rm = TRUE), .by = species) %>%
  ggplot(aes(x = reorder(species, mean_mass), y = mean_mass)) +
  geom_col() + coord_flip()



# 4) Praca z stringami
# Zaawansowane: https://stringi.gagolewski.com
library(stringr) # https://stringr.tidyverse.org
letters
?paste0
x <- paste0(letters[1:5], "=", 1:5, "__", letters[6:10], "=", 6:10)
x
# o) podział stringów ---> str_split()

str_split(x, "__")

# p) usunięcie/zastąpienie znaków ---> str_remove(), str_replace()
str_remove("abc123", "123")
# wynik: "abc"

str_replace("abc123", "123", "_NUM_")
# wynik: "abc_NUM_"




# 5) https://www.tidyverse.org/packages
# Bezpieczne wczytywanie danych w R https://readr.tidyverse.org

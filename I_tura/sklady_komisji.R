
# żródło danych  ----------------------------------------------------------

# https://wybory.gov.pl/prezydent20200628/pl/dane_w_arkuszach

# ładowanie pakietów  -----------------------------------------------------

library(tidyverse) # must have 
library(sf) # do map 
library(stringr) # do przetwarzania napisów 


# ładowanie danych  -------------------------------------------------------

obwody <- read_csv2("sklad_komisji_obwodowych.csv")


# podejrzyjmy dane 

head(obwody)


# sprawdzenie - kobieta czy mężczyzna? ------------------------------------

# zanim skorzystamy z funkcji, podzielmy kolumnę "Imiona" w ten sposób, aby każde imię było 
# w oddzielnej kolumnie

obwody <-  obwody %>% 
  separate(col = Imiona, sep = "\\s", 
           into = c("pierwsze_imie", "drugie_imie","trzecie_imie"))

# sprawdzone "ręcznie" - maksymalnie występują trzy imiona 

# to czy członek komisji jest kobietą czy mężczyzną osądzimy na podstawie pierwszego imienia 
# zasada - polskie imiona żeńskie kończą się na literę "a", z niektórymi wyjątkami 

# 
plec <-  character(dim(obwody)[1])

# wyjątki - polskie imiona męskie kończące się literą "a"
wyjatki <- c("Bonawentura", "Barnaba", "Kosma", "Dyzma", "Juda")


for (i in 1:dim(obwody)[1]){
  if(str_detect(obwody$pierwsze_imie[i], paste(wyjatki, collapse = "|"))){
    plec[i] = "mezczyzna"  # jeżeli imię występuje w podanych wyjątkach - to mężczyzna
  } else {
    if(str_ends(obwody$pierwsze_imie[i], "a")){
      plec[i] = "kobieta"      # jeżeli imię kończy się na "a" to kobieta 
    } else {
      plec[i] = "mezczyzna"  # pozostałe przypadki - to mężczyzna 
    }
  }
}


# stworzenie nowej kolumny - płeć 
obwody <-  obwody %>% 
  mutate(plec = plec)


# plik shp z mapą Polski z podziałem według gmin 
# plik "jednostki_ewidencyjne" - zawiera szczegółowy podział Warszawy na dzielnice 

plik_shp_mapa <- "C:/Users/Radek/Desktop/!PROJEKTY_R/wybory2020/pliki_shp/Jednostki_ewidencyjne.shp"

# plik_shp_mapa <- "./pliki_shp/Gminy.shp"

# "C:/Users/Radek/Desktop/!PROJEKTY_R/wybory2020/pliki_shp/Jednostki_ewidencyjne.shp"

# mapa Polski według gmin  ------------------------------------------------

# plik shp jest dosyć duży - stąd wybrane są tylko kolumny JPT_KOD_JE (to nic innego jak kod TERYT 
# więc tylko zmiana nazwy) oraz geometry czyli listy z współrzędnymi granic obszarów 

# Należy poprawić Zieloną Górę (gminę miejską, nie miasto) oraz Łódź i Kraków - które są podzielone na 
# dzielnice natomiast wyniki z PKW traktują to jako ten sam kod TERYT 

mapa <- read_sf(plik_shp_mapa) %>%
  select(TERYT = JPT_KOD_JE, geometry, JPT_NAZWA_) %>%
  mutate(TERYT = str_sub(TERYT, 1, 6)) %>%  # skrócenie kodu teryt do 6 cyfr 
  mutate(TERYT = if_else(str_detect(TERYT, "080910"), "086201", TERYT),
         TERYT = if_else(str_detect(TERYT, "106102"), "106101", TERYT),
         TERYT = if_else(str_detect(TERYT, "106103"), "106101", TERYT),
         TERYT = if_else(str_detect(TERYT, "106104"), "106101", TERYT),
         TERYT = if_else(str_detect(TERYT, "106105"), "106101", TERYT),
         TERYT = if_else(str_detect(TERYT, "106106"), "106101", TERYT),
         TERYT = if_else(str_detect(TERYT, "126102"), "126101", TERYT),
         TERYT = if_else(str_detect(TERYT, "126103"), "126101", TERYT),
         TERYT = if_else(str_detect(TERYT, "126104"), "126101", TERYT),
         TERYT = if_else(str_detect(TERYT, "126105"), "126101", TERYT))

# dobrna zmiana nazwy kolumny 
obwody <- obwody %>% 
  rename(TERYT = "TERYT gminy")



# przewodniczący komisji - kobieta czy mężczyzna? -------------------------


obwody_mapa <- left_join(mapa, obwody %>% 
                           select(TERYT, Funkcja, plec) %>% 
                           filter(Funkcja == "Przewodniczący"), by = "TERYT")


przewodniczacy_plec <- obwody_mapa %>%
  ggplot() +
  geom_sf(mapping = aes(fill = plec), size = 0.1, color = "gray95") + 
  scale_fill_manual(values = c("orchid1", "royalblue3")) + 
  theme_void() + 
  theme(legend.position = "bottom", legend.direction = "horizontal",
        legend.title = element_blank(),
        legend.text = element_text(size = 8),
        legend.key.size = unit(10, "points"),
        plot.title = element_text(face = "bold", size = 11),
        plot.subtitle = element_text(size = 9),
        plot.caption = element_text(size = 7, colour = "grey30"), 
        plot.margin = unit(c(5,5,5,5), "points")) +
  labs(title = "Przewodniczący komisji - kobieta czy mężczyzna?",
       subtitle = "I tura, 28.06.2020",
       caption = "Radosław Pałkowski \n github.com/rpalkowski",
       fill = "płeć")

ggsave(przewodniczacy_plec, file = "przewodniczacy_plec.png", width=6, height=6, dpi=300)



# płeć przeważająca w komisji/obwodach ---------------------------------------------


plec <- obwody %>% 
  group_by(TERYT) %>% 
  count(plec) %>% 
  ungroup() %>% 
  spread(key = plec, value = n) %>% 
  mutate(plec_przewazajaca = kobieta > mezczyzna)

plec$plec_przewazajaca <- gsub("TRUE", "kobieta", plec$plec_przewazajaca)


plec_mapa <- left_join(mapa, plec %>% 
                         select(TERYT, plec_przewazajaca), by = "TERYT")


plec_przewazajaca <- plec_mapa %>%
  ggplot() +
  geom_sf(mapping = aes(fill = plec_przewazajaca), size = 0.1, color = "gray95") + 
  scale_fill_manual(values = c("royalblue3", "orchid1")) + 
  theme_void() + 
  theme(legend.position = "bottom", legend.direction = "horizontal",
        legend.title = element_blank(),
        legend.text = element_text(size = 8),
        legend.key.size = unit(10, "points"),
        plot.title = element_text(face = "bold", size = 11),
        plot.subtitle = element_text(size = 9),
        plot.caption = element_text(size = 7, colour = "grey30"), 
        plot.margin = unit(c(5,5,5,5), "points")) +
  labs(title = "Przeważająca płeć w komisjach",
       subtitle = "I tura, 28.06.2020",
       caption = "Radosław Pałkowski \n github.com/rpalkowski",
       fill = "płeć")

ggsave(plec_przewazajaca, file = "plec_przewazajaca.png", width=6, height=6, dpi=300)


# Pierwsza tura wyborów prezydenckich -------------------------------------

# źródło danych: https://wybory.gov.pl/prezydent20200628/pl/dane_w_arkuszach
# źródło plików shp: https://gis-support.pl/granice-administracyjne/

# załadowanie pakietów  ---------------------------------------------------

library(tidyverse)
library(sf)
library(stringr)


# ładowanie danych  -------------------------------------------------------

dane <- read_csv2("./I_tura/wyniki_gl_na_kand_po_obwodach_utf8.csv")

# plik shp z mapą Polski z podziałem według gmin 
# plik "jednostki_ewidencyjne" - zawiera podział Warszawy

plik_shp_mapa <- "./pliki_shp/Jednostki_ewidencyjne.shp"

# plik_shp_mapa <- "./pliki_shp/Gminy.shp"


# mapa Polski według gmin  ------------------------------------------------

# plik shp jest dosyć duży - stąd wybrane są tylko kolumny JPT_KOD_JE (to nic innego jak kod TERYT 
# więc tylko zmiana nazwy) oraz geometry czyli listy z współrzędnymi granic obszarów 

# Należy poprawić Zieloną Górę (gminę miejską, nie miasto) oraz Łódź i Kraków - które są podzielone na 
# dzielnice natomiast wyniki z PKW traktują to jako ten sam kod TERYT 

mapa <- read_sf(plik_shp_mapa) %>%
  select(TERYT = JPT_KOD_JE, geometry) %>%
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



# x <- read_sf(plik_shp_mapa) %>%
#   select(TERYT = JPT_KOD_JE, JPT_NAZWA_) %>% 
#   filter(JPT_NAZWA_ == "Podgórze")

# 
# 106104 polesie 
# 106105 srodmiescie 
# 06 WIDZEW
# 02 bałuty
# 03 górna



# 126105 srodmiescie 
# 03 nowa huta 
# 02 krowodrza 
# 04 podgórze 



# drobne zmiany w zbiorze danych - str_sub zmienia długość kodu TERYT na równą długość dla wszystkich 
# użycie ` ` pozwala na wybranie nazw zmiennych ze spacją  

dane <- dane %>% 
  mutate(`Kod TERYT` = str_sub(`Kod TERYT`, 1, 6))


# frekwencja w lokalach ---------------------------------------------------
# wyborcy uprawnieni do gł♠osowania / wydane karty do głosowania 


# nazwy i numery kolumn
colnames(dane)

# wybór odpowiednich kolumn + zmiana nazw i obliczenie frekwencji 
frekwencja <- dane[, c(3, 13, 15)] %>% 
  set_names("TERYT", "uprawnieni_wyborcy", "wydane_karty") %>% 
  group_by(TERYT) %>% 
  summarise(uprawnieni_wyborcy = sum(uprawnieni_wyborcy), 
            wydane_karty = sum(wydane_karty)) %>% 
  ungroup() %>% 
  mutate(frekwencja_proc = (wydane_karty / uprawnieni_wyborcy)*100)

# c(3,13,15) - po obwodach 
# c(2,8,10) - po gmianch 

frekwencja_do_mapy <- left_join(mapa, 
                             frekwencja %>% 
                               select(TERYT, frekwencja_proc), 
                             by = "TERYT")

frekwencja_mapa <- ggplot() +
  #geom_sf(data = mapa, size = 0.15, color = "gray", fill = "white") + 
  geom_sf(data = frekwencja_do_mapy, aes(fill = frekwencja_proc),  size = 0.15, color = "gray") +
  scale_fill_distiller(palette = "Oranges", direction = 1) +
  theme_void()

# zapis pliku 
ggsave(frekwencja_mapa, file = "frekwencja_mapa_obwody.png", width=20, height=20, dpi=300)  




# # mapa z samymi nazwami jednostek teryt 
# test <- ggplot(mapa) +
#   geom_sf(data = mapa, size = 0.1, color = "gray90", fill = "white") + 
#   #geom_sf(data = frekwencja_do_mapy, aes(fill = frekwencja_proc),  size = 0.1, color = "gray90") +
#   #scale_fill_distiller(palette = "Oranges", direction = 1) +
#   geom_sf_text(aes(label = JPT_NAZWA_), colour = "black", size = 1)
# 
# # zapis pliku 
# ggsave(test, file = "test.png", width=20, height=20, dpi=300)  



# głosy nieważne  ---------------------------------------------------------

niewazne <- dane[, c(3, 25, 29, 33, 27)] %>% 
  set_names("TERYT", "glosy_oddane", "glosy_niewazne", "glosy_wazne", "karty_niewazne")



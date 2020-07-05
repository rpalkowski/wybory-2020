
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


# drobne zmiany w zbiorze danych - str_sub zmienia długość kodu TERYT na równą długość dla wszystkich 
# użycie ` ` pozwala na wybranie nazw zmiennych ze spacją  

dane <- dane %>% 
  mutate(`Kod TERYT` = str_sub(`Kod TERYT`, 1, 6))


# frekwencja w lokalach ---------------------------------------------------
# wyborcy uprawnieni do głosowania / wydane karty do głosowania 


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

# łączenie z granicami obszarów po kodzie TERYT
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


# głosy ważne i nieważne --------------------------------------------------

glosy <- dane[, c(3, 25, 29, 33, 27)] %>% 
  set_names("TERYT", "glosy_oddane", "glosy_niewazne", "glosy_wazne", "karty_niewazne") %>% 
  group_by(TERYT) %>% 
  summarise(glosy_oddane = sum(glosy_oddane),
            glosy_niewazne = sum(glosy_niewazne), 
            glosy_wazne = sum(glosy_wazne), 
            karty_niewazne = sum(karty_niewazne)) %>% 
  ungroup() %>% 
  mutate(glosy_niewazne_proc = (glosy_niewazne / glosy_oddane)*100,
         glosy_wazne_proc = (glosy_wazne / glosy_oddane)*100,
         karty_niewazne_proc = (karty_niewazne / glosy_oddane)*100)

# połączenie z tabelą ze współrzędnymi
glosy_mapa <- left_join(mapa, glosy, by = "TERYT")


# głosy nieważne  ---------------------------------------------------------

glosy_niewazne <- glosy_mapa %>% 
  ggplot() +
  geom_sf(mapping = aes(fill = glosy_niewazne),  size = 0.15, color = "gray") +
  scale_fill_distiller(palette = "PuRd", direction = 1) +
  theme_minimal() +
  labs(title = "Głosy nieważne w wyborach prezydenckich 2020 (I tura) na poziomie gmin",
       subtitle = "karty_niewazne / glosy_oddane",
       caption = "Radosław Pałkowski",
       fill = "", color="")
  

# zapis pliku 
ggsave(glosy_niewazne, file = "glosy_niewazne_proc.png", width=12, height=12, dpi=300)




# zwycięzcy według gmin ---------------------------------------------------

wyniki <- dane[, c(3, 33, 34:44)] %>%
  rename(TERYT = `Kod TERYT`) %>% 
  gather(3:13, key = "kandydat", value = "glosy") %>%
  group_by(TERYT, kandydat) %>% 
  summarise(glosy_na_kandydata = sum(glosy)) %>% 
  ungroup() %>% 
  left_join(glosy %>% select(TERYT, glosy_wazne), by = "TERYT") %>% 
  mutate(glosy_na_kandydata_proc = (glosy_na_kandydata / glosy_wazne)*100)

zwyciezcy <- left_join(mapa, 
                       wyniki %>% 
                         group_by(TERYT) %>% 
                         top_n(1, glosy_na_kandydata), 
                      by = "TERYT")



mapa_zwyciezcy_gminy = zwyciezcy %>%
  ggplot() +
  geom_sf(aes(fill = kandydat), size = 0.1, color = "gray90") +
  scale_fill_manual(values = c("blue4", "cyan3", "yellow")) +
  theme_void() + 
  theme(legend.position="bottom", legend.box = "horizontal",
        legend.title = element_text(size = 16),
        legend.text = element_text(size = 16),
        plot.title = element_text(face = "bold", size = 22),
        plot.caption = element_text(size = 12, colour = "grey70")) +
  labs(title = "Zwycięzcy na poziomie gmin",
       subtitle = "I tura, 28.06.2020",
       caption = "Radosław Pałkowski \n github.com/rpalkowski",
       fill = "", color="")
  
ggsave(mapa_zwyciezcy_gminy, file = "zwyciezcy_okregow.png", width=12, height=12, dpi=300)

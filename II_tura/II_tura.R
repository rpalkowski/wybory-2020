
# Druga tura wyborów prezydenckich -------------------------------------

# źródło danych: https://wybory.gov.pl/prezydent20200628/pl/dane_w_arkuszach
# źródło plików shp: https://gis-support.pl/granice-administracyjne/

# załadowanie pakietów  ---------------------------------------------------

library(tidyverse)
library(sf)
library(stringr)


# ładowanie danych  -------------------------------------------------------

dane <- read_csv2("wyniki_gl_na_kand_po_obwodach_utf8.csv")

# plik shp z mapą Polski z podziałem według gmin 
# plik "jednostki_ewidencyjne" - zawiera szczegółowy podział Warszawy na dzielnice 

shp_gminy <- "./pliki_shp/Jednostki_ewidencyjne.shp"

shp_wojewodztwa <- "./pliki_shp/Województwa.shp"



# mapa Polski według gmin  ------------------------------------------------

# plik shp jest dosyć duży - stąd wybrane są tylko kolumny JPT_KOD_JE (to nic innego jak kod TERYT 
# więc tylko zmiana nazwy) oraz geometry czyli listy z współrzędnymi granic obszarów 

# Należy poprawić Zieloną Górę (gminę miejską, nie miasto) oraz Łódź i Kraków - które są podzielone na 
# dzielnice natomiast wyniki z PKW traktują to jako ten sam kod TERYT 

mapa_gminy <- read_sf(shp_gminy) %>%
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

mapa_woj <- read_sf(shp_wojewodztwa) %>% 
  select(TERYT = JPT_KOD_JE, geometry, JPT_NAZWA_) %>% 
  mutate(TERYT = str_sub(TERYT, 1, 6))



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


# frekwencja do poprawy - albo liczona razem z korespondencyjnym albo oddzielnie
# (trzeba odjąć od głosów wyjętych)

frekwencja_do_mapy <- left_join(mapa_gminy, 
                             frekwencja %>% 
                               select(TERYT, frekwencja_proc), 
                             by = "TERYT")


frekwencja_mapa <- ggplot() +
  geom_sf(data = frekwencja_do_mapy, aes(fill = frekwencja_proc),  size = 0.08, color = "gray95") +
  geom_sf(data = mapa_woj, size = 0.2, color = "gray30", fill = NA) +
  geom_sf_text(data = frekwencja_do_mapy %>% filter(frekwencja_proc == 0),
               aes(label = JPT_NAZWA_), colour = "black", size = 1.25, fontface = "bold", 
               position = position_nudge(y = -0.1)) +
  scale_fill_distiller(palette = "Greens", direction = 1) +
  theme_void() + 
  theme(legend.position = c(0.20, 0.10), legend.direction = "horizontal",
        legend.title = element_text(size = 7),
        legend.text = element_text(size = 6),
        legend.key.size = unit(10, "points"),
        plot.title = element_text(face = "bold", size = 11),
        plot.subtitle = element_text(size = 9),
        plot.caption = element_text(size = 7, colour = "grey30"), 
        plot.margin = unit(c(5,3,5,3), "points")) +
  labs(title = "Frekwencja w wyborach prezydenckich 2020 (I tura) na poziomie gmin",
       subtitle = "Tylko lokale wyborcze, bez głosowania korespondencyjnego\n W gminach Baranów i Marklowice przeprowadzono wyłącznie głosowanie korespondencyjne",
       caption = "Radosław Pałkowski \n github.com/rpalkowski", 
       fill = "Frekwencja (%)")



# zapis pliku 
# ggsave(frekwencja_mapa, file = "frekwencja_mapa_gminy.png", width=6, height=6, dpi=300)



# # mapa z samymi nazwami jednostek teryt 
# test <- ggplot(mapa) +
#   geom_sf(data = mapa, size = 0.1, color = "gray90", fill = "white") + 
#   geom_sf_text(aes(label = JPT_NAZWA_), colour = "black", size = 1)
# 
# # zapis pliku 
# ggsave(test, file = "test.png", width=20, height=20, dpi=300)  




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
glosy_mapa <- left_join(mapa_gminy, glosy, by = "TERYT")


glosy_niewazne_mapa <- glosy_mapa %>% 
  ggplot() +
  geom_sf(mapping = aes(fill = glosy_niewazne_proc),  size = 0.10, color = "gray95") +
  geom_sf(data = mapa_woj, size = 0.2, color = "gray30", fill = NA) +
  scale_fill_distiller(palette = "Reds", direction = 1) +
  theme_void() +
  theme(legend.position = c(0.20, 0.10), legend.direction = "horizontal",
      legend.title = element_text(size = 7),
      legend.text = element_text(size = 6),
      legend.key.size = unit(10, "points"),
      plot.title = element_text(face = "bold", size = 11),
      plot.subtitle = element_text(size = 9),
      plot.caption = element_text(size = 7, colour = "grey30"), 
      plot.margin = unit(c(5,3,5,3), "points")) +
  labs(title = "Głosy nieważne w wyborach prezydenckich 2020 (I tura) na poziomie gmin",
       caption = "Radosław Pałkowski \n github.com/rpalkowski", 
       fill = "Głosy nieważne (%)")
  

# zapis pliku 
# ggsave(glosy_niewazne_mapa, file = "glosy_niewazne_proc.png", width=6, height=6, dpi=300)



# zwycięzcy według gmin ---------------------------------------------------

wyniki <- dane[, c(3, 33, 34:44)] %>%
  rename(TERYT = `Kod TERYT`) %>% 
  gather(3:13, key = "kandydat", value = "glosy") %>%
  group_by(TERYT, kandydat) %>% 
  summarise(glosy_na_kandydata = sum(glosy)) %>% 
  ungroup() %>% 
  left_join(glosy %>% select(TERYT, glosy_wazne), by = "TERYT") %>% 
  mutate(glosy_na_kandydata_proc = (glosy_na_kandydata / glosy_wazne)*100)

wyniki_mapa <- left_join(mapa_gminy, 
                       wyniki %>% 
                         group_by(TERYT) %>% 
                         top_n(1, glosy_na_kandydata), 
                      by = "TERYT")


mapa_zwyciezcy_gminy <- wyniki_mapa %>%
  ggplot() +
  geom_sf(aes(fill = kandydat), size = 0.1, color = "gray95") +
  geom_sf(data = mapa_woj, size = 0.2, color = "gray30", fill = NA) +
  scale_fill_manual(values = c("dodgerblue3", "cyan3", "yellow1")) +
  theme_void() + 
  theme(legend.position = "bottom", legend.direction = "horizontal",
        legend.title = element_blank(),
        legend.text = element_text(size = 8),
        legend.key.size = unit(10, "points"),
        plot.title = element_text(face = "bold", size = 11),
        plot.subtitle = element_text(size = 9),
        plot.caption = element_text(size = 7, colour = "grey30"), 
        plot.margin = unit(c(5,5,5,5), "points")) +
  labs(title = "Kandydaci z największą liczbą głosów na poziomie gmin",
       subtitle = "I tura, 28.06.2020",
       caption = "Radosław Pałkowski \n github.com/rpalkowski",
       fill = "", color="")
  
# ggsave(mapa_zwyciezcy_gminy, file = "zwyciezcy_gmin.png", width=6, height=6, dpi=300)


# poparcie ----------------------------------------------------------------


poparcie <- left_join(mapa_gminy, wyniki, by = "TERYT")

# wszyscy kandydaci razem na jednym wykresie 
mapa_poparcie_proc <- poparcie %>% 
  ggplot() +
  geom_sf(mapping = aes(fill = glosy_na_kandydata_proc), size = 0.1, color = "gray90") +
  geom_sf(data = mapa_woj, size = 0.2, color = "gray30", fill = NA) +
  scale_fill_distiller(palette = "Blues", direction = 1) +
  facet_wrap(~kandydat) +
  theme_void() + 
  theme(legend.position = c(0.90, 0.20), legend.direction = "horizontal",
        legend.title = element_text(size = 9),
        legend.text = element_text(size = 8),
        legend.key.size = unit(15, "points"),
        plot.title = element_text(face = "bold", size = 11),
        plot.subtitle = element_text(size = 9),
        plot.caption = element_text(size = 7, colour = "grey30"), 
        plot.margin = unit(c(5,5,5,5), "points")) +
  labs(title = "Poparcie dla wszystkich kandydatów (I tura)",
       subtitle = " ", 
       caption = "Radosław Pałkowski \n github.com/rpalkowski",
       fill = "Głosy (%)")
  
# ggsave(mapa_poparcie_proc, file = "poparcie_proc.png", width=30, height=15, dpi=300)






# poparcie dla Rafała Trzaskowskiego 
mapa_poparcie_proc_rt <- poparcie %>%
  filter(kandydat == "Rafał Kazimierz TRZASKOWSKI") %>% 
  ggplot() +
  geom_sf(mapping = aes(fill = glosy_na_kandydata_proc), size = 0.1, color = "gray100") +
  geom_sf(data = mapa_woj, size = 0.2, color = "gray30", fill = NA) +
  scale_fill_distiller(palette = "Blues", direction = 1) +
  theme_void() +
  theme(legend.position = c(0.20, 0.10), legend.direction = "horizontal",
        legend.title = element_text(size = 7),
        legend.text = element_text(size = 6),
        legend.key.size = unit(10, "points"),
        plot.title = element_text(face = "bold", size = 11),
        plot.subtitle = element_text(size = 9),
        plot.caption = element_text(size = 7, colour = "grey30"), 
        plot.margin = unit(c(5,5,5,5), "points")) +
  labs(title = "Poparcie na poziomie gmin - Rafał Kazimierz TRZASKOWSKI",
       subtitle = "I tura, 28.06.2020",
       caption = "Radosław Pałkowski \n github.com/rpalkowski",
       fill = "Procent głosów")

# ggsave(mapa_poparcie_proc_rt, file = "poparcie_proc_rt.png", width=6, height=6, dpi=300)


# poparcie dla Andrzeja Dudy 
mapa_poparcie_proc_ad = poparcie %>%
  filter(kandydat == "Andrzej Sebastian DUDA") %>% 
  ggplot() +
  geom_sf(mapping = aes(fill = glosy_na_kandydata_proc), size = 0.1, color = "gray100") +
  geom_sf(data = mapa_woj, size = 0.2, color = "gray30", fill = NA) +
  scale_fill_distiller(palette = "Blues", direction = 1) +
  theme_void() +
  theme(legend.position = c(0.20, 0.10), legend.direction = "horizontal",
        legend.title = element_text(size = 7),
        legend.text = element_text(size = 6),
        legend.key.size = unit(10, "points"),
        plot.title = element_text(face = "bold", size = 11),
        plot.subtitle = element_text(size = 9),
        plot.caption = element_text(size = 7, colour = "grey30"), 
        plot.margin = unit(c(5,5,5,5), "points")) +
  labs(title = "Poparcie na poziomie gmin - Andrzej Sebastian DUDA",
       subtitle = "I tura, 28.06.2020",
       caption = "Radosław Pałkowski \n github.com/rpalkowski",
       fill = "Procent głosów")

# ggsave(mapa_poparcie_proc_ad, file = "poparcie_proc_ad.png", width=6, height=6, dpi=300)






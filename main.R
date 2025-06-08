# Importowanie biblioteki readr --------------------------------------------------
library(readr)

# Ustawienie ścieżki folderu roboczego -----------------------------------
sciezka_folderu <- "C:/Users/pavli/OneDrive/Робочий стіл/Analiza_wyn_sr/Analiza-wynikow-srednich-egzaminow-maturalnych"

# Importowanie danych z pliku CSV ----------------------------------------
srednie_wyniki_egzaminu_maturalnego <- read_csv2(
  file = file.path(sciezka_folderu, "srednie_wyniki_egzaminu_maturalnego_2.csv"), 
  locale = locale(encoding = "UTF-8")
)

# Usuwanie niepotrzebnych kolumn -----------------------------------------
data_cleaned <- srednie_wyniki_egzaminu_maturalnego[, !(names(srednie_wyniki_egzaminu_maturalnego) %in% c("nazwa_zmiennej", "kraj", "typ_informacji_z_jednostka_miary"))]

# Usuwanie wierszy, gdzie flaga to "(.)" lub "(–)" ------------------------
data_cleaned <- data_cleaned[!(data_cleaned$flaga %in% c("(.)", "(–)")), ]

# Usuwanie kolumny "flaga" po przefiltrowaniu -----------------------------
data_cleaned$flaga <- NULL  # Kolumna "flaga" nie jest już potrzebna, więc ją usuwamy

# Eksport przetworzonych danych do nowego pliku CSV -----------------------
write_csv2(
  data_cleaned, 
  file.path(sciezka_folderu, "srednie_wyniki_cleaned.csv")  # Zapisujemy dane do nowego pliku w tym samym folderze
)

# Dzielenie ramek danych --------------------------------------------------
# Ramka danych dla "ogółem"
data_general <- data_cleaned[data_cleaned$plec == "ogółem", ]
data_general$plec <- NULL
# Ramka danych dla pozostałych płci
data_by_gender <- data_cleaned[data_cleaned$plec != "ogółem", ]

# Importowanie biblioteki ggplot2 --------------------------------------------------
library(ggplot2)
# Wykres słupkowy przedstawiający średnie wyniki z poszczególnych przedmiotów w różnych latach
plot1 <- ggplot(data_general, aes(x = przedmiot, y = as.numeric(wartosc), fill = rok)) +
  geom_col(position = "dodge") +
  labs(title = "Średnie wyniki z przedmiotów (ogółem)", y = "Średni wynik", x = "Przedmiot") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave(file.path(sciezka_folderu, "wykres_srednie_ogolem.png"), plot = plot1, width = 10, height = 6, dpi = 300)

# Wykres słupkowy/boxplot z wynikami wg płci.
plot2 <- ggplot(data_by_gender, aes(x = przedmiot, y = as.numeric(wartosc), fill = plec)) +
  geom_boxplot() +
  labs(title = "Porównanie wyników wg płci", y = "Wynik", x = "Przedmiot") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave(file.path(sciezka_folderu, "wykres_plec_boxplot.png"), plot = plot2, width = 10, height = 6, dpi = 300)

# Importowanie biblioteki dplyr ------------------------------------------------
library(dplyr)

# Filtruj tylko matematykę
math_trend <- filter(data_general, przedmiot == "matematyka")
math_trend_avg <- math_trend %>%
  group_by(rok) %>%
  summarise(sredni_wynik = mean(as.numeric(wartosc), na.rm = TRUE))
# Srednia zmiana wyników w czasie matematyka
plot3 <- ggplot(math_trend_avg, aes(x = rok, y = sredni_wynik)) +
  geom_line() +
  geom_point() +
  scale_x_continuous(breaks = math_trend_avg$rok) +
  labs(title = "Średni trend wyników z matematyki w czasie", x = "Rok", y = "Średni wynik (%)")

ggsave(file.path(sciezka_folderu, "trend_matematyka.png"), plot = plot3, width = 10, height = 6, dpi = 300)

#  Analiza dysproporcji płci ----------------------------------------------
library(tidyr)

gender_diff <- data_by_gender %>% #przekazuje dane z ramki
  group_by(rok, przedmiot, plec) %>% #przydziela grupy wierszom
  summarise(wynik = mean(as.numeric(wartosc), na.rm = TRUE)) %>% # obilcza średnie
  pivot_wider(names_from = plec, values_from = wynik) %>% #eliminuje "plec" i "wynik" i przetwarza dane dla dwu nowych kolumn - "kobiety" oraz "mężczyźni"
  mutate(roznica = kobiety - mężczyźni) #tworzy nową kolumnę z róźnicami

gender_diff <- na.omit(gender_diff)

gender_diff_avg <- gender_diff %>%
  group_by(przedmiot) %>%
  summarise(srednia_roznica = mean(roznica))
# Różnice płci
plot4 <- ggplot(gender_diff_avg, aes(x = reorder(przedmiot, srednia_roznica), y = srednia_roznica, fill = srednia_roznica > 0)) +
  geom_col() +
  coord_flip() +
  labs(
    title = "Średnia różnica wyników matur (kobiety - mężczyźni)",
    x = "Przedmiot",
    y = "Średnia różnica (%)"
  ) +
  scale_fill_manual(values = c("TRUE" = "tomato", "FALSE" = "steelblue"),
                    name = "Kto wypadł lepiej?", labels = c("Mężczyźni", "Kobiety")) +
  theme_minimal()

ggsave(file.path(sciezka_folderu, "roznica_plec.png"), plot = plot4, width = 10, height = 6, dpi = 300)


# 1. Średnie wartości z każdego przedmiotu w danym roku
subject_matrix <- data_general %>%
  mutate(wartosc = as.numeric(wartosc)) %>%
  group_by(rok, przedmiot) %>%
  summarise(wartosc = mean(wartosc, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(names_from = przedmiot, values_from = wartosc)

# 2. Obliczenie macierzy korelacji (bez kolumny 'rok')
cor_matrix <- cor(subject_matrix[,-1], use = "pairwise.complete.obs")

# 3. Przekształcenie korelacji do formatu długiego
cor_long <- as.data.frame(as.table(cor_matrix)) %>%
  rename(Przedmiot_1 = Var1, Przedmiot_2 = Var2, Korelacja = Freq)

# 4. Heatmapa dla wszystkich korelacji
library(ggplot2)

plot5 <- ggplot(cor_long, aes(x = Przedmiot_1, y = Przedmiot_2, fill = Korelacja)) +
  geom_tile(color = "white") +
  scale_fill_gradient2(
    low = "steelblue", high = "darkred", mid = "white",
    midpoint = 0, limit = c(-1,1), space = "Lab",
    name = "Korelacja"
  ) +
  labs(
    title = "Macierz korelacji między średnimi wynikami z przedmiotów",
    x = "Przedmiot", y = "Przedmiot"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
    axis.text.y = element_text(face = "bold"),
    panel.grid = element_blank()
  ) +
  coord_fixed()

ggsave(file.path(sciezka_folderu, "heatmapa_korelacji.png"), plot = plot5, width = 10, height = 8, dpi = 300)


# 1. Przekształcenie i agregacja danych
wos_lacina_long <- data_general %>%
  filter(przedmiot %in% c("wiedza o społeczeństwie", "język łaciński i kultura antyczna")) %>%
  mutate(wartosc = as.numeric(wartosc)) %>%
  group_by(rok, przedmiot) %>%
  summarise(wartosc = mean(wartosc, na.rm = TRUE), .groups = "drop")

# 2. Tabela porównawcza (wide)
comparison <- wos_lacina_long %>%
  pivot_wider(names_from = przedmiot, values_from = wartosc) %>%
  mutate(lepszy_przedmiot = ifelse(`wiedza o społeczeństwie` > `język łaciński i kultura antyczna`,
                                   "wiedza o społeczeństwie", "język łaciński i kultura antyczna"))

# 3. Połączenie danych
wos_lacina_long <- left_join(wos_lacina_long, comparison %>% select(rok, lepszy_przedmiot), by = "rok")

# 4. Obliczenie współczynnika korelacji
r_val <- round(cor(comparison$`wiedza o społeczeństwie`, comparison$`język łaciński i kultura antyczna`, use = "complete.obs"), 3)

# 5. Wykres słupkowy
plot6 <- ggplot(wos_lacina_long, aes(x = factor(rok), y = wartosc, fill = przedmiot)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(~lepszy_przedmiot, scales = "free_x") +
  labs(
    title = paste("Porównanie wyników: WOS vs. Łacina\n(Współczynnik korelacji:", r_val, ")"),
    x = "Rok",
    y = "Średni wynik (%)",
    fill = "Przedmiot"
  ) +
  scale_fill_manual(values = c("wiedza o społeczeństwie" = "steelblue",
                               "język łaciński i kultura antyczna" = "darkorange")) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave(file.path(sciezka_folderu, "porownanie_wos_lacina.png"), plot = plot6, width = 10, height = 6, dpi = 300)


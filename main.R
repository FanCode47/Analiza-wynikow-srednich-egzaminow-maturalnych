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
ggplot(data_general, aes(x = przedmiot, y = as.numeric(wartosc), fill = rok)) +
  geom_col(position = "dodge") +
  labs(title = "Średnie wyniki z przedmiotów (ogółem)", y = "Średni wynik", x = "Przedmiot") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
# Wykres słupkowy/boxplot z wynikami wg płci.
ggplot(data_by_gender, aes(x = przedmiot, y = as.numeric(wartosc), fill = plec)) +
  geom_boxplot() +
  labs(title = "Porównanie wyników wg płci", y = "Wynik", x = "Przedmiot") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Importowanie biblioteki dplyr ------------------------------------------------
library(dplyr)

# Filtruj tylko matematykę
math_trend <- data_general %>% filter(przedmiot == "matematyka")
# Zmiana wyników w czasie (np. matematyka, język polski)
ggplot(math_trend, aes(x = as.integer(rok), y = as.numeric(wartosc))) +
  geom_line() +
  geom_point() +
  labs(title = "Trend wyników z matematyki w czasie", x = "Rok", y = "Średni wynik (%)")

#  Analiza dysproporcji płci ----------------------------------------------
library(tidyr)

gender_diff <- data_by_gender %>%
  group_by(rok, przedmiot, plec) %>%
  summarise(wynik = mean(as.numeric(wartosc), na.rm = TRUE)) %>%
  pivot_wider(names_from = plec, values_from = wynik) %>%
  mutate(roznica = kobiety - mężczyźni)

gender_diff <- na.omit(gender_diff)

gender_diff_avg <- gender_diff %>%
  group_by(przedmiot) %>%
  summarise(srednia_roznica = mean(roznica))

ggplot(gender_diff_avg, aes(x = reorder(przedmiot, srednia_roznica), y = srednia_roznica, fill = srednia_roznica > 0)) +
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




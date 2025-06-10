# Instalacja wymaganych pakietów -----------------------------------------------------------------------
if (!("readr" %in% rownames(installed.packages()))) {
  install.packages("readr", dependencies = TRUE, character.only = TRUE)
}
if (!("ggplot2" %in% rownames(installed.packages()))) {
  install.packages("ggplot2", dependencies = TRUE, character.only = TRUE)
}
if (!("dplyr" %in% rownames(installed.packages()))) {
  install.packages("dplyr", dependencies = TRUE, character.only = TRUE)
}
if (!("tidyr" %in% rownames(installed.packages()))) {
  install.packages("tidyr", dependencies = TRUE, character.only = TRUE)
}
if (!("plotly" %in% rownames(installed.packages()))) {
  install.packages("plotly", dependencies = TRUE, character.only = TRUE)
}
if (!("gganimate" %in% rownames(installed.packages()))) {
  install.packages("gganimate", dependencies = TRUE, character.only = TRUE)
}

# Import bibliotek -----------------------------------------------------------------------
if (!("readr" %in% search())) {
  library("readr", character.only = TRUE)
}
if (!("ggplot2" %in% search())) {
  library("ggplot2", character.only = TRUE)
}
if (!("dplyr" %in% search())) {
  library("dplyr", character.only = TRUE)
}
if (!("tidyr" %in% search())) {
  library("tidyr", character.only = TRUE)
}
if (!("plotly" %in% search())) {
  library("plotly", character.only = TRUE)
}
if (!("gganimate" %in% search())) {
  library("gganimate", character.only = TRUE)
}

# Ustawienia ścieżki do folderu z danymi -----------------------------------------------------------------------
sciezka_folderu <- "C:/Users/pavli/OneDrive/Робочий стіл/Analiza-wynikow-srednich-egzaminow-maturalnych"

# Wczytanie danych -----------------------------------------------------------------------
srednie_wyniki_egzaminu_maturalnego <- read_csv2(
  file = file.path(sciezka_folderu, "srednie_wyniki_egzaminu_maturalnego_2.csv"),
  locale = locale(encoding = "UTF-8")
)

# Czyszczenie danych -----------------------------------------------------------------------
data_cleaned <- srednie_wyniki_egzaminu_maturalnego %>%
  select(-nazwa_zmiennej, -kraj, -typ_informacji_z_jednostka_miary) %>%
  filter(!(flaga %in% c("(.)", "(–)"))) %>%
  select(-flaga)
# Czyszczenie danych bez dplyr
#data_cleaned <- srednie_wyniki_egzaminu_maturalnego
#data_cleaned <- data_cleaned[, !(names(data_cleaned) %in% c("nazwa_zmiennej", "kraj", "typ_informacji_z_jednostka_miary"))]
#data_cleaned <- data_cleaned[!(data_cleaned$flaga %in% c("(.)", "(–)")), ]
#data_cleaned <- data_cleaned[, -c("flaga")]

# Zapis oczyszczonych danych do pliku -----------------------------------------------------------------------
write_csv2(
  data_cleaned,
  file.path(sciezka_folderu, "srednie_wyniki_cleaned.csv")
)

# Podział danych na ogółem i wg płci -----------------------------------------------------------------------
data_general   <- data_cleaned %>% filter(plec == "ogółem") %>% select(-plec)
data_by_gender <- data_cleaned %>% filter(plec != "ogółem")

# Agregacja średnich wyników dla wszystkich przedmiotów i lat -----------------------------------------------------------------------
data_general_avg <- data_general %>%
  group_by(rok, przedmiot) %>%
  summarise(wartosc = mean(as.numeric(wartosc), na.rm = TRUE), .groups = "drop")
# Agregacja bez użycia dplyr
#data_general$wartosc <- as.numeric(data_general$wartosc) # переконайтесь, що wartosc числова
#data_general_avg <- aggregate(
#  wartosc ~ rok + przedmiot,
#  data = data_general,
#  FUN = function(x) mean(x, na.rm = TRUE)
#)

# Utworzenie własnego wektora kolorów (24 różne kolory) -----------------------------------------------------------------------
my_colors <- c(
  "#e41a1c", "#377eb8", "#4daf4a", "#984ea3", "#ff7f00", "#ffff33", "#a65628", "#f781bf", "#999999",
  "#66c2a5", "#fc8d62", "#8da0cb", "#e78ac3", "#a6d854", "#ffd92f", "#e5c494", "#b3b3b3",
  "#1b9e77", "#d95f02", "#7570b3", "#e7298a", "#66a61e", "#e6ab02", "#a6761d"
)

# Animowany wykres średnich wyników z przedmiotów -----------------------------------------------------------------------
plot_anim <- ggplot(data_general_avg, aes(x = przedmiot, y = wartosc, fill = przedmiot)) +
  geom_col() +
  labs(title = "Średnie wyniki z przedmiotów — {closest_state}", y = "Średni wynik", x = "Przedmiot") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  ylim(0, 100) +
  scale_fill_manual(values = my_colors) + # Używanie własnej palety
  transition_states(rok, transition_length = 2, state_length = 1) +
  enter_fade() + exit_fade()

anim_save(
  filename = file.path(sciezka_folderu, "srednie_wyniki_anim.gif"),
  animation = animate(plot_anim, width = 1000, height = 600, fps = 2, nframes = 10)
)

# Trendy wyników z wszystkich przedmiotów -----------------------------------------------------------------------
plot_trendy <- ggplot(data_general_avg, aes(x = rok, y = wartosc, color = przedmiot, group = przedmiot)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  scale_x_continuous(breaks = unique(data_general_avg$rok)) +
  scale_color_manual(values = my_colors) + # Używanie własnej palety
  labs(
    title = "Trendy wyników z wszystkich przedmiotów w czasie",
    x = "Rok",
    y = "Średni wynik (%)",
    color = "Przedmiot"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave(file.path(sciezka_folderu, "trendy_wszystkie_przedmioty.png"), plot = plot_trendy, width = 12, height = 7, dpi = 300)

# Analiza różnic wyników wg płci -----------------------------------------------------------------------
gender_diff <- data_by_gender %>%
  group_by(rok, przedmiot, plec) %>%
  summarise(wynik = mean(as.numeric(wartosc), na.rm = TRUE)) %>%
  pivot_wider(names_from = plec, values_from = wynik) %>%
  mutate(roznica = kobiety - mężczyźni) %>%
  na.omit()

gender_diff_avg <- gender_diff %>%
  group_by(przedmiot) %>%
  summarise(srednia_roznica = mean(roznica))

plot4 <- ggplot(gender_diff_avg, aes(x = reorder(przedmiot, srednia_roznica), y = srednia_roznica, fill = srednia_roznica > 0)) +
  geom_col() +
  coord_flip() +
  labs(
    title = "Średnia różnica wyników matur (kobiety - mężczyźni)",
    x = "Przedmiot",
    y = "Średnia różnica (%)"
  ) +
  scale_fill_manual(values = c("TRUE" = "#D32F2F", "FALSE" = "#008cff"),
                    name = "Kto wypadł lepiej?", labels = c("Mężczyźni", "Kobiety")) +
  theme_minimal()

ggsave(file.path(sciezka_folderu, "roznica_plec.png"), plot = plot4, width = 10, height = 6, dpi = 300)

# Przygotowanie macierzy korelacji między przedmiotami -----------------------------------------------------------------------
subject_matrix <- data_general %>%
  mutate(wartosc = as.numeric(wartosc)) %>%
  group_by(rok, przedmiot) %>%
  summarise(wartosc = mean(wartosc, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(names_from = przedmiot, values_from = wartosc)

cor_matrix <- cor(subject_matrix[,-1], use = "pairwise.complete.obs")

cor_long <- as.data.frame(as.table(cor_matrix)) %>%
  rename(Przedmiot_1 = Var1, Przedmiot_2 = Var2, Korelacja = Freq)

# Wyszukiwanie 10 najsilniejszych dodatnich i ujemnych korelacji -----------------------------------------------------------------------
cor_long_filtered <- cor_long %>%
  filter(Przedmiot_1 != Przedmiot_2)

# Usuwanie duplikatów par korelacji -----------------------------------------------------------------------
cor_long_filtered_unique <- cor_long_filtered %>%
  mutate(
    Przedmiot_1 = as.character(Przedmiot_1),
    Przedmiot_2 = as.character(Przedmiot_2),
    pair = ifelse(Przedmiot_1 < Przedmiot_2,
                  paste(Przedmiot_1, Przedmiot_2, sep = " — "),
                  paste(Przedmiot_2, Przedmiot_1, sep = " — "))
  ) %>%
  distinct(pair, .keep_all = TRUE)

top10_positive <- cor_long_filtered_unique %>%
  arrange(desc(Korelacja)) %>%
  slice_head(n = 10)

#top10_positive <- cor_long_filtered_unique[order(cor_long_filtered_unique$Korelacja, decreasing = TRUE)[1:10], ]
top10_negative <- cor_long_filtered_unique %>%
  arrange(Korelacja) %>%
  slice_head(n = 10)
#top10_negative <- cor_long_filtered_unique[order(cor_long_filtered_unique$Korelacja)[1:10], ]

print("Top 10 najsilniejszych dodatnich korelacji:")
print(top10_positive)

print("Top 10 najsilniejszych ujemnych korelacji:")
print(top10_negative)

# Heatmapa korelacji między przedmiotami -----------------------------------------------------------------------
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

# Porównanie wyników: WOS vs. Łacina -----------------------------------------------------------------------
r_val <- cor_long_filtered_unique %>%
  filter(Przedmiot_1 == "wiedza o społeczeństwie" & Przedmiot_2 == "język łaciński i kultura antyczna") %>%
  pull(Korelacja) %>%
  round(3)

wos_lacina_long <- data_general %>%
  filter(przedmiot %in% c("wiedza o społeczeństwie", "język łaciński i kultura antyczna")) %>%
  mutate(wartosc = as.numeric(wartosc)) %>%
  group_by(rok, przedmiot) %>%
  summarise(wartosc = mean(wartosc, na.rm = TRUE), .groups = "drop")

plot6 <- ggplot(wos_lacina_long, aes(x = factor(rok), y = wartosc, fill = przedmiot)) +
  geom_bar(stat = "identity", position = "dodge") +
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

# Porównanie wyników: język hiszpański vs. język niemiecki -----------------------------------------------------------------------
r_val_hiszp_niem <- cor_long_filtered_unique %>%
  filter(pair == "język hiszpański — język niemiecki") %>%
  pull(Korelacja) %>%
  round(3)

hiszp_niem_long <- data_general %>%
  filter(przedmiot %in% c("język hiszpański", "język niemiecki")) %>%
  mutate(wartosc = as.numeric(wartosc)) %>%
  group_by(rok, przedmiot) %>%
  summarise(wartosc = mean(wartosc, na.rm = TRUE), .groups = "drop")

plot_hiszp_niem <- ggplot(hiszp_niem_long, aes(x = factor(rok), y = wartosc, fill = przedmiot)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(
    title = paste("Porównanie wyników: język hiszpański vs. język niemiecki\n(Współczynnik korelacji:", r_val_hiszp_niem, ")"),
    x = "Rok",
    y = "Średni wynik (%)",
    fill = "Przedmiot"
  ) +
  scale_fill_manual(values = c(
    "język hiszpański" = "#FFD600",
    "język niemiecki" = "#D32F2F"
  )) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave(file.path(sciezka_folderu, "porownanie_hiszp_niem.png"), plot = plot_hiszp_niem, width = 10, height = 6, dpi = 300)

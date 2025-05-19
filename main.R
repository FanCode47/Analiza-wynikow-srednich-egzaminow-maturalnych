# Importowanie bibliotek --------------------------------------------------
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

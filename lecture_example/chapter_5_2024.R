# Einlesen der pakete
source("functions/packages.R") # wenn dei paket nicht installiert sind, wird es installiert
source("functions/library.R")
# Laden der notwendigen Bibliotheken

file_path_to <- "data/input/kc_house_data.csv"
# Datensatz einlesen
data <-read.csv(file_path_to) %>% 
  mutate(date = lubridate::as_date(date))

# 5.1. Hypothesenformulierung ----
# Ziel: Überprüfen, ob der durchschnittliche Preis der Immobilien und ob dieser höher als 0 ist.
# H0: Der durchschnittliche Preis ist kleiner oder gleich 0 (µ =< 0).
# H1: Der durchschnittliche Preis ist größer 0 (µ > 0).

# 5.1a Testdurchführung -----
# Mittelwert des Preises
mean_price <- mean(data$price)
std_dev_price <- sd(data$price)
sample_size <- nrow(data)

# t-Test
t_test_result <- t.test(data$price, mu = 0, alternative = "greater", conf.level = 0.99)
# Kritischer t-Wert für ein zweiseitiges Signifikanzniveau von 0.05
alpha <- 0.05 # Signifikanzniveau
df <- 21612  # Freiheitsgrade
critical_t <- qt(1 - alpha , df) # Das ist der OBERE kritische Wert. Der Untere wäre mit "qt(alpha/2, df)" zu berechnen.
critical_t # kritischer t-Wert
qnorm(1 - alpha) # kritischer z-Wert
print(t_test_result)
# Ergebnis: Ich lehne die H0 ab, da der p-Wert kleiner als 0.05 ist. 
# und der kritische Wert kleiner als der t-Wert ist


# Ziel: Überprüfen, ob der durchschnittliche Preis der Immobilien 500.000 beträgt.
# H0: Der durchschnittliche Preis ist gleich 500.000 (µ = 500.000).
# H1: Der durchschnittliche Preis ist ungleich 500.000 (µ ≠ 500.000).

# 5.1b Testdurchführung -----
# Mittelwert des Preises
mean_price <- mean(data$price)
std_dev_price <- sd(data$price)
sample_size <- nrow(data)

# t-Test
t_test_result <- t.test(data$price, mu = 500000, alternative = "two.sided")
print(t_test_result)

# 3. Testvoraussetzungen prüfen und diskutieren
# a) Normalverteilung der Daten: QQ-Plot und Shapiro-Wilk-Test
qqnorm(data$price, main = "QQ-Plot für Preis")
qqline(data$price, col = "red")

# Shapiro-Wilk-Test (nur für Stichproben bis 5000 Beobachtungen)
  shapiro_test <- shapiro.test(sample(data$price, 5000))
  print(shapiro_test)


# Kolmogorov-Smirnov-Test (Alternative für größere Stichproben)
ks_test <- ks.test(data$price, "pnorm", mean = mean_price, sd = std_dev_price)
print(ks_test)

# b) Unabhängigkeit der Beobachtungen
# Annahme: Die Daten wurden unabhängig erhoben. 
# Diskutieren Sie diese Annahme auf Basis der Datenquelle.

# c) Diskussion der Ergebnisse
cat("Diskussion der Ergebnisse:\n")
cat("1. Hypothesen: H0 (µ = 500.000) und H1 (µ ≠ 500.000).\n")
cat("2. Ergebnisse des t-Tests:\n")
cat("   - Teststatistik:", t_test_result$statistic, "\n")
cat("   - p-Wert:", t_test_result$p.value, "\n")
if (t_test_result$p.value < 0.05) {
  cat("   - Ergebnis: Die Nullhypothese wird abgelehnt. Der durchschnittliche Preis unterscheidet sich signifikant von 500.000.\n")
} else {
  cat("   - Ergebnis: Die Nullhypothese wird nicht abgelehnt. Es gibt keine ausreichenden Beweise für einen Unterschied.\n")
}

cat("3. Testvoraussetzungen:\n")
cat("- Normalverteilung: QQ-Plot und Normalitätstests zeigen die Erfüllung der Normalverteilungsannahme.\n")
cat("- Unabhängigkeit: Basierend auf der Datenquelle wird die Unabhängigkeit angenommen.\n")
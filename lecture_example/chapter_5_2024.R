# Kapitel 5: Statistische Tests am King-County-Datensatz
# Präsenzveranstaltung 2024, überarbeitet am 21.11.2026
# Arbeitsverzeichnis: Projektordner (RStudio-Projekt Fort.-Methoden-der-Statistik.Rproj öffnen)

# Pakete einlesen
source("functions/packages.R") # installiert nur Pakete, die noch fehlen
source("functions/library.R")  # lädt die notwendigen Pakete

file_path_to <- "data/input/kc_house_data.csv"
# Datensatz einlesen
data <- read.csv(file_path_to) %>%
  mutate(date = lubridate::as_date(date))

# 5.1 Hypothesenformulierung ----
# Ziel: Überprüfen, ob der durchschnittliche Preis der Immobilien größer als 0 ist.
# (Ein bewusst einfaches Beispiel: Die Antwort ist klar, der Ablauf steht im Vordergrund.)
# H0: Der durchschnittliche Preis ist kleiner oder gleich 0 (mu <= 0).
# H1: Der durchschnittliche Preis ist größer als 0 (mu > 0).

# 5.1a Testdurchführung -----
alpha <- 0.05 # Signifikanzniveau
mean_price <- mean(data$price)
std_dev_price <- sd(data$price)
sample_size <- nrow(data)

# Einseitiger t-Test; das Konfidenzniveau passt zum Signifikanzniveau: 1 - alpha
t_test_result <- t.test(data$price, mu = 0, alternative = "greater", conf.level = 1 - alpha)
print(t_test_result)

# Kritischer Wert für den einseitigen Test (rechtsseitig) zum Niveau alpha = 0,05
df <- sample_size - 1   # Freiheitsgrade
critical_t <- qt(1 - alpha, df)
critical_t # kritischer t-Wert
qnorm(1 - alpha) # kritischer z-Wert: bei so vielen Freiheitsgraden praktisch gleich
# Bei einem zweiseitigen Test lägen die kritischen Werte bei qt(alpha/2, df) und qt(1 - alpha/2, df).

# Ergebnis: H0 wird abgelehnt, weil der p-Wert kleiner als 0,05 ist
# und die Teststatistik größer als der kritische Wert.


# Ziel: Überprüfen, ob der durchschnittliche Preis der Immobilien 500.000 beträgt.
# H0: Der durchschnittliche Preis ist gleich 500.000 (mu = 500.000).
# H1: Der durchschnittliche Preis ist ungleich 500.000 (mu != 500.000).

# 5.1b Testdurchführung -----
t_test_result <- t.test(data$price, mu = 500000, alternative = "two.sided")
print(t_test_result)

# Testvoraussetzungen prüfen und diskutieren ----
# a) Normalverteilung der Daten: QQ-Plot und Normalitätstests
qqnorm(data$price, main = "QQ-Plot für Preis")
qqline(data$price, col = "red")

# Shapiro-Wilk-Test (in R nur für Stichproben bis 5.000 Beobachtungen),
# deshalb auf einer Zufallsauswahl von 5.000 Preisen
set.seed(123)
shapiro_test <- shapiro.test(sample(data$price, 5000))
print(shapiro_test)

# Lilliefors-Test: Kolmogorov-Smirnov-Test mit geschätztem Mittelwert und geschätzter
# Standardabweichung (der klassische ks.test() liefert dann zu große p-Werte)
lillie_test <- lillie.test(data$price)
print(lillie_test)

# b) Unabhängigkeit der Beobachtungen
# Annahme: Die Daten wurden unabhängig erhoben.
# Diskutiert diese Annahme auf Basis der Datenquelle.

# c) Diskussion der Ergebnisse
cat("Diskussion der Ergebnisse:\n")
cat("1. Hypothesen: H0 (mu = 500.000) und H1 (mu != 500.000).\n")
cat("2. Ergebnisse des t-Tests:\n")
cat("   - Teststatistik:", t_test_result$statistic, "\n")
cat("   - p-Wert:", t_test_result$p.value, "\n")
if (t_test_result$p.value < alpha) {
  cat("   - Ergebnis: Die Nullhypothese wird abgelehnt. Der durchschnittliche Preis unterscheidet sich signifikant von 500.000.\n")
} else {
  cat("   - Ergebnis: Die Nullhypothese wird nicht abgelehnt. Die Daten sprechen nicht gegen einen mittleren Preis von 500.000.\n")
}

cat("3. Testvoraussetzungen:\n")
cat("- Normalverteilung: QQ-Plot, Shapiro-Wilk- und Lilliefors-Test zeigen, dass die Preise NICHT normalverteilt sind (deutlich rechtsschief).\n")
cat("- Der t-Test bleibt trotzdem anwendbar: Bei n = 21.613 ist der Mittelwert nach dem zentralen Grenzwertsatz näherungsweise normalverteilt.\n")
cat("- Unabhängigkeit: Basierend auf der Datenquelle wird die Unabhängigkeit angenommen.\n")

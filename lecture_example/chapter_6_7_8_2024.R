# Kapitel 6 bis 8: Zweistichprobentests, Tests für Zusammenhangsmaße und lineare Regression
# Präsenzveranstaltung 2024, überarbeitet am 21.11.2026
# Arbeitsverzeichnis: Projektordner (RStudio-Projekt Fort.-Methoden-der-Statistik.Rproj öffnen)

# Pakete einlesen
source("functions/packages.R") # installiert nur Pakete, die noch fehlen
source("functions/library.R")  # lädt die notwendigen Pakete

# ---- 6. Zweistichprobentests ------------------------------------
# Ziel: Vergleich von Häusern mit und ohne Wasserblick hinsichtlich des Preises

file_path_to <- "data/input/kc_house_data.csv"
# Datensatz einlesen
data <- read.csv(file_path_to) %>%
  mutate(date = lubridate::as_date(date))

# a) Gruppenvergleich: Preis für waterfront (0 = ohne, 1 = mit Wasserblick)
group_0 <- data$price[data$waterfront == 0] # Gruppe ohne Wasserblick
group_1 <- data$price[data$waterfront == 1] # Gruppe mit Wasserblick

# Testwahl: Überprüfung der Normalverteilung in beiden Gruppen
ad_0 <- ad.test(group_0)
ad_1 <- ad.test(group_1)
cat("Anderson-Darling-Test für Normalverteilung (Gruppe 0): p-Wert =", ad_0$p.value, "\n")
cat("Anderson-Darling-Test für Normalverteilung (Gruppe 1): p-Wert =", ad_1$p.value, "\n")

# Variabilität der Gruppen überprüfen
var_test <- var.test(group_0, group_1)
cat("Varianzhomogenitätstest (F-Test): p-Wert =", var_test$p.value, "\n")

# Testwahl basierend auf Ergebnissen
# Mann-Whitney-U-Test, da Normalverteilung nicht gegeben ist (p-Wert < 0,05)
wilcox_test <- wilcox.test(group_0, group_1, paired = FALSE, alternative = "two.sided")
cat("Mann-Whitney-U-Test: p-Wert =", wilcox_test$p.value, "\n")
wilcox_test
# Der Mann-Whitney-U-Test vergleicht Lage und Verteilung, nicht Mittelwerte.
# Zur Einordnung deshalb Median und Mittelwert je Gruppe:
c(Median_ohne = median(group_0), Median_mit = median(group_1))
c(Mittelwert_ohne = mean(group_0), Mittelwert_mit = mean(group_1))

# Interpretation
cat("Interpretation:\n")
cat("Der Mann-Whitney-U-Test zeigt, ob die Preisverteilung für Häuser mit und ohne Wasserblick unterschiedlich ist.
     Ein p-Wert < 0,05 deutet darauf hin, dass die beiden Gruppen signifikant unterschiedlich sind.\n")


# Bei ungleichen Varianzen ist der Welch-Test die passende t-Test-Variante.
# Normalverteilung ist hier zwar nicht gegeben; bei großen Gruppen trägt aber
# der zentrale Grenzwertsatz (die Gruppe mit Wasserblick hat 163 Häuser).
welch_test <- t.test(group_0, group_1, var.equal = FALSE)
welch_test


# Visualisierung
ggplot(data, aes(x = factor(waterfront), y = price)) +
  geom_boxplot(fill = c("blue", "green")) +
  ggtitle("Preisvergleich: Häuser mit vs. ohne Wasserblick") +
  xlab("Wasserblick (0 = ohne, 1 = mit)") +
  ylab("Preis")

# ---- 7. Tests für Zusammenhangsmaße -----------------------------
# 7.1a Beispiel 1: Korrelation zwischen Wohnfläche und Preis -------------------
# Berechnung der Pearson- und Spearman-Korrelation
cor_pearson_1 <- cor(data$sqft_living, data$price, method = "pearson") # zwei metrische Merkmale, linearer Zusammenhang
cor_spearman_1 <- cor(data$sqft_living, data$price, method = "spearman") # monotoner Zusammenhang; auch bei ordinalen Merkmalen

plot(data$sqft_living, data$price)

# Signifikanztests
# H0: Die Korrelation ist 0
cor_test_pearson_1 <- cor.test(data$sqft_living, data$price, method = "pearson")
cor_test_spearman_1 <- cor.test(data$sqft_living, data$price, method = "spearman", exact = FALSE)

# Ergebnisse ausgeben
cat("Beispiel 1: Korrelation zwischen Wohnfläche und Preis\n")
cat("Pearson-Korrelation:", cor_pearson_1, "\n")
cat("  Signifikanz p-Wert:", cor_test_pearson_1$p.value, "\n")
cat("Spearman-Korrelation:", cor_spearman_1, "\n")
cat("  Signifikanz p-Wert:", cor_test_spearman_1$p.value, "\n\n")

# Visualisierung: Streudiagramm für Beispiel 1
ggplot(data, aes(x = sqft_living, y = price)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", formula = y ~ x, se = FALSE, color = "red") +
  ggtitle("Zusammenhang zwischen Wohnfläche und Preis") +
  xlab("Wohnfläche (sqft)") +
  ylab("Preis")

# Interpretation
cat("Interpretation für Beispiel 1:\n")
cat("Die Pearson- und Spearman-Korrelationen zeigen einen starken positiven Zusammenhang zwischen Wohnfläche und Preis.
     Die niedrigen p-Werte (< 0,05) weisen auf eine signifikante Beziehung hin.\n\n")

# 7.1b Beispiel 2: Korrelation zwischen Anzahl der Schlafzimmer und Wohnfläche ----
# Berechnung der Pearson- und Spearman-Korrelation
cor_pearson_2 <- cor(data$bedrooms, data$sqft_living, method = "pearson")
cor_spearman_2 <- cor(data$bedrooms, data$sqft_living, method = "spearman")

# Signifikanztests (exact = FALSE, weil viele gleiche Werte vorkommen)
cor_test_pearson_2 <- cor.test(data$bedrooms, data$sqft_living, method = "pearson")
cor_test_spearman_2 <- cor.test(data$bedrooms, data$sqft_living, method = "spearman", exact = FALSE)

# Ergebnisse ausgeben
cat("Beispiel 2: Korrelation zwischen Anzahl der Schlafzimmer und Wohnfläche\n")
cat("Pearson-Korrelation:", cor_pearson_2, "\n")
cat("  Signifikanz p-Wert:", cor_test_pearson_2$p.value, "\n")
cat("Spearman-Korrelation:", cor_spearman_2, "\n")
cat("  Signifikanz p-Wert:", cor_test_spearman_2$p.value, "\n\n")

# Visualisierung: Streudiagramm für Beispiel 2
ggplot(data, aes(x = bedrooms, y = sqft_living)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", formula = y ~ x, se = FALSE, color = "blue") +
  ggtitle("Zusammenhang zwischen Anzahl der Schlafzimmer und Wohnfläche") +
  xlab("Anzahl der Schlafzimmer") +
  ylab("Wohnfläche (sqft)")

# Interpretation
cat("Interpretation für Beispiel 2:\n")
cat("Die Pearson- und Spearman-Korrelationen zeigen einen moderaten positiven Zusammenhang zwischen der Anzahl der Schlafzimmer und der Wohnfläche.
     Die niedrigen p-Werte (< 0,05) weisen auf eine signifikante Beziehung hin.\n")


# 7.2 Chi-Quadrat-Test: Zusammenhang zwischen Wasserblick und Zustand (condition) -----
# Kontingenztabelle erstellen
# H0: Die beiden Variablen sind unabhängig
table_waterfront_condition <- table(data$waterfront, data$condition)
table_waterfront_condition
chi_square_test <- chisq.test(table_waterfront_condition, simulate.p.value = TRUE, B = 10000)
chi_square_test # p-Wert per Simulation, weil einige erwartete Häufigkeiten unter 5 liegen

# Der Chi-Quadrat-Test braucht kategoriale Merkmale. Der Preis ist stetig (rund 4.000
# verschiedene Werte); deshalb wird er zuerst in vier Preisklassen (Quartile) eingeteilt.
data$preisklasse <- cut(data$price, breaks = quantile(data$price, probs = seq(0, 1, 0.25)),
                        include.lowest = TRUE, labels = c("Q1", "Q2", "Q3", "Q4"))
table_waterfront_price <- table(data$waterfront, data$preisklasse)
table_waterfront_price
chisq.test(table_waterfront_price)

# 7.3 Visualisierung: Mosaikplot -----
mosaicplot(table_waterfront_condition, main = "Zusammenhang zwischen Wasserblick und Zustand",
           color = TRUE, xlab = "Wasserblick", ylab = "Zustand")



# ----8: Lineare Regression --------------------------------------
# a) Einfache lineare Regression: Preis ~ Wohnfläche
model_simple <- lm(price ~ sqft_living, data = data)
summary(model_simple)
# Coefficients:
#               Estimate Std. Error t value Pr(>|t|)
# (Intercept) -43580.743   4402.690  -9.899   <2e-16 ***
# sqft_living    280.624      1.936 144.920   <2e-16 ***
# Ausgegeben werden Koeffizient, Standardfehler, t-Wert (t-Test auf beta = 0) und p-Wert.
# Drei Sterne bedeuten einen p-Wert kleiner als 0,001: H0 (beta = 0) wird abgelehnt.

# Wie errechnet sich der t-Wert und der p-Wert?
t_wert <- -43580.743 / 4402.690   # Schätzwert geteilt durch Standardfehler
t_wert
2 * pt(-abs(t_wert), df = model_simple$df.residual) # zweiseitiger p-Wert

# Interpretation:
# Die Wohnfläche hat einen signifikanten Einfluss auf den Preis.
# Der Koeffizient von sqft_living ist 280,62: Steigt die Wohnfläche um 1 Quadratfuß,
# steigt der erwartete Preis um rund 281 USD.
# Der Achsenabschnitt (-43.581) wäre der Preis eines Hauses mit 0 Quadratfuß Wohnfläche.
# Solche Häuser gibt es nicht; der Achsenabschnitt ist hier eine Rechengröße ohne
# sinnvolle Interpretation (Extrapolation außerhalb des Datenbereichs).

# b) Multiple Regression: Preis ~ Wohnfläche + Anzahl der Schlafzimmer
model_multiple_1 <- lm(price ~ sqft_living + bedrooms, data = data)
summary(model_multiple_1)

# c) Multiple Regression: Preis ~ Wohnfläche + Anzahl der Schlafzimmer + Wasserblick
model_multiple_2 <- lm(price ~ sqft_living + bedrooms + waterfront, data = data)
summary(model_multiple_2)

# d) Modellvergleich: Einfache vs. multiple Regression
# Modellbewertung: Vergleich von R² und F-Test
cat("Modellbewertung:\n")
cat("Der R²-Wert gibt den Anteil der Varianz an, der durch das Modell erklärt wird.
     Höhere Werte deuten auf ein besseres Modell hin.\n")
# Je mehr Variablen im Modell sind, desto höher wird das R².
# Deshalb ist das angepasste R² (Adjusted R²) für den Vergleich wichtig.
c(einfach = summary(model_simple)$adj.r.squared,
  multipel_1 = summary(model_multiple_1)$adj.r.squared,
  multipel_2 = summary(model_multiple_2)$adj.r.squared)

# Der F-Test prüft, ob mindestens ein Regressor einen signifikanten Einfluss hat;
# hier ist er in allen Modellen signifikant.

# Residuenanalyse
hist(residuals(model_multiple_2), breaks = 50, main = "Residuen Modell c)", xlab = "Residuen")

par(mfrow = c(2, 2))  # Mehrere Plots auf einer Seite
plot(model_multiple_2)  # Residuen, QQ-Plot, Scale-Location, Hebelwerte
par(mfrow = c(1, 1))

# Zusatz, nicht in der Vorlesung behandelt: Homoskedastizität prüfen
# bptest_result <- bptest(model_multiple_2)
# cat("Breusch-Pagan-Test für Homoskedastizität: p-Wert =", bptest_result$p.value, "\n")

# Normalverteilung der Residuen prüfen
# shapiro.test() akzeptiert höchstens 5.000 Werte; bei 21.613 Residuen deshalb der
# Anderson-Darling-Test (nortest::ad.test) und der QQ-Plot oben
ad_residuals <- ad.test(residuals(model_multiple_2))
cat("Anderson-Darling-Test für Normalverteilung der Residuen: p-Wert =", ad_residuals$p.value, "\n")

# Linearität prüfen (Paket car)
car::residualPlots(model_multiple_2)

# Interpretation der Residuenanalyse
cat("Interpretation der Residuenanalyse:\n")
cat("1. Der QQ-Plot und der Anderson-Darling-Test prüfen die Normalverteilung der Residuen.\n")
cat("2. Residuenplots zeigen mögliche Abweichungen von der Linearitätsannahme.\n")

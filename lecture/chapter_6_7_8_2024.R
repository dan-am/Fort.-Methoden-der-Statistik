# ---- 6. Zweistichprobentests ------------------------------------
# Ziel: Vergleich von Häusern mit und ohne Wasserblick hinsichtlich des Preises

file_path_to <- "data/input/kc_house_data.csv"
# Datensatz einlesen
data <-read.csv(file_path_to) %>% 
  mutate(date = lubridate::as_date(date))

# a) Gruppenvergleich: Preis für waterfront (0 = ohne, 1 = mit Wasserblick)
group_0 <- data$price[data$waterfront == 0] # Gruppe ohne Wasserblick
group_1 <- data$price[data$waterfront == 1] # Gruppe mit Wasserblick

# Testwahl: Überprüfung der Normalverteilung in beiden Gruppen
ad_0 <- ad.test(group_0)
ad_1 <- ad.test(group_1)
cat("Anderson-Darling-Test für Normalverteilung (Gruppe 0): p-Wert =", ad_0$p.value, "\n")
cat("Anderson-Darling-Test für Normalverteilung (Gruppe 1): p-Wert =", ad_1$p.value, "\n")

# Variablität der Gruppen überprüfen
var_test <- var.test(group_0, group_1)
cat("Varianzhomogenitätstest (F-Test): p-Wert =", var_test$p.value, "\n")

# Testwahl basierend auf Ergebnissen
# Mann-Whitney-U-Test, da Normalverteilung nicht gegeben ist (p-Wert < 0.05)
wilcox_test <- wilcox.test(group_0, group_1, paired = FALSE, alternative = "two.sided") # 
cat("Mann-Whitney-U-Test: p-Wert =", wilcox_test$p.value, "\n")
wilcox_test
# wir schauen uns die Mittelwerte an 
mean(group_0) 
mean(group_1)
# -> in der Theorie nicht behandelt hier erfolgt Nachreichung

# Interpretation
cat("Interpretation:\n")
cat("Der Mann-Whitney-U-Test zeigt, ob die Preisverteilung für Häuser mit und ohne Wasserblick unterschiedlich ist. 
     Ein p-Wert < 0.05 deutet darauf hin, dass die beiden Gruppen signifikant unterschiedlich sind.\n")


# wenn die Daten normalverteilt sind und die Varianzen ungleich sind, 
# dann können wir den t-Test durchführen
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
cor_pearson_1 <- cor(data$sqft_living, data$price, method = "pearson") # immer wenn ich 2 metrische Merkmale habe
cor_spearman_1 <- cor(data$sqft_living, data$price, method = "spearman") # immer wenn min. 1 merkmal ordinal ist

plot(data$sqft_living, data$price)

# Signifikanztests
# hier nutze ich die Hypothese das die Korrelation 0 ist
cor_test_pearson_1 <- cor.test(data$sqft_living, data$price, method = "pearson")
cor_test_spearman_1 <- cor.test(data$sqft_living, data$price, method = "spearman" , exact  = FALSE)

# Ergebnisse ausgeben
cat("Beispiel 1: Korrelation zwischen Wohnfläche und Preis\n")
cat("Pearson-Korrelation:", cor_pearson_1, "\n")
cat("  Signifikanz p-Wert:", cor_test_pearson_1$p.value, "\n")
cat("Spearman-Korrelation:", cor_spearman_1, "\n")
cat("  Signifikanz p-Wert:", cor_test_spearman_1$p.value, "\n\n")

# Visualisierung: Streudiagramm für Beispiel 1
ggplot(data, aes(x = sqft_living, y = price)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  ggtitle("Zusammenhang zwischen Wohnfläche und Preis") +
  xlab("Wohnfläche (sqft)") +
  ylab("Preis")

# Interpretation
cat("Interpretation für Beispiel 1:\n")
cat("Die Pearson- und Spearman-Korrelationen zeigen einen starken positiven Zusammenhang zwischen Wohnfläche und Preis.
     Die niedrigen p-Werte (< 0.05) weisen auf eine signifikante Beziehung hin.\n\n")

# 7.1b Beispiel 2: Korrelation zwischen Anzahl der Schlafzimmer und Wohnfläche ----
# Berechnung der Pearson- und Spearman-Korrelation
cor_pearson_2 <- cor(data$bedrooms, data$sqft_living, method = "pearson")
cor_spearman_2 <- cor(data$bedrooms, data$sqft_living, method = "spearman")

# Signifikanztests
cor_test_pearson_2 <- cor.test(data$bedrooms, data$sqft_living, method = "pearson")
cor_test_spearman_2 <- cor.test(data$bedrooms, data$sqft_living, method = "spearman")

# Ergebnisse ausgeben
cat("Beispiel 2: Korrelation zwischen Anzahl der Schlafzimmer und Wohnfläche\n")
cat("Pearson-Korrelation:", cor_pearson_2, "\n")
cat("  Signifikanz p-Wert:", cor_test_pearson_2$p.value, "\n")
cat("Spearman-Korrelation:", cor_spearman_2, "\n")
cat("  Signifikanz p-Wert:", cor_test_spearman_2$p.value, "\n\n")

# Visualisierung: Streudiagramm für Beispiel 2
ggplot(data, aes(x = bedrooms, y = sqft_living)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  ggtitle("Zusammenhang zwischen Anzahl der Schlafzimmer und Wohnfläche") +
  xlab("Anzahl der Schlafzimmer") +
  ylab("Wohnfläche (sqft)")

# Interpretation
cat("Interpretation für Beispiel 2:\n")
cat("Die Pearson- und Spearman-Korrelationen zeigen einen moderaten positiven Zusammenhang zwischen der Anzahl der Schlafzimmer und der Wohnfläche.
     Die niedrigen p-Werte (< 0.05) weisen auf eine signifikante Beziehung hin.\n")


# 7.2 Chi-Quadrat-Test: Zusammenhang zwischen Wasserblick und Zustand (condition) -----
# Kontingenztabelle erstellen
# die Nullhypothese ist das die beiden Variablen unabhängig sind
table_waterfront_condition <- table(data$waterfront, data$condition) 
chi_square_test <- chisq.test(table_waterfront_condition)
cat("Chi-Quadrat-Test: p-Wert =", chi_square_test$p.value, "\n") # p-Wert ist relativ klein 
# aber nicht zu klein, das Resultat darf deshalb kritisch geprüft werden

table_waterfront_price <- table(data$waterfront, data$price) 
chi_square_test2 <- chisq.test(table_waterfront_price, simulate.p.value = TRUE)
chi_square_test2

t(table(data$waterfront, data$price))  %>% View()

# 7.3 Visualisierung: Mosaikplot -----
mosaicplot(table_waterfront_condition, main = "Zusammenhang zwischen Wasserblick und Zustand", 
           color = TRUE, xlab = "Wasserblick", ylab = "Zustand")



# ----8: Lineare Regression --------------------------------------
# a) Einfache lineare Regression: Preis ~ Wohnfläche
model_simple <- lm(price ~ sqft_living, data = data)
summary(model_simple)
# ---
# Coefficients:
# ---
# Estimate Std. Error t value Pr(>|t|)    
# es werden die Koeffizienten, die Standard Fehler, der T-Wert (t-test) und der dazu gehörige p-Wert ausgegeben
# (Intercept) -43580.743      4402.690  -9.899   <2e-16 *** alle 3 Sterne bedeuten das der p-Wert kleiner als 0.001 ist
# sqft_living    280.624      1.936 144.920   <2e-16 *** das heißt das die Nullhypothese abgelehnt wird
# ---
# wie errechne ich den t-Wert und den p-Wert
-43580.743/4402.690 # t-Wert
dnorm(-9.898663) # p-Wert
# das Ergebnis zeigt, dass die Wohnfläche einen signifikanten Einfluss auf den Preis hat
# der Achsenabschnitt ist von 0 verschieden, 
# das bedeutet das es auch Häuser die 0 sqft haben einen Preis von -43580.743 haben
# der Koeffizient von sqft_living ist 280.624, das bedeutet das der Preis um 280.624 steigt wenn die Wohnfläche um 1 steigt
# der p-Wert ist sehr klein, das bedeutet das die Nullhypothese abgelehnt wird

# b) Multiple Regression: Preis ~ Wohnfläche + Anzahl der Schlafzimmer
model_multiple_1 <- lm(price ~ sqft_living + bedrooms, data = data)
summary(model_multiple_1)

# c) Multiple Regression: Preis ~ Wohnfläche + Anzahl der Schlafzimmer + Wasserblick
model_multiple_2 <- lm(price ~ sqft_living + bedrooms + waterfront, data = data)
summary(model_multiple_2)

# d) Modellvergleich: Einfache vs. multiple Regression 
# Modellbewertung: Vergleich von R² und F-Test
cat("Modellbewertung:\n")
cat("Das R²-Wert gibt den Anteil der Varianz an, der durch das Modell erklärt wird. 
     Höhere Werte deuten auf ein besseres Modell hin.\n")
# je mehr Variablen ich in das Modell nehme, desto höher wird das R²
# aus diesem Grund ist es wichtig das R² mit dem Adjusted R² zu vergleichen

# der F-test ist ein indikator ob mindestens eine geschätzte Variable signifikant ist
# hier in allen Beispielen ist eine Signifikanz festzustellen

# Residuenanalyse
hist(residuals(model_multiple_2), breaks = 50)  # Histogramm der Residuen

par(mfrow = c(2, 2))  # Mehrere Plots auf einer Seite
plot(model_multiple_2)  # Plots: Residuen, QQ-Plot, Skalierung

# # haben wir nicht in der VL gemacht ist also Zusatz
# # Homoskedastizität prüfen
# bptest_result <- bptest(model_multiple_2)
# cat("Breusch-Pagan-Test für Homoskedastizität: p-Wert =", bptest_result$p.value, "\n")

# Normalverteilung der Residuen prüfen
shapiro_residuals <- shapiro.test(residuals(model_multiple_2))
cat("Shapiro-Wilk-Test für Normalverteilung der Residuen: p-Wert =", shapiro_residuals$p.value, "\n")

# Linearity prüfen
residualPlots(model_multiple_2)

# Interpretation der Residuenanalyse
cat("Interpretation der Residuenanalyse:\n")
cat("1. Der QQ-Plot und der Shapiro-Wilk-Test prüfen die Normalverteilung der Residuen.\n")
# cat("2. Der Breusch-Pagan-Test prüft die Homoskedastizität.\n")
cat("3. Residualplots zeigen mögliche Abweichungen von der Linearitätsannahme.\n")
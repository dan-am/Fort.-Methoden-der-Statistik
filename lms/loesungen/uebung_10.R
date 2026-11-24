# =============================================================================
# Lösung zu Übung 10 (Kapitel 8: Lineare Regression)
# Quelle: Aufgabe „Übung 10: Lineare Regression“ und Seite „Lösung Übung 10“ im LMS,
# neu seit 22.11.2026
# Daten: kc_house_data.csv im Arbeitsverzeichnis oder in data/input/
# Benötigt nur Base R.
# =============================================================================

datei <- if (file.exists("kc_house_data.csv")) "kc_house_data.csv" else "data/input/kc_house_data.csv"
haeuser <- read.csv(datei)
dim(haeuser)    # 21.613 Hausverkäufe, 21 Variablen


# -----------------------------------------------------------------------------
# Aufgabe 1 (8.1): Einfache lineare Regression
# -----------------------------------------------------------------------------
m1 <- lm(price ~ sqft_living, data = haeuser)
summary(m1)
# Steigung 280,6: Je zusätzlichem Quadratfuß Wohnfläche steigt der erwartete Preis
# um rund 281 Dollar. Der Achsenabschnitt (-43.581 Dollar) hat keine sinnvolle
# Deutung, ein Haus ohne Wohnfläche gibt es nicht.
# R^2 = 0,49: Die Wohnfläche erklärt knapp die Hälfte der Streuung der Preise.

plot(haeuser$sqft_living, haeuser$price, pch = 16, cex = 0.3, col = rgb(0, 0, 0, 0.25),
     xlab = "Wohnfläche (Quadratfuß)", ylab = "Preis (Dollar)", main = "Preis und Wohnfläche")
abline(m1, col = "red", lwd = 2)


# -----------------------------------------------------------------------------
# Aufgabe 2 (8.2): Prognose
# -----------------------------------------------------------------------------
neu <- data.frame(sqft_living = 2000)
predict(m1, newdata = neu, interval = "confidence", level = 0.95)
predict(m1, newdata = neu, interval = "prediction", level = 0.95)
# Prognose: 517.666 Dollar.
# Konfidenzintervall für den mittleren Preis aller Häuser mit 2.000 Quadratfuß:
#   514.167 bis 521.165 Dollar, sehr schmal wegen der 21.613 Beobachtungen.
# Prognoseintervall für ein einzelnes Haus: 5.188 bis 1.030.145 Dollar.
# Es enthält zusätzlich die Streuung der einzelnen Preise um die Gerade
# (Residuenstandardfehler rund 261.500 Dollar) und ist deshalb viel breiter.
# Dass die Untergrenze fast bei 0 liegt, zeigt schon hier: Ein einziges Merkmal
# beschreibt die Preise nur grob.


# -----------------------------------------------------------------------------
# Aufgabe 3 (8.3): Multiple Regression
# -----------------------------------------------------------------------------
m2 <- lm(price ~ sqft_living + bedrooms + bathrooms + grade + waterfront, data = haeuser)
summary(m2)
# Alle Koeffizienten sind signifikant (partielle t-Tests, p < 0,001); der F-Test
# lehnt ab, dass alle Steigungen 0 sind. Bei sonst gleichen Merkmalen
#   - steigt der Preis je Quadratfuß um 213 Dollar,
#   - steigt er je Stufe der Bauqualität (grade) um rund 96.600 Dollar,
#   - liegt er mit Wasserblick um rund 798.800 Dollar höher.
# bedrooms (-31.960 Dollar) und bathrooms (-25.510 Dollar) sind negativ: Bei gleicher
# Wohnfläche bedeutet ein zusätzliches Schlafzimmer kleinere Räume. Allein betrachtet
# hängen Schlafzimmer und Preis positiv zusammen, weil viele Schlafzimmer meist viel
# Wohnfläche bedeuten:
cor(haeuser$bedrooms, haeuser$sqft_living)    # 0,58


# -----------------------------------------------------------------------------
# Aufgabe 4 (8.4): Modellwahl und Residuen
# -----------------------------------------------------------------------------
c(R2_angepasst_m1 = summary(m1)$adj.r.squared, R2_angepasst_m2 = summary(m2)$adj.r.squared)
AIC(m1, m2)
# Modell 2 ist besser: angepasstes R^2 0,578 statt 0,493, AIC 596.568 statt 600.541.

plot(fitted(m2), resid(m2), pch = 16, cex = 0.3, col = rgb(0, 0, 0, 0.25),
     xlab = "angepasste Werte (Dollar)", ylab = "Residuen (Dollar)",
     main = "Modell 2: Residuen gegen angepasste Werte")
abline(h = 0, col = "red", lwd = 2)
# Die Residuen bilden einen Trichter: Bei teuren Häusern streuen sie viel stärker
# (Heteroskedastie), dazu kommen einzelne sehr große positive Residuen. Die
# Standardfehler und Intervalle des Modells sind dann nur eingeschränkt verlässlich.

# Ausblick: logarithmierter Preis
m3 <- lm(log(price) ~ sqft_living + bedrooms + bathrooms + grade + waterfront, data = haeuser)
summary(m3)$adj.r.squared    # 0,566
# Mit log(price) streuen die Residuen viel gleichmäßiger. Das AIC von Modell 3 lässt
# sich nicht mit dem von Modell 1 und 2 vergleichen, weil die abhängige Variable
# eine andere ist.
plot(fitted(m3), resid(m3), pch = 16, cex = 0.3, col = rgb(0, 0, 0, 0.25),
     xlab = "angepasste Werte (log Dollar)", ylab = "Residuen",
     main = "Modell 3: log(price), Residuen gegen angepasste Werte")
abline(h = 0, col = "red", lwd = 2)

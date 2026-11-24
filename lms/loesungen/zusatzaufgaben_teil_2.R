# =============================================================================
# Lösungen zu „Zusatzaufgaben Teil 2: Regression“
# (Regression, Kapitel 8)
# Quelle: Datei „R-Code Aufgaben Teil 2“ (00 Tag 2.R) im LMS, Präsenztag 2023
# Überarbeitet am 21.11. und 23.11.2026; Änderungen gegenüber dem LMS stehen in lms/AENDERUNGEN.md
# Daten: wine.csv, wine_38.csv
# Benötigt die Pakete readr und corrplot.
# =============================================================================

pakete  <- c("readr", "corrplot")
fehlend <- setdiff(pakete, rownames(installed.packages()))
if (length(fehlend) > 0) install.packages(fehlend, repos = "https://cloud.r-project.org")
library(readr)
library(corrplot)

# Die Datendateien liegen im Arbeitsverzeichnis (Download aus dem LMS)
# oder im Projektordner unter data/input/
pfad <- function(datei) if (file.exists(datei)) datei else file.path("data", "input", datei)


# Hilfsfunktion: Kennzahlen eines Modells für den Vergleich
kennzahlen <- function(modell) {
  s <- summary(modell)
  round(c(R2 = s$r.squared, R2_angepasst = s$adj.r.squared,
          AIC = AIC(modell), BIC = BIC(modell)), 3)
}


# -----------------------------------------------------------------------------
# Aufgabe 1: Qualität des Weins (wine.csv)
# -----------------------------------------------------------------------------
pinot <- read_delim(pfad("wine.csv"), delim = ";", show_col_types = FALSE)

# Pearson-Korrelationen mit Signifikanz (nicht signifikante Felder bleiben leer)
C <- cor(pinot, method = "pearson")
test_res <- cor.mtest(pinot)
corrplot(C, p.mat = test_res$p, method = "circle", type = "lower", insig = "blank",
         addCoef.col = "black", number.cex = 0.8, order = "AOE", diag = FALSE)

# Modell 1: alle Variablen
mlr <- lm(quality ~ clarity + aroma + body + flavor + oakiness, data = pinot)
summary(mlr)   # partielle t-Tests je Koeffizient und Overall-F-Test
plot(fitted(mlr), residuals(mlr), xlab = "geschätzte Werte", ylab = "Residuen",
     main = "Residuenplot Modell 1")
abline(h = 0, lty = 2)

# Modell 2: nur Geschmack (flavor)
lr <- lm(quality ~ flavor, data = pinot)
summary(lr)

# Modell 3: Geschmack und Aroma
mlr3 <- lm(quality ~ flavor + aroma, data = pinot)
summary(mlr3)

# Vergleich: höheres angepasstes R^2 und kleineres AIC/BIC sprechen für ein Modell
rbind(alle_Variablen = kennzahlen(mlr), Geschmack = kennzahlen(lr),
      Geschmack_Aroma = kennzahlen(mlr3))
# AIC und BIC haben keine feste Skala; sie lassen sich nur zwischen Modellen
# für dieselben Daten vergleichen.


# -----------------------------------------------------------------------------
# Aufgabe 2: dieselben Modelle mit allen 38 Beobachtungen (wine_38.csv)
# -----------------------------------------------------------------------------
wine38 <- read_delim(pfad("wine_38.csv"), delim = ";", show_col_types = FALSE)

mlr_38  <- lm(quality ~ clarity + aroma + body + flavor + oakiness, data = wine38)
lr_38   <- lm(quality ~ flavor, data = wine38)
mlr3_38 <- lm(quality ~ flavor + aroma, data = wine38)
summary(mlr_38)

plot(fitted(mlr_38), residuals(mlr_38), xlab = "geschätzte Werte", ylab = "Residuen",
     main = "Residuenplot Modell 1 (38 Beobachtungen)")
abline(h = 0, lty = 2)

rbind(alle_Variablen = kennzahlen(mlr_38), Geschmack = kennzahlen(lr_38),
      Geschmack_Aroma = kennzahlen(mlr3_38))
# Vergleich mit Aufgabe 1: Ändern sich Vorzeichen, Signifikanz oder Rangfolge
# der Modelle, wenn alle Beobachtungen eingeschlossen werden?

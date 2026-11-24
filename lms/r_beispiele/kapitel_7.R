# =============================================================================
# R-Beispiele 25 bis 27 zu Kapitel 7: Tests für Zusammenhangsmaße
# Quelle: Textseiten „R Beispiel 25“, „R Beispiel 26“ und „R Beispiel 27“ im LMS
# Überarbeitet am 21.11.2026; Änderungen gegenüber dem LMS stehen in lms/AENDERUNGEN.md
# Benötigt nur Base R.
# =============================================================================


# -----------------------------------------------------------------------------
# Beispiel 25 (Kap. 7.1): Test auf Korrelation bei normalverteilter Grundgesamtheit
# -----------------------------------------------------------------------------
Cholesterin.1 <- c(226, 255, 247, 211, 289, 190, 231, 276, 241, 210)
Cholesterin.2 <- c(220, 244, 243, 211, 299, 170, 210, 276, 252, 189)

# H0: rho = 0 gegen H1: rho != 0
cor.test(Cholesterin.1, Cholesterin.2, method = "pearson",
         alternative = "two.sided", conf.level = 0.95)


# -----------------------------------------------------------------------------
# Beispiel 26 (Kap. 7.2): Pearson- und Spearman-Test im Vergleich
# -----------------------------------------------------------------------------
Niederschlag.Berlin      <- c(120.5, 120.9, 120.3, 121.3, 120.4, 120.2)
Niederschlag.Brandenburg <- c(122.1, 121.5, 121.7, 121.1, 122.9, 121.8)

cor.test(Niederschlag.Berlin, Niederschlag.Brandenburg,
         alternative = "two.sided", method = "pearson", conf.level = 0.95)
cor.test(Niederschlag.Berlin, Niederschlag.Brandenburg,
         alternative = "two.sided", method = "spearman")


# -----------------------------------------------------------------------------
# Beispiel 27 (Kap. 7.3): Chi-Quadrat-Test auf Unabhängigkeit und Anpassungstest
# -----------------------------------------------------------------------------
# a) Unabhängigkeitstest: zwei kategoriale Merkmale aus mtcars
#    am: Getriebe (0 = Automatik, 1 = Schaltgetriebe), vs: Motorbauart (0 = V-Motor, 1 = Reihe)
#    H0: am und vs sind unabhängig
data(mtcars)
tabelle <- table(Getriebe = mtcars$am, Motor = mtcars$vs)
tabelle
chisq.test(tabelle)            # Erwartete Häufigkeiten prüfen: chisq.test(tabelle)$expected
chisq.test(tabelle, correct = FALSE)  # ohne Yates-Korrektur, wie die Formel für T im Skript
fisher.test(tabelle)           # exakter Test, sinnvoll bei kleinen Häufigkeiten

# b) Anpassungstest: Ist ein Würfel fair?
#    120 Würfe; H0: alle sechs Augenzahlen haben die Wahrscheinlichkeit 1/6
wuerfe <- c(15, 22, 18, 25, 21, 19)
chisq.test(wuerfe, p = rep(1/6, 6))

# Hinweis: mpg ist stetig. Ein Chi-Quadrat-Test mit mpg als Merkmal behandelt jeden
# Messwert als eigene Kategorie und ist deshalb nicht sinnvoll.

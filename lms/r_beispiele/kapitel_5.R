# =============================================================================
# R-Beispiele 19 und 20 zu Kapitel 5: Statistische Tests
# Quelle: Textseiten „R Beispiel 19 - Kapitel 5.1“ (seit 21.11.2026, vorher auf
#         „R Beispiel 18-19“ in Kapitel 4) und „R Beispiel 20“ im LMS
# Überarbeitet am 21.11.2026; Änderungen gegenüber dem LMS stehen in lms/AENDERUNGEN.md
# Benötigt nur Base R.
# =============================================================================


# -----------------------------------------------------------------------------
# Beispiel 19 (Kap. 5.1): Annahme- und Ablehnungsbereich eines Gauß-Tests
# -----------------------------------------------------------------------------
# Stichprobe mit n = 50 aus N(0, 1). Getestet wird H0: mu = 0 bei bekanntem
# sigma = 1. Teststatistik: z = (x_quer - 0) / (sigma / sqrt(n)), unter H0 N(0, 1).
set.seed(123)
stichprobe <- rnorm(50, mean = 0, sd = 1)
n <- length(stichprobe)
z_beob <- mean(stichprobe) / (1 / sqrt(n))
alpha <- 0.05

# Hilfsfunktion: Dichte der N(0, 1) zeichnen und einen Bereich darunter füllen
flaeche <- function(von, bis, farbe) {
  x <- seq(von, bis, length.out = 200)
  polygon(c(von, x, bis), c(0, dnorm(x), 0), col = farbe, border = NA)
}
testplot <- function(titel, ablehnung, kritisch) {
  curve(dnorm(x), from = -4, to = 4, lwd = 2, ylim = c(0, 0.45),
        xlab = "Teststatistik z", ylab = "Dichte unter H0", main = titel)
  for (b in ablehnung) flaeche(b[1], b[2], rgb(1, 0, 0, 0.4))
  abline(v = kritisch, col = "red", lty = 2)
  abline(v = z_beob, col = "blue", lwd = 2)
  legend("topright", c("Ablehnungsbereich (Fläche = alpha)", "kritischer Wert",
                       "beobachtete Teststatistik"),
         fill = c(rgb(1, 0, 0, 0.4), NA, NA), border = c("black", NA, NA),
         col = c(NA, "red", "blue"), lty = c(NA, 2, 1), lwd = c(NA, 1, 2),
         cex = 0.75, bty = "n")
}

# Zweiseitiger Test: H0: mu = 0 gegen H1: mu != 0
z_krit <- qnorm(1 - alpha / 2)
testplot("Zweiseitiger Test", list(c(-4, -z_krit), c(z_krit, 4)), c(-z_krit, z_krit))
cat("zweiseitig: kritische Werte +/-", round(z_krit, 3),
    "| z =", round(z_beob, 3), "| p-Wert =", round(2 * pnorm(-abs(z_beob)), 4), "\n")

# Rechtsseitiger Test: H0: mu <= 0 gegen H1: mu > 0
z_krit <- qnorm(1 - alpha)
testplot("Rechtsseitiger Test", list(c(z_krit, 4)), z_krit)
cat("rechtsseitig: kritischer Wert", round(z_krit, 3),
    "| p-Wert =", round(1 - pnorm(z_beob), 4), "\n")

# Linksseitiger Test: H0: mu >= 0 gegen H1: mu < 0
z_krit <- qnorm(alpha)
testplot("Linksseitiger Test", list(c(-4, z_krit)), z_krit)
cat("linksseitig: kritischer Wert", round(z_krit, 3),
    "| p-Wert =", round(pnorm(z_beob), 4), "\n")


# -----------------------------------------------------------------------------
# Beispiel 20 (Kap. 5.2): t-Test, Jahresniederschlag
# -----------------------------------------------------------------------------
# Liegt der mittlere Jahresniederschlag bei 485 mm?
# H0: mu = 485 gegen H1: mu != 485, alpha = 0,05; Varianz unbekannt -> t-Test
Regen <- c(499.2, 555.2, 398.8, 391.9, 453.4,
           459.8, 483.7, 417.6, 469.2, 452.4,
           499.3, 340.6, 522.8, 469.9, 527.2,
           565.5, 584.1, 727.3, 558.6, 338.6)

t.test(Regen, alternative = "two.sided", mu = 485, conf.level = 0.95)
# p-Wert > 0,05: H0 wird nicht abgelehnt; 485 liegt im 95-%-Konfidenzintervall.

# =============================================================================
# R-Beispiele 14 bis 17 zu Kapitel 3: Induktive Statistik
# Quelle: Textseiten „R Beispiel 14“, „R Beispiel 15“ und „R Beispiel 16-17“ im LMS
# Überarbeitet am 21.11.2026; Änderungen gegenüber dem LMS stehen in lms/AENDERUNGEN.md
# Benötigt das Paket MASS (fitdistr).
# =============================================================================

if (!"MASS" %in% rownames(installed.packages())) {
  install.packages("MASS", repos = "https://cloud.r-project.org")
}

data(mtcars)


# -----------------------------------------------------------------------------
# Beispiel 14 (Kap. 3.2): Parameterschätzung für mtcars$mpg
# -----------------------------------------------------------------------------
# mpg = miles per gallon (Meilen je Gallone)
x <- mtcars$mpg
n <- length(x)

mu_hat   <- mean(x)                  # Schätzer für den Erwartungswert
S2       <- var(x)                   # erwartungstreuer Schätzer für die Varianz (n - 1)
sig2_hat <- (n - 1) / n * S2         # ML-Schätzer für die Varianz (Nenner n)
cat("mu_hat =", round(mu_hat, 3), "| S^2 =", round(S2, 3),
    "| sigma^2_ML =", round(sig2_hat, 3), "\n")

# Dieselbe Schätzung über die Maximum-Likelihood-Methode
anpassung <- MASS::fitdistr(x, densfun = "normal")
anpassung

# Histogramm, Kerndichteschätzer und angepasste Normalverteilung
hist(x, breaks = 10, freq = FALSE, col = "skyblue", ylim = c(0, 0.12),
     main = "Verteilung von mpg", xlab = "mpg", ylab = "Dichte")
lines(density(x), col = "red", lwd = 2)
curve(dnorm(x, mean = anpassung$estimate["mean"], sd = anpassung$estimate["sd"]),
      add = TRUE, col = "darkblue", lwd = 2, lty = 2)
legend("topright", c("Kerndichteschätzer", "angepasste Normalverteilung"),
       col = c("red", "darkblue"), lwd = 2, lty = c(1, 2), bty = "n")


# -----------------------------------------------------------------------------
# Beispiel 15 (Kap. 3.3.2): Maximum-Likelihood-Schätzung, Exponentialverteilung
# -----------------------------------------------------------------------------
# Lebensdauer von Autobatterien in Jahren; Annahme X ~ Exp(lambda)
Lebensdauer <- c(6.9, 1.3, 10.8, 10.3, 0.4,
                 6.5, 4.2, 21.8, 4.0, 2.7,
                 3.0, 41.3, 0.2, 15.3, 15.3,
                 5.5, 2.1, 2.5, 5.8, 9.1)

lambda_hat <- 1 / mean(Lebensdauer)  # ML-Schätzer
cat("lambda_hat =", round(lambda_hat, 4), "\n")
MASS::fitdistr(Lebensdauer, densfun = "exponential")   # Kontrolle

hist(Lebensdauer, breaks = 10, freq = FALSE, ylim = c(0, 0.15),
     col = rgb(0, 0, 1, 0.5), main = "Lebensdauer von Autobatterien in Jahren",
     xlab = "Jahre", ylab = "Dichte")
curve(dexp(x, rate = lambda_hat), from = 0, to = 50, add = TRUE,
      col = "red", lwd = 2)


# -----------------------------------------------------------------------------
# Beispiel 16 (Kap. 3.4.2): Methode der kleinsten Quadrate, mpg ~ hp
# -----------------------------------------------------------------------------
reg <- lm(mpg ~ hp, data = mtcars)
summary(reg)
y_dach <- fitted(reg)                # geschätzte Werte auf der Regressionsgeraden

# Jedes Residuum wird als Quadrat gezeichnet: Die Kleinste-Quadrate-Gerade macht
# die Summe dieser Flächen so klein wie möglich. Weil die Achsen verschiedene
# Einheiten haben, erscheinen die Quadrate als Rechtecke.
plot(mtcars$hp, mtcars$mpg, main = "Lineare Einfachregression mpg ~ hp",
     xlab = "PS (hp)", ylab = "Meilen je Gallone (mpg)", pch = 20,
     ylim = c(5, 35), col = "skyblue")
abline(reg, col = "red", lwd = 2)
for (i in seq_along(mtcars$hp)) {
  e <- y_dach[i] - mtcars$mpg[i]     # negatives Residuum
  rect(xleft = mtcars$hp[i], ybottom = min(mtcars$mpg[i], y_dach[i]),
       xright = mtcars$hp[i] + e, ytop = max(mtcars$mpg[i], y_dach[i]),
       border = rgb(1, 0, 1, 0.3), lwd = 2)
}


# -----------------------------------------------------------------------------
# Beispiel 17 (Kap. 3.4.2): Regression auf eine 0-1-Variable, mpg ~ am
# -----------------------------------------------------------------------------
# am: 0 = Automatik, 1 = Schaltgetriebe. Die Steigung ist die Differenz der
# Gruppenmittelwerte, der Achsenabschnitt der Mittelwert der Automatik-Gruppe.
reg2 <- lm(mpg ~ am, data = mtcars)
summary(reg2)
tapply(mtcars$mpg, mtcars$am, mean)  # Vergleich: Mittelwerte je Gruppe

plot(mtcars$am, mtcars$mpg, main = "Lineare Einfachregression mpg ~ am",
     xlab = "Getriebe (0 = Automatik, 1 = Schaltgetriebe)",
     ylab = "Meilen je Gallone (mpg)", pch = 20, ylim = c(5, 35),
     col = "darkgreen", xaxt = "n")
axis(1, at = c(0, 1))
abline(reg2, col = "red", lwd = 2)

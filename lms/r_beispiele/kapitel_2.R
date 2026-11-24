# =============================================================================
# R-Beispiele 8 bis 13 zu Kapitel 2: Ergänzungen zur Wahrscheinlichkeitstheorie
# Quelle: Textseiten „R Beispiel 8-11“, „R Beispiel 12“ und „R Beispiel 13“ im LMS
# Überarbeitet am 21.11.2026; Änderungen gegenüber dem LMS stehen in lms/AENDERUNGEN.md
# Benötigt die Pakete MASS, mnormt und scatterplot3d.
# =============================================================================

# Pakete installieren, falls sie fehlen, und laden
pakete  <- c("MASS", "mnormt", "scatterplot3d")
fehlend <- setdiff(pakete, rownames(installed.packages()))
if (length(fehlend) > 0) install.packages(fehlend, repos = "https://cloud.r-project.org")

library(MASS)          # mvrnorm()
library(mnormt)        # dmnorm(), pmnorm()
library(scatterplot3d) # scatterplot3d()


# -----------------------------------------------------------------------------
# Beispiel 8 (Kap. 2.1.1): Stichprobe aus einer zweidimensionalen Normalverteilung
# -----------------------------------------------------------------------------
set.seed(123)
mu    <- c(0, 0)                                    # Erwartungswerte beider Merkmale
sigma <- matrix(c(1, 0, 0, 1), nrow = 2, ncol = 2)  # Kovarianzmatrix
punkte <- mvrnorm(n = 10000, mu = mu, Sigma = sigma)

plot(punkte, col = rgb(0, 0, 1, 0.3), pch = 20, xlab = "Merkmal 1",
     ylab = "Merkmal 2", main = "Zweidimensionale Normalverteilung",
     asp = 1)


# -----------------------------------------------------------------------------
# Beispiel 9 (Kap. 2.1.1): Gemeinsame Dichte f(x1, x2) mit Korrelation 0,8
# -----------------------------------------------------------------------------
n1 <- 80
n2 <- 80
x1 <- seq(from = -2, to = 2, length.out = n1)
x2 <- seq(from = -2, to = 2, length.out = n2)
cov_matr <- matrix(c(1, 0.8, 0.8, 1), 2, 2)

f12 <- matrix(NA, nrow = n1, ncol = n2)
for (i in 1:n1) {
  for (j in 1:n2) {
    f12[i, j] <- dmnorm(x = c(x1[i], x2[j]), mean = c(0, 0), varcov = cov_matr)
  }
}
persp(x = x1, y = x2, z = f12, theta = -30, phi = 15, expand = 0.75, r = 4,
      col = rgb(1, 1, 0, 0.75), ltheta = 120, shade = 0.75,
      ticktype = "detailed", xlab = "x1", ylab = "x2", zlab = "f(x1, x2)",
      main = "Gemeinsame Dichte")


# -----------------------------------------------------------------------------
# Beispiel 10 (Kap. 2.1.1): Gemeinsame Verteilungsfunktion F(x1, x2)
# -----------------------------------------------------------------------------
F12 <- matrix(NA, nrow = n1, ncol = n2)
for (i in 1:n1) {
  for (j in 1:n2) {
    F12[i, j] <- pmnorm(x = c(x1[i], x2[j]), mean = c(0, 0), varcov = cov_matr)
  }
}
persp(x = x1, y = x2, z = F12, theta = -30, phi = 15, expand = 0.75, r = 4,
      col = rgb(1, 1, 1, 0.75), ltheta = 120, shade = 0.75,
      ticktype = "detailed", xlab = "x1", ylab = "x2", zlab = "F(x1, x2)",
      main = "Gemeinsame Verteilungsfunktion")


# -----------------------------------------------------------------------------
# Beispiel 11 (Kap. 2.1.1): Schnitte durch die gemeinsame Dichte
# -----------------------------------------------------------------------------
# Für drei feste Werte von x2 wird f(x1, x2) als Funktion von x1 gezeigt.
# Die Schnitte haben die Form der bedingten Dichten f(x1 | x2); zur bedingten
# Dichte fehlt nur noch die Division durch die Randdichte f2(x2).
scatterplot3d(x = cbind(x1, x1, x1),
              y = cbind(rep(x2[10], n1), rep(x2[40], n1), rep(x2[70], n1)),
              z = cbind(f12[, 10], f12[, 40], f12[, 70]),
              ylim = c(-2, 2), scale.y = 1, angle = 120,
              color = rgb(0, 0, 1, 0.75), pch = NA_integer_, type = "h",
              xlab = "x1", ylab = "x2", zlab = "f(x1, x2)",
              main = "Schnitte der gemeinsamen Dichte für feste x2")


# -----------------------------------------------------------------------------
# Beispiel 12 (Kap. 2.3.1): Schwaches Gesetz der großen Zahlen
# -----------------------------------------------------------------------------
# Das arithmetische Mittel vieler Würfelwürfe nähert sich dem Erwartungswert 3,5.
set.seed(123)
n_max <- 5000
wuerfe <- sample(1:6, n_max, replace = TRUE)
laufendes_mittel <- cumsum(wuerfe) / seq_len(n_max)

plot(laufendes_mittel, type = "l", ylim = c(1, 6), col = "darkblue",
     xlab = "Anzahl der Würfe n", ylab = "arithmetisches Mittel",
     main = "Schwaches Gesetz der großen Zahlen")
abline(h = 3.5, col = "red", lty = 2, lwd = 2)

# Das schwache Gesetz sagt: P(|Mittel - 3,5| > epsilon) geht für jedes epsilon > 0
# gegen 0. Die Simulation schätzt diese Wahrscheinlichkeit für epsilon = 0,1.
epsilon <- 0.1
for (n in c(10, 100, 1000, 5000)) {
  abweichung <- replicate(2000, abs(mean(sample(1:6, n, replace = TRUE)) - 3.5) > epsilon)
  cat("n =", n, ": geschätzte P(|Mittel - 3,5| > 0,1) =", mean(abweichung), "\n")
}


# -----------------------------------------------------------------------------
# Beispiel 13 (Kap. 2.3.2): Zentraler Grenzwertsatz
# -----------------------------------------------------------------------------
# Je n = 25 Zufallsvariablen werden m = 1.000-mal simuliert. Die standardisierten
# Mittelwerte sqrt(n) * (x_quer - mu) / sigma folgen näherungsweise N(0, 1),
# gleich ob die Ausgangsverteilung gleich-, binomial- oder chi-Quadrat-verteilt ist.
n <- 25
m <- 1000
set.seed(65535)
z <- seq(-4, 4, by = 0.01)

par(mfrow = c(1, 3))

# Gleichverteilung U(10, 30)
unif_a <- 10
unif_b <- 30
mu   <- (unif_a + unif_b) / 2
sig2 <- (unif_b - unif_a)^2 / 12
x_bar <- replicate(m, mean(runif(n, min = unif_a, max = unif_b)))
x_bar_norm <- sqrt(n) * (x_bar - mu) / sqrt(sig2)
hist(x_bar_norm, prob = TRUE, nclass = 20, main = "Gleichverteilung",
     xlab = "standardisiertes Mittel", ylab = "Dichte")
lines(z, dnorm(z), col = "red", lwd = 2)

# Binomialverteilung B(25; 0,8)
binom_n <- n
binom_p <- 0.8
mu   <- binom_n * binom_p
sig2 <- binom_n * binom_p * (1 - binom_p)
x_bar <- replicate(m, mean(rbinom(n, size = binom_n, prob = binom_p)))
x_bar_norm <- sqrt(n) * (x_bar - mu) / sqrt(sig2)
hist(x_bar_norm, prob = TRUE, nclass = 20, main = "Binomialverteilung",
     xlab = "standardisiertes Mittel", ylab = "Dichte")
lines(z, dnorm(z), col = "red", lwd = 2)

# Chi-Quadrat-Verteilung mit 5 Freiheitsgraden
chisq_df <- 5
mu   <- chisq_df
sig2 <- 2 * chisq_df
x_bar <- replicate(m, mean(rchisq(n, df = chisq_df)))
x_bar_norm <- sqrt(n) * (x_bar - mu) / sqrt(sig2)
hist(x_bar_norm, prob = TRUE, nclass = 20, main = "Chi-Quadrat-Verteilung",
     xlab = "standardisiertes Mittel", ylab = "Dichte")
lines(z, dnorm(z), col = "red", lwd = 2)

par(mfrow = c(1, 1))

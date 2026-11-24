# 2. Kapitel: Beispiele zum 2. Kapitel, noch einmal als Hinführung zu den Themen -----
# 2.1 Zweidimensionale Verteilungsfunktionen ----
# Beispiel: Diskrete zweidimensionale Verteilungsfunktion

# Diskrete gemeinsame Wahrscheinlichkeitsverteilung
X <- c(0, 1, 2)
Y <- c(0, 1, 2)
joint_prob <- matrix(c(0.1, 0.1, 0.1,
                       0.1, 0.2, 0.1,
                       0.1, 0.1, 0.1), nrow = 3, byrow = TRUE)

# Randverteilungen
marginal_X <- rowSums(joint_prob) # für X
marginal_Y <- colSums(joint_prob) # für Y

# Ausgabe
print("Gemeinsame Wahrscheinlichkeitsverteilung:")
print(joint_prob)
print("Randverteilung für X:")
print(marginal_X)
print("Randverteilung für Y:")
print(marginal_Y)

# Beispiel: 2D-Normalverteilung visualisieren

library(MASS)

mu <- c(0, 0) # Mittelwerte
sigma <- matrix(c(1, 0.8, 0.8, 1), 2, 2) # Kovarianzmatrix

# Generieren von Zufallsdaten
set.seed(123)
data <- mvrnorm(n = 1000, mu = mu, Sigma = sigma)

# Scatterplot
plot(data, main = "2D-Normalverteilung", xlab = "X1", ylab = "X2", pch = 19, col = rgb(0, 0, 1, 0.5))

# 2.2 Ungleichung von Tschebyscheff -----
# Beispiel: Anwendung der Tschebyscheff-Ungleichung

# Tschebyscheff: P(|X - mu| < epsilon) >= 1 - sigma^2 / epsilon^2

# Mittelwert und Varianz
mu <- 5
sigma2 <- 4

# Abstand epsilon: zwei Standardabweichungen (sigma = 2)
# (Bei epsilon = sigma = 2 ergäbe sich nur die nutzlose Schranke 0.)
epsilon <- 4

# Untere Schranke für die Wahrscheinlichkeit
P <- 1 - sigma2 / epsilon^2
P   # mindestens 75 % der Werte liegen im Intervall (1, 9)

# Vergleich: Ist X normalverteilt, liegen tatsächlich rund 95 % im Intervall
pnorm(mu + epsilon, mean = mu, sd = sqrt(sigma2)) - pnorm(mu - epsilon, mean = mu, sd = sqrt(sigma2))

# 2.3 Grenzwertsätze ---------
# Beispiel: Würfelspiel und Grenzwertsätze

set.seed(123)
n <- 10000  # Anzahl Würfe
rolls <- sample(1:6, n, replace = TRUE)

# Berechnung der relativen Häufigkeit für eine Sechs
relative_freq <- cumsum(rolls == 6) / (1:n)

# Plot
plot(relative_freq, type = "l", col = "blue", ylim = c(0, 0.2),
     main = "Relative Häufigkeit für eine Sechs", xlab = "Anzahl der Würfe", ylab = "Relative Häufigkeit")
abline(h = 1/6, col = "red", lty = 2) # Erwartungswert

# Beispiel: Zentraler Grenzwertsatz

# Ausgangsverteilung ist die schiefe Exponentialverteilung: Wären die Daten selbst
# normalverteilt, wäre auch der Mittelwert exakt normalverteilt, und der
# zentrale Grenzwertsatz hätte nichts zu zeigen.
set.seed(123)
n <- 30 # Stichprobengröße
mu <- 5 # Erwartungswert der Exponentialverteilung (rate = 1/5)
sigma <- 5 # Standardabweichung der Exponentialverteilung

# Generieren von Stichproben
samples <- replicate(1000, mean(rexp(n, rate = 1 / mu)))

# Histogramm und Normalverteilung
hist(samples, probability = TRUE, main = "Zentraler Grenzwertsatz (Exponentialverteilung)",
     xlab = "Mittelwert", ylab = "Dichte", col = "lightblue", breaks = 20)
curve(dnorm(x, mean = mu, sd = sigma / sqrt(n)), col = "red", lwd = 2, add = TRUE)

# Beispiel: Konvergenz der empirischen Verteilungsfunktion

set.seed(123)
n <- 1000 # Stichprobengröße
data <- rnorm(n, mean = 0, sd = 1)

# Empirische Verteilungsfunktion
empirical_cdf <- ecdf(data)

# Plot
plot(empirical_cdf, main = "Empirische vs. Theoretische Verteilungsfunktion",
     xlab = "x", ylab = "F(x)", col = "blue")
curve(pnorm(x, mean = 0, sd = 1), col = "red", lty = 2, add = TRUE)
legend("topleft", legend = c("Empirisch", "Theoretisch"), col = c("blue", "red"), lty = 1:2)

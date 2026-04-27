# 2. Kapitel: In diesem Kapitel folgen die Beispiel zum 2. Kapitel und dienen noch einmal als Themen hinführung -----
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

# Mittelwert und Varianz
mu <- 5
sigma2 <- 4

# Abstand epsilon
epsilon <- 2

# Wahrscheinlichkeit
P <- 1 - sigma2 / epsilon^2
P

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

set.seed(123)
n <- 30 # Stichprobengröße
mu <- 5 # Mittelwert
sigma <- 2 # Standardabweichung

# Generieren von Stichproben
samples <- replicate(1000, mean(rnorm(n, mean = mu, sd = sigma)))

# Histogramm und Normalverteilung
hist(samples, probability = TRUE, main = "Zentraler Grenzwertsatz",
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

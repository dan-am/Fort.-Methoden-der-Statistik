# =============================================================================
# Übung Kapitel 2: Ergänzungen zur Wahrscheinlichkeitstheorie
# Themen: 2-dim Verteilungen, Tschebyscheff, Schwaches GGZ, Zentraler Grenzwertsatz
# Datensätze: iris (R-base), MASS::mvrnorm
# =============================================================================


# -----------------------------------------------------------------------------
# Aufgabe 1 – Zweidimensionale Verteilung & Randverteilungen
# -----------------------------------------------------------------------------
# Verwende den iris-Datensatz. Betrachte die beiden Merkmale
# X = Sepal.Length, Y = Petal.Length.
#
# a) Erstelle einen Scatterplot von Y gegen X.
# b) Berechne E(X), E(Y), Var(X), Var(Y) und Cov(X,Y) sowie die Korrelation.
# c) Visualisiere die geschätzten Randverteilungen (Histogramme) von X und Y
#    nebeneinander (par(mfrow = c(1,2))).
#


# falls noch nicht installiert
install.packages("plotly", dependencies = TRUE)
install.packages("MASS", dependencies = TRUE)
install.packages("ggplot2", dependencies = TRUE)


# Lösungsskizze:
library(MASS)
library(ggplot2)
library(plotly)

data(iris)
X <- iris$Sepal.Length
Y <- iris$Petal.Length

# a)
plot(X, Y, pch = 20, col = "steelblue",
     xlab = "Sepal.Length", ylab = "Petal.Length",
     main = "2-dim Verteilung: Sepal vs. Petal")

# b)
mean(X); mean(Y)
var(X);  var(Y)
cov(X, Y)
cor(X, Y)

# c)
par(mfrow = c(1, 2))
hist(X, col = "skyblue", main = "Randverteilung Sepal.Length", xlab = "X")
hist(Y, col = "salmon",  main = "Randverteilung Petal.Length", xlab = "Y")
par(mfrow = c(1, 1))

# ein Merkmal auf die 3 Blumentypen
ggplot(iris, aes(x = Sepal.Length, fill = Species, color = Species)) +
  geom_histogram(binwidth = 0.2, position = "identity", alpha = 0.6) +
  ggtitle("Randverteilung Sepal.Length") +
  xlab("Sepal.Length") + ylab("Häufigkeit")

# 3D Darstellung eines Scatterplots mit plotly
library(plotly)
plot_ly(iris, x = ~Sepal.Length, y = ~Petal.Length, 
z = ~Petal.Width, color = ~Species, colors = c("red", "green", "blue")) %>%
  add_markers() %>%
  layout(title = "3D Scatterplot: Sepal.Length vs. Petal.Length vs. Petal.Width",
         scene = list(xaxis = list(title = "Sepal.Length"),
                      yaxis = list(title = "Petal.Length"),
                      zaxis = list(title = "Petal.Width")))

# Heatmap 
ggplot(iris, aes(x = Sepal.Length, y = Petal.Length)) +
  geom_bin2d(bins = 30) +
  ggtitle("Heatmap: Sepal.Length vs. Petal.Length") +
  xlab("Sepal.Length") + ylab("Petal.Length") +
  scale_fill_gradient(low = "lightblue", high = "darkblue")

# -----------------------------------------------------------------------------
# Aufgabe 2 – 2-dimensionale Normalverteilung simulieren
# -----------------------------------------------------------------------------
# Erzeuge mit MASS::mvrnorm n = 5000 Beobachtungen einer 2-dim
# Normalverteilung mit mu = (0,0) und Korrelation rho.
#
# Vergleiche grafisch rho = 0 und rho = 0.8.

# install.packages("MASS")
set.seed(123)

simuliere_2dnormal <- function(rho, n = 5000) {
  Sigma <- matrix(c(1, rho, rho, 1), nrow = 2)
  mvrnorm(n = n, mu = c(0, 0), Sigma = Sigma)
}

dat0   <- data.frame(simuliere_2dnormal(0))
dat08  <-  data.frame(simuliere_2dnormal(0.8))

par(mfrow = c(1, 2))
plot(dat0,  pch = 20, col = "blue",
     main = "rho = 0",   xlab = "X1", ylab = "X2")
plot(dat08, pch = 20, col = "red",
     main = "rho = 0.8", xlab = "X1", ylab = "X2")
par(mfrow = c(1, 1))


# Heatmap 
ggplot(dat08, aes(x = X1, y = X2)) +
  geom_bin2d(bins = 30) +
  xlab("X1") + ylab("X2") +
  scale_fill_gradient(low = "lightblue", high = "darkblue")

# -----------------------------------------------------------------------------
# Aufgabe 3 – Tschebyscheff-Ungleichung
# -----------------------------------------------------------------------------
# Sei X ~ N(0, 1). Vergleiche für epsilon = 1, 2, 3:
#   - tatsächliche Wahrscheinlichkeit P(|X - mu| > epsilon)
#   - obere Schranke laut Tschebyscheff: Var(X) / epsilon^2
#
# Was beobachtest du?
set.seed(123)

mu  <- 0
sig <- 1

eps <- c(1, 2, 3)
P_tatsaechlich <- 2 * (1 - pnorm(eps, mean = mu, sd = sig))
P_tschebyscheff <- sig^2 / eps^2

vergleich <- data.frame(
  epsilon       = eps,
  tatsaechlich  = round(P_tatsaechlich, 4),
  tschebyscheff = round(P_tschebyscheff, 4)
)
print(vergleich)
# -> Tschebyscheff ist eine (sehr lockere) obere Schranke.


# -----------------------------------------------------------------------------
# Aufgabe 4 – Schwaches Gesetz der großen Zahlen (Würfelspiel)
# -----------------------------------------------------------------------------
# Werfe einen fairen Würfel n-mal und betrachte die relative Häufigkeit
# einer "6". Plotte den Verlauf des arithmetischen Mittels für n = 1..5000.
# Eingezeichnet werden soll außerdem der Erwartungswert 1/6.

set.seed(42)
n <- 5000
wuerfel  <- sample(1:6, n, replace = TRUE)
sechs    <- as.numeric(wuerfel == 6)
rel_haeufigkeit <- cumsum(sechs) / seq_len(n)

plot(rel_haeufigkeit, type = "l", col = "darkblue",
     ylab = "relative Häufigkeit der 6", xlab = "n",
     main = "Schwaches Gesetz der großen Zahlen")
abline(h = 1/6, col = "red", lwd = 2, lty = 2)


# -----------------------------------------------------------------------------
# Aufgabe 5 – Zentraler Grenzwertsatz (Simulationsstudie)
# -----------------------------------------------------------------------------
# Ziehe m = 1000 Stichproben vom Umfang n = 30 aus einer Gleichverteilung
# U(10, 30). Berechne jeweils das standardisierte Mittel
#   z = sqrt(n) * (X_bar - mu) / sigma
# und vergleiche das Histogramm mit der Standardnormalverteilung.

set.seed(65535)
n <- 30
m <- 1000
unif_a <- 10
unif_b <- 30

mu   <- (unif_a + unif_b) / 2
sig2 <- (unif_b - unif_a)^2 / 12

x_bar <- replicate(m, mean(runif(n, min = unif_a, max = unif_b)))
z     <- sqrt(n) * (x_bar - mu) / sqrt(sig2)

hist(z, prob = TRUE, nclass = 20, col = "lightgray",
     main = "ZG: standardisierter Mittelwert", xlab = "z")
curve(dnorm, from = -4, to = 4, add = TRUE, col = "red", lwd = 2)

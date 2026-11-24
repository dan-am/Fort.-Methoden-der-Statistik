# =============================================================================
# R-Beispiele 1 bis 7 zu Kapitel 1: Wahrscheinlichkeitstheorie
# Quelle: Textseiten „R Beispiel 1“ bis „R Beispiel 7“ im LMS (Kurs FMS01)
# Überarbeitet am 21.11.2026; Änderungen gegenüber dem LMS stehen in lms/AENDERUNGEN.md
# Beispiel 3 und 4 liegen seit 22.11.2026 im Archiv des LMS-Kurses (Kapitel 1 als Auffrischung).
# Benötigt nur Base R.
# =============================================================================


# -----------------------------------------------------------------------------
# Beispiel 1 (Kap. 1.5): Verteilungsfunktion und Dichte
# -----------------------------------------------------------------------------
# Zufallsstichprobe mit 50 Beobachtungen aus einer Normalverteilung
# mit Mittelwert 50 und Standardabweichung 10
set.seed(123)                          # zur Reproduzierbarkeit
stichprobe <- rnorm(50, mean = 50, sd = 10)

dichte <- density(stichprobe)          # geschätzte Dichtefunktion
vf     <- ecdf(stichprobe)             # empirische Verteilungsfunktion

# Histogramm mit Dichte
hist(stichprobe, freq = FALSE, main = "Histogramm mit Dichte",
     xlab = "x", ylab = "Dichte")
lines(dichte, col = "red", lwd = 2)

# Empirische Verteilungsfunktion
plot(vf, main = "Empirische Verteilungsfunktion", xlab = "x", ylab = "F(x)")


# -----------------------------------------------------------------------------
# Beispiel 2 (Kap. 1.6.1): Binomialverteilung (aus der Vorlesung)
# -----------------------------------------------------------------------------
n <- 50     # Anzahl der Versuche (Patienten)
p <- 0.9    # Wahrscheinlichkeit, dass das Medikament wirkt
x <- 0:n    # mögliche Anzahl von Erfolgen

wahrscheinlichkeiten <- dbinom(x, n, p)
barplot(wahrscheinlichkeiten, names.arg = x,
        xlab = "Anzahl Erfolge", ylab = "Wahrscheinlichkeit",
        main = "Binomialverteilung B(50; 0,9)")

# Wahrscheinlichkeit, dass das Medikament bei genau 48 von 50 Patienten wirkt
dbinom(48, n, p)


# -----------------------------------------------------------------------------
# Beispiel 3 (Kap. 1.9.2): Hypergeometrische Verteilung
# -----------------------------------------------------------------------------
# Eine Urne enthält 40 Kugeln, davon 5 rote und 35 blaue.
# Wir ziehen 4 Kugeln ohne Zurücklegen; X = Anzahl der roten Kugeln.
N <- 40     # Anzahl der Kugeln in der Urne
K <- 5      # Anzahl der roten Kugeln
n <- 4      # Anzahl der gezogenen Kugeln
x <- 0:n    # X kann nur Werte von 0 bis n annehmen

# Wahrscheinlichkeitsfunktion P(X = x)
plot(x, dhyper(x, K, N - K, n), type = "h", lwd = 3,
     xlab = "Anzahl roter Kugeln", ylab = "P(X = x)",
     main = "Hypergeometrische Verteilung: Wahrscheinlichkeitsfunktion")

# Verteilungsfunktion P(X <= x) als Treppenfunktion
plot(x, phyper(x, K, N - K, n), type = "s", lwd = 2, ylim = c(0, 1),
     xlab = "Anzahl roter Kugeln", ylab = "F(x) = P(X <= x)",
     main = "Hypergeometrische Verteilung: Verteilungsfunktion")

# Wahrscheinlichkeit für genau 2 rote Kugeln
dhyper(2, K, N - K, n)


# -----------------------------------------------------------------------------
# Beispiel 4 (Kap. 1.9.3): Poissonverteilung
# -----------------------------------------------------------------------------
lambda <- 3   # im Mittel 3 Unfälle pro Zeiteinheit
x <- 0:12

plot(x, dpois(x, lambda), type = "h", lwd = 3,
     xlab = "Anzahl Unfälle", ylab = "P(X = x)",
     main = "Poissonverteilung mit lambda = 3")

# Wahrscheinlichkeit für genau 2 Unfälle
dpois(2, lambda)


# -----------------------------------------------------------------------------
# Beispiel 5 (Kap. 1.7): Stetige Gleichverteilung
# -----------------------------------------------------------------------------
set.seed(123)
x <- runif(1000, min = 0, max = 30)   # 1.000 gleichverteilte Zufallszahlen

par(mfrow = c(1, 2))                  # zwei Grafiken nebeneinander
hist(x, freq = FALSE, ylab = "Dichte", main = "Dichte der Gleichverteilung")
curve(dunif(x, min = 0, max = 30), add = TRUE, col = "blue", lwd = 2)
plot(ecdf(x), main = "Verteilungsfunktion der Gleichverteilung", ylab = "F(x)")
curve(punif(x, min = 0, max = 30), add = TRUE, col = "blue", lwd = 2)
par(mfrow = c(1, 1))


# -----------------------------------------------------------------------------
# Beispiel 6 (Kap. 1.7.1): Normalverteilung
# -----------------------------------------------------------------------------
mu    <- c(0, 2, -2)       # Erwartungswerte
sigma <- c(1, 0.5, 2)      # Standardabweichungen
farben <- c("black", "red", "darkgreen")
x <- seq(-7, 7, length.out = 1000)

plot(x, dnorm(x, mean = mu[1], sd = sigma[1]), type = "l", lwd = 2,
     col = farben[1], ylim = c(0, 0.8), xlab = "x", ylab = "Dichte f(x)",
     main = "Dichten der Normalverteilung")
for (i in 2:3) {
  lines(x, dnorm(x, mean = mu[i], sd = sigma[i]), lwd = 2, col = farben[i])
}
legend("topright", legend = paste0("mu = ", mu, ", sigma = ", sigma),
       col = farben, lwd = 2, bty = "n")


# -----------------------------------------------------------------------------
# Beispiel 7 (Kap. 1.7.2): Exponentialverteilung (aus der Vorlesung)
# -----------------------------------------------------------------------------
curve(dexp(x, rate = 0.5), from = 0, to = 10, ylim = c(0, 1.5), lwd = 2,
      col = "blue", xlab = "x", ylab = "Dichte f(x)",
      main = "Dichten der Exponentialverteilung")
curve(dexp(x, rate = 1),   from = 0, to = 10, lwd = 2, col = "red",   add = TRUE)
curve(dexp(x, rate = 1.5), from = 0, to = 10, lwd = 2, col = "green", add = TRUE)
legend("topright", legend = c("lambda = 0,5", "lambda = 1", "lambda = 1,5"),
       col = c("blue", "red", "green"), lwd = 2, bty = "n")

# =============================================================================
# Lösung zu Übung 4 (Kapitel 1: Auffrischung in R)
# Quelle: Aufgabe „Übung 4: Auffrischung in R“ und Seite „Lösung Übung 4“ im LMS
# Neu gefasst am 22.11.2026 (Paket 10). Die frühere Übung, eine R-Simulation von
# Verteilungen, steht wortgleich im Bachelormodul AMS02 und wurde ersetzt.
# Benötigt nur Base R.
# =============================================================================

set.seed(2026)


# -----------------------------------------------------------------------------
# Aufgabe 1: Normalverteilung, X ~ N(100, 15^2)
# -----------------------------------------------------------------------------
pnorm(120, mean = 100, sd = 15)                                   # P(X <= 120) = 0,9088
pnorm(115, mean = 100, sd = 15) - pnorm(85, mean = 100, sd = 15)  # P(85 < X <= 115) = F(115) - F(85) = 0,6827
qnorm(0.95, mean = 100, sd = 15)                                  # 95-%-Quantil = 124,67

x <- rnorm(10000, mean = 100, sd = 15)
mean(x > 85 & x <= 115)                                           # Simulation: 0,6842
# Rund 68 % der Werte liegen höchstens eine Standardabweichung vom Erwartungswert entfernt.


# -----------------------------------------------------------------------------
# Aufgabe 2: Binomialverteilung, X ~ B(50; 0,9)
# -----------------------------------------------------------------------------
dbinom(48, size = 50, prob = 0.9)        # P(X = 48) = 0,0779
1 - pbinom(44, size = 50, prob = 0.9)    # P(X >= 45) = 1 - P(X <= 44) = 0,6161

y <- rbinom(10000, size = 50, prob = 0.9)
c(mean(y == 48), mean(y >= 45))          # Simulation: 0,0761 und 0,6136


# -----------------------------------------------------------------------------
# Aufgabe 3: Exponentialverteilung mit lambda = 0,5
# -----------------------------------------------------------------------------
z <- rexp(10000, rate = 0.5)
c(Mittelwert = mean(z), Theorie = 1 / 0.5)     # 1,97 gegen 2
c(Varianz = var(z), Theorie = 1 / 0.5^2)       # 3,90 gegen 4

hist(z, breaks = 60, freq = FALSE, col = "grey85", border = "white",
     xlab = "x", ylab = "Dichte", main = "Exponentialverteilung mit lambda = 0,5")
curve(dexp(x, rate = 0.5), add = TRUE, col = "red", lwd = 2)


# -----------------------------------------------------------------------------
# Aufgabe 4: Kovarianz und Korrelation von X und Y = 2X + eps
# -----------------------------------------------------------------------------
x <- rnorm(10000)
eps <- rnorm(10000)
y <- 2 * x + eps

# Theorie: Cov(X, Y) = Cov(X, 2X + eps) = 2 Var(X) = 2, weil X und eps unabhängig sind.
#          Var(Y) = 4 Var(X) + Var(eps) = 5, also Corr(X, Y) = 2 / sqrt(1 * 5) = 0,894.
c(Kovarianz = cov(x, y), Theorie = 2)                 # 2,01 gegen 2
c(Korrelation = cor(x, y), Theorie = 2 / sqrt(5))     # 0,894 gegen 0,894

plot(x, y, pch = 16, cex = 0.3, col = rgb(0, 0, 0, 0.2),
     xlab = "X", ylab = "Y", main = "Y = 2X + eps")

# =============================================================================
# Übung Kapitel 3: Induktive Statistik
# Themen: Parameterschätzung, Maximum-Likelihood, Methode der kleinsten
#         Fehlerquadrate (lineare Regression), Erwartungstreue, MSE, Konsistenz
# Datensätze: mtcars, faithful (R-base) sowie Lebensdauer-Daten aus der Vorlesung
# =============================================================================


# -----------------------------------------------------------------------------
# Aufgabe 1 – Parameterschätzer für mtcars
# -----------------------------------------------------------------------------
# Verwende die Variable mtcars$mpg.
#
# a) Schätze E(X) (= mu) und Var(X) (= sigma^2) durch
#       mu_hat   = Mittelwert
#       sig2_hat = empirische Varianz   (1/n   * sum((x - x_bar)^2))
#       S2       = korrigierte Varianz  (1/(n-1) * sum((x - x_bar)^2))  =  var()
# b) Plotte das Histogramm zusammen mit dem Kerndichteschätzer.

data(mtcars)
x <- mtcars$mpg
n <- length(x)

mu_hat   <- mean(x)
sig2_hat <- mean((x - mu_hat)^2)            # MLE / empirische Varianz
S2       <- var(x)                          # erwartungstreu (n-1 im Nenner)

cat("mu_hat   =", round(mu_hat, 3),
    "\nsig2_hat =", round(sig2_hat, 3),
    "\nS^2      =", round(S2, 3), "\n")

hist(x, breaks = 10, freq = FALSE,
     col = "skyblue", main = "mpg: Histogramm + Dichteschätzer",
     xlab = "mpg")
lines(density(x), col = "red", lwd = 2)


# -----------------------------------------------------------------------------
# Aufgabe 2 – Maximum-Likelihood: Exponentialverteilung
# -----------------------------------------------------------------------------
# Die folgenden Werte sind Lebensdauern (in Jahren) von Autobatterien
# (Beispiel aus der Vorlesung). Es wird X ~ Exp(lambda) angenommen.
#
# a) Bestimme den ML-Schätzer  lambda_hat = 1 / x_bar.
# b) Vergleiche das Histogramm mit der angepassten Exp-Dichte.
# c) Schätze P(X > 10).

Lebensdauer <- c(6.9, 1.3, 10.8, 10.3, 0.4,
                 6.5, 4.2, 21.8,  4.0, 2.7,
                 3.0, 41.3,  0.2, 15.3, 15.3,
                 5.5, 2.1,   2.5,  5.8, 9.1)

lambda_hat <- 1 / mean(Lebensdauer)
cat("lambda_hat =", round(lambda_hat, 5), "\n")

hist(Lebensdauer, breaks = 10, freq = FALSE, ylim = c(0, 0.15),
     col = rgb(0, 0, 1, 0.5), main = "Lebensdauer Autobatterien",
     xlab = "Jahre")
curve(dexp(x, rate = lambda_hat), add = TRUE, col = "red", lwd = 2)

# c)
P_X_gt_10 <- 1 - pexp(10, rate = lambda_hat)
cat("P(X > 10) ≈", round(P_X_gt_10, 3), "\n")


# -----------------------------------------------------------------------------
# Aufgabe 3 – Methode der kleinsten Fehlerquadrate (lineare Regression)
# -----------------------------------------------------------------------------
# Zusammenhang zwischen Pferdestärken (hp) und Verbrauch (mpg) im mtcars-Datensatz.
#
# a) Schätze beta_0 und beta_1 von Hand mit den Formeln aus der Vorlesung
#    und vergleiche mit lm().
# b) Plotte die Regressionsgerade.
# c) Sage den mpg-Wert für hp = 150 voraus.

X <- mtcars$hp
Y <- mtcars$mpg

# a) "Per Hand"
xq <- mean(X);  yq <- mean(Y)
beta1_hat <- sum((X - xq) * (Y - yq)) / sum((X - xq)^2)
beta0_hat <- yq - beta1_hat * xq
cat("Per Hand: beta0 =", round(beta0_hat, 3),
    ", beta1 =", round(beta1_hat, 4), "\n")

reg <- lm(mpg ~ hp, data = mtcars)
print(coef(reg))     # Vergleich

# b)
plot(X, Y, pch = 20, col = "skyblue",
     xlab = "horse power", ylab = "miles per gallon",
     main = "Lineare Einfachregression mpg ~ hp")
abline(reg, col = "red", lwd = 2)

# c)
neuer_punkt <- data.frame(hp = 150)
predict(reg, newdata = neuer_punkt)


# -----------------------------------------------------------------------------
# Aufgabe 4 – Erwartungstreue & MSE: empirische vs. korrigierte Varianz
# -----------------------------------------------------------------------------
# Simuliere m = 5000 Stichproben vom Umfang n = 10 aus N(0, 4).
# Berechne in jeder Stichprobe sig2_hat (mit n) und S^2 (mit n-1).
# Zeige empirisch:
#   E(sig2_hat) ≈ (n-1)/n * sigma^2  (verzerrt)
#   E(S^2)      ≈ sigma^2            (erwartungstreu)
# und vergleiche die MSE.

set.seed(2025)
n  <- 10
m  <- 5000
sigma2_wahr <- 4

sig2_vec <- numeric(m)
S2_vec   <- numeric(m)

for (i in seq_len(m)) {
  x <- rnorm(n, mean = 0, sd = sqrt(sigma2_wahr))
  sig2_vec[i] <- mean((x - mean(x))^2)   # empirische Varianz
  S2_vec[i]   <- var(x)                  # korrigierte Varianz
}

cat("Mittel sig2_hat =", round(mean(sig2_vec), 3),
    "  (erwartet:", round((n - 1)/n * sigma2_wahr, 3), ")\n")
cat("Mittel S^2      =", round(mean(S2_vec),   3),
    "  (erwartet:", sigma2_wahr, ")\n")

mse_sig2 <- mean((sig2_vec - sigma2_wahr)^2)
mse_S2   <- mean((S2_vec   - sigma2_wahr)^2)
cat("MSE(sig2_hat) =", round(mse_sig2, 3),
    " | MSE(S^2) =",   round(mse_S2,   3), "\n")
# Der MLE ist verzerrt, hat aber häufig den kleineren MSE.


# -----------------------------------------------------------------------------
# Aufgabe 5 – Konsistenz des Mittelwertes
# -----------------------------------------------------------------------------
# Plotte den MSE des Mittelwertes als Funktion des Stichprobenumfangs n
# bei Ziehung aus N(5, 4). Erwartet: MSE = sigma^2 / n -> 0.

set.seed(7)
sigma2 <- 4
ns <- seq(10, 1000, by = 10)
mse <- sapply(ns, function(n) {
  xq <- replicate(500, mean(rnorm(n, mean = 5, sd = sqrt(sigma2))))
  mean((xq - 5)^2)
})

plot(ns, mse, type = "l", col = "darkgreen", lwd = 2,
     xlab = "Stichprobenumfang n", ylab = "MSE des Mittelwertes",
     main = "Konsistenz: MSE(X_bar) -> 0")
lines(ns, sigma2 / ns, col = "red", lty = 2, lwd = 2)
legend("topright", legend = c("simulated MSE", "sigma^2 / n"),
       col = c("darkgreen", "red"), lty = c(1, 2), lwd = 2)

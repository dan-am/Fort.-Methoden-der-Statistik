# =============================================================================
# Lösung zu Übung 6 (Kapitel 3.5: Anforderungen an Schätzfunktionen)
# Quelle: Aufgabe „Übung 6: Induktive Statistik“ und Seite „Lösung Übung 6“ im LMS
# Neu gefasst am 21.11.2026, im LMS seit 22.11.2026; Änderungen gegenüber dem LMS
# stehen in lms/AENDERUNGEN.md
# Benötigt nur Base R.
# =============================================================================

# -----------------------------------------------------------------------------
# Aufgabe 1: Erwartungstreue und mittlerer quadratischer Fehler
# -----------------------------------------------------------------------------
# 10.000 Stichproben vom Umfang n = 10 aus N(0, 4). In jeder Stichprobe werden
# die empirische Varianz (Teiler n) und die korrigierte Varianz S^2 (Teiler n - 1)
# berechnet.
set.seed(2026)
n <- 10
m <- 10000
sigma2 <- 4

emp_var  <- numeric(m)
korr_var <- numeric(m)
for (i in seq_len(m)) {
  x <- rnorm(n, mean = 0, sd = sqrt(sigma2))
  emp_var[i]  <- mean((x - mean(x))^2)   # Teiler n
  korr_var[i] <- var(x)                  # Teiler n - 1
}

# b) Mittelwerte der 10.000 Schätzungen gegen den wahren Wert 4
c(empirisch = mean(emp_var), korrigiert = mean(korr_var))
# Die korrigierte Varianz trifft im Mittel 4: Sie ist erwartungstreu.
# Die empirische Varianz liegt im Mittel bei (n - 1) / n * 4 = 3,6: Sie unterschätzt
# sigma^2 um den Faktor (n - 1) / n = 0,9.

# c) Mittlerer quadratischer Fehler: MSE = Varianz + Verzerrung^2
c(MSE_empirisch = mean((emp_var - sigma2)^2), MSE_korrigiert = mean((korr_var - sigma2)^2))
# Theoretisch bei Normalverteilung: MSE(S^2) = 2 sigma^4 / (n - 1) = 3,56 und
# MSE der empirischen Varianz = (2n - 1) sigma^4 / n^2 = 3,04.
# Die empirische Varianz ist verzerrt, streut aber weniger und hat den kleineren MSE.
# Erwartungstreue allein entscheidet also nicht, welcher Schätzer besser ist.

hist(korr_var, breaks = 40, freq = FALSE, col = "grey85", border = "white",
     xlab = "Schätzwert", main = "Korrigierte Varianz in 10.000 Stichproben")
abline(v = sigma2, col = "red", lwd = 2)
abline(v = mean(emp_var), col = "blue", lwd = 2, lty = 2)
legend("topright", c("wahrer Wert 4", "Mittel der empirischen Varianz"),
       col = c("red", "blue"), lty = c(1, 2), lwd = 2, bty = "n")


# -----------------------------------------------------------------------------
# Aufgabe 2: Konsistenz des Mittelwerts
# -----------------------------------------------------------------------------
# Je Stichprobenumfang 2.000 Stichproben aus N(5, 4), MSE des Stichprobenmittels
set.seed(2026)
mu <- 5
umfaenge <- c(5, 10, 20, 50, 100, 200, 500)
mse_mittel <- sapply(umfaenge, function(n) {
  mittel <- replicate(2000, mean(rnorm(n, mean = mu, sd = sqrt(sigma2))))
  mean((mittel - mu)^2)
})
round(cbind(n = umfaenge, MSE = mse_mittel, theoretisch = sigma2 / umfaenge), 4)

plot(umfaenge, mse_mittel, type = "b", pch = 19, log = "x",
     xlab = "Stichprobenumfang n (logarithmische Achse)", ylab = "MSE des Mittelwerts",
     main = "Konsistenz des Stichprobenmittels")
lines(umfaenge, sigma2 / umfaenge, col = "red", lty = 2, lwd = 2)
legend("topright", c("Simulation", "theoretisch: sigma^2 / n"),
       col = c("black", "red"), lty = c(1, 2), pch = c(19, NA), bty = "n")
# Der MSE fällt mit wachsendem n gegen 0, so wie sigma^2 / n: Das Stichprobenmittel
# ist erwartungstreu und konsistent.

# =============================================================================
# Übung Kapitel 5 – Zusatz: Einstichprobentests (weitere Verfahren)
# Themen: Binomialtest / Anteilstest, Wilcoxon-Vorzeichen-Rang-Test (Median),
#         Chi-Quadrat-Test für eine Varianz, Power-Berechnung,
#         Fehler 1. und 2. Art, Stichprobenumfangsplanung
# Datensätze: mtcars, sleep (R-base), eigene Werte
# =============================================================================

library(ggplot2)


# -----------------------------------------------------------------------------
# Aufgabe 1 – Exakter Binomialtest für einen Anteil
# -----------------------------------------------------------------------------
# Eine Marketingstudie behauptet, dass 30 % der Kunden ein Produkt kaufen.
# In einer Stichprobe von n = 50 Personen kaufen es k = 20.
#
# H0: p  = 0.30
# H1: p != 0.30
#
# a) Führe einen exakten Binomialtest durch.
# b) Vergleiche mit der Normalapproximation (prop.test).
# c) Bestimme das exakte 95%-Konfidenzintervall für p.

n <- 50
k <- 20
p0 <- 0.30

# a) exakt
bin <- binom.test(k, n, p = p0, alternative = "two.sided", conf.level = 0.95)
print(bin)

# b) Normalapproximation
pp  <- prop.test(k, n, p = p0, alternative = "two.sided", correct = FALSE)
print(pp)

# c)
cat("Exaktes 95%-KI für p:", round(bin$conf.int, 4), "\n")


# -----------------------------------------------------------------------------
# Aufgabe 2 – Wilcoxon-Vorzeichen-Rang-Test (Test auf Median)
# -----------------------------------------------------------------------------
# Wenn die Normalverteilungsannahme verletzt ist, kann der Median statt des
# Mittelwertes getestet werden.
#
# Verwende mtcars$mpg. Hypothese: der Median liegt bei 20.
# H0: med  = 20
# H1: med != 20

x <- mtcars$mpg
wt <- wilcox.test(x, mu = 20, alternative = "two.sided", conf.int = TRUE)
print(wt)

# Vergleiche kurz mit dem t-Test (auf Mittelwert)
t.test(x, mu = 20)$p.value


# -----------------------------------------------------------------------------
# Aufgabe 3 – Chi-Quadrat-Test für eine Varianz
# -----------------------------------------------------------------------------
# Bei normalverteilten Daten kann man die Hypothese sigma^2 = sigma_0^2 testen.
# Teststatistik:  T = (n-1) * S^2 / sigma_0^2  ~ chi^2_(n-1)
#
# Behauptung: die Varianz von mtcars$wt ist sigma_0^2 = 1.
# H0: sigma^2  = 1
# H1: sigma^2 != 1   (zweiseitig, alpha = 0.05)

x <- mtcars$wt
n <- length(x)
S2 <- var(x)
sigma2_0 <- 1
alpha <- 0.05

T_stat <- (n - 1) * S2 / sigma2_0
chi_u  <- qchisq(    alpha/2, df = n - 1)
chi_o  <- qchisq(1 - alpha/2, df = n - 1)

# zweiseitiger p-Wert
p_value <- 2 * min(pchisq(T_stat, df = n - 1),
                   1 - pchisq(T_stat, df = n - 1))

cat("S^2 =", round(S2, 3),
    "  T =", round(T_stat, 3),
    "  krit:", round(c(chi_u, chi_o), 3),
    "  p =", round(p_value, 4), "\n")

if (T_stat < chi_u || T_stat > chi_o) {
  cat("-> H0 wird abgelehnt\n")
} else {
  cat("-> H0 wird nicht abgelehnt\n")
}


# -----------------------------------------------------------------------------
# Aufgabe 4 – Power und benötigter Stichprobenumfang
# -----------------------------------------------------------------------------
# power.t.test() berechnet den Zusammenhang zwischen
#   n (Stichprobenumfang), delta (echter Effekt), sd, sig.level und power.
# Es muss genau einer dieser Parameter NULL sein.
#
# a) Welcher Stichprobenumfang ist nötig, um einen Effekt von delta = 0.5
#    bei sd = 1 mit Power 80 % bei alpha = 0.05 (zweiseitig) zu entdecken?
# b) Welche Power hat ein Test mit n = 30, delta = 0.4, sd = 1, alpha = 0.05?

# a)
res_a <- power.t.test(power = 0.80, delta = 0.5, sd = 1,
                      sig.level = 0.05, type = "one.sample",
                      alternative = "two.sided")
print(res_a)

# b)
res_b <- power.t.test(n = 30, delta = 0.4, sd = 1,
                      sig.level = 0.05, type = "one.sample",
                      alternative = "two.sided")
cat("Power bei n = 30 und delta = 0.4:", round(res_b$power, 3), "\n")


# -----------------------------------------------------------------------------
# Aufgabe 5 – Fehler 1. und 2. Art per Simulation
# -----------------------------------------------------------------------------
# Definitionen:
#   alpha  = P(H0 ablehnen | H0 wahr)   -> Fehler 1. Art
#   beta   = P(H0 nicht ablehnen | H1)  -> Fehler 2. Art
#   1-beta = Power
#
# Simuliere für zwei Szenarien:
#   (i)  Daten kommen aus N(0, 1) (H0: mu = 0 wahr)
#   (ii) Daten kommen aus N(0.5, 1) (H1: mu = 0.5 wahr)

set.seed(2024)
n  <- 30
m  <- 5000

# (i) H0 wahr -> Anteil falsche Ablehnungen sollte ~ alpha sein
ablehnen_h0 <- replicate(m, {
  x <- rnorm(n, mean = 0, sd = 1)
  t.test(x, mu = 0)$p.value < 0.05
})
cat("Empirischer alpha-Fehler:", round(mean(ablehnen_h0), 3),
    "(Soll: ~0.05)\n")

# (ii) H1 wahr -> Anteil korrekter Ablehnungen = empirische Power
ablehnen_h1 <- replicate(m, {
  x <- rnorm(n, mean = 0.5, sd = 1)
  t.test(x, mu = 0)$p.value < 0.05
})
cat("Empirische Power      :", round(mean(ablehnen_h1), 3), "\n")
cat("Empirischer beta-Fehler:", round(1 - mean(ablehnen_h1), 3), "\n")


# -----------------------------------------------------------------------------
# Aufgabe 6 – Visualisierung: alpha vs. beta
# -----------------------------------------------------------------------------
# Stelle die Verteilungen unter H0 und H1 grafisch dar und schraffiere
# die Bereiche für alpha (Fehler 1. Art) und beta (Fehler 2. Art).

mu0 <- 0
mu1 <- 0.7
sigma <- 1
n     <- 25
se    <- sigma / sqrt(n)
alpha <- 0.05
krit  <- qnorm(1 - alpha, mean = mu0, sd = se)

x <- seq(-1.5, 2.0, length.out = 500)
y0 <- dnorm(x, mean = mu0, sd = se)
y1 <- dnorm(x, mean = mu1, sd = se)

plot(x, y0, type = "l", lwd = 2, col = "blue",
     ylim = c(0, max(y0, y1) * 1.05),
     xlab = "X_bar", ylab = "Dichte",
     main = "Fehler 1. und 2. Art")
lines(x, y1, lwd = 2, col = "red")
abline(v = krit, lty = 2)
legend("topright",
       c("H0: mu=0", "H1: mu=0.7", "krit. Wert"),
       col = c("blue", "red", "black"), lty = c(1, 1, 2), lwd = 2)
# Rechts vom kritischen Wert unter H0 = alpha;
# Links  vom kritischen Wert unter H1 = beta.

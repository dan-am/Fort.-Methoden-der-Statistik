# =============================================================================
# R-Beispiele 21 bis 24 zu Kapitel 6: Zweistichprobentests
# Quelle: Textseite „R Beispiel 21-24“ im LMS
# Überarbeitet am 21.11.2026; Änderungen gegenüber dem LMS stehen in lms/AENDERUNGEN.md
# Benötigt nur Base R.
# =============================================================================


# -----------------------------------------------------------------------------
# Beispiel 21 (Kap. 6.1): t-Test für unabhängige Stichproben, gleiche Varianzen
# -----------------------------------------------------------------------------
set.seed(123)
stichprobe1 <- rnorm(20, mean = 5, sd = 2)
stichprobe2 <- rnorm(25, mean = 7, sd = 2)

# H0: mu1 = mu2 gegen H1: mu1 != mu2
t.test(stichprobe1, stichprobe2, var.equal = TRUE)


# -----------------------------------------------------------------------------
# Beispiel 22 (Kap. 6.1): Welch-Test für unabhängige Stichproben, ungleiche Varianzen
# -----------------------------------------------------------------------------
set.seed(123)
stichprobe1 <- rnorm(20, mean = 5, sd = 2)
stichprobe2 <- rnorm(25, mean = 7, sd = 4.5)

t.test(stichprobe1, stichprobe2, var.equal = FALSE)


# -----------------------------------------------------------------------------
# Beispiel 23 (Kap. 6.1): F-Test auf Gleichheit zweier Varianzen
# -----------------------------------------------------------------------------
# Der F-Test setzt unabhängige, normalverteilte Stichproben voraus. Er wird daher
# auf die beiden unabhängigen Stichproben aus Beispiel 22 angewendet, nicht auf
# die gepaarten Cholesterinwerte aus Beispiel 24.
# H0: sigma1^2 = sigma2^2 gegen H1: sigma1^2 != sigma2^2
var.test(stichprobe1, stichprobe2, ratio = 1, conf.level = 0.95)


# -----------------------------------------------------------------------------
# Beispiel 24 (Kap. 6.2): t-Test für verbundene Stichproben
# -----------------------------------------------------------------------------
# Cholesterinwerte derselben 10 Personen zu zwei Zeitpunkten (Messung 1 und 2).
# Gepaart, weil jede Person zweimal gemessen wurde. Mit d = Messung 1 - Messung 2:
# H0: mu_d <= 0 gegen H1: mu_d > 0 (Messung 2 liegt im Mittel niedriger).
Cholesterin.1 <- c(226, 255, 247, 211, 289, 190, 231, 276, 241, 210)
Cholesterin.2 <- c(220, 244, 243, 211, 299, 170, 210, 276, 252, 189)

t.test(x = Cholesterin.1, y = Cholesterin.2, alternative = "greater",
       mu = 0, paired = TRUE, conf.level = 0.90)

# Gleichwertig: Einstichproben-t-Test auf die Differenzen
t.test(Cholesterin.1 - Cholesterin.2, alternative = "greater", mu = 0, conf.level = 0.90)

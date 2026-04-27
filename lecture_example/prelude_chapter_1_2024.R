# Inhalte zum 1. Kapitel als Hinführung zum Thema
# 1. Wahrscheinlichkeitsrechnung --------

# 1.1 Wahrscheinlichkeit von Ereignissen ---- 

# Beispiel: Zweimaliger Münzwurf

# Grundgesamtheit Ω
omega <- expand.grid(c("K", "Z"), c("K", "Z"))

# Ereignis A: Mindestens einmal Kopf
A <- omega[apply(omega, 1, function(x){"K"  %in% x} ), ] # apply(omega, 1, function(x) "K" %in% x) prüft, ob "K" in der Zeile x enthalten ist
print(A)

# Wahrscheinlichkeit von A
P_A <- nrow(A) / nrow(omega) # nrow() gibt die Anzahl der Zeilen zurück
P_A

# 1.2 Axiomatik der Wahrscheinlichkeitstheorie -----
# Beispiel: Laplace-Wahrscheinlichkeit eines fairen Würfels
# Grundgesamtheit
omega <- 1:6

# Ereignis A: Würfeln einer geraden Zahl
A <- c(2, 4, 6)

# Wahrscheinlichkeit P(A)
P_A <- length(A) / length(omega) # length() gibt die Anzahl der Elemente zurück
P_A

# 1.3 Multiplikationssatz für bedingte Wahrscheinlichkeiten -----
# Beispiel: Job und Einkommen

P_B <- 0.1  # Wahrscheinlichkeit, Data Scientist zu sein
P_A_given_B <- 0.8  # Bedingte Wahrscheinlichkeit
P_A_and_B <- P_B * P_A_given_B
P_A_and_B

# 1.4 Satz von der totalen Wahrscheinlichkeit ----
#Beispiel: Jobarten und Einkommen

P_A_given_DS <- 0.6
P_DS <- 0.1
P_A_given_JA <- 0.8
P_JA <- 0.25
P_A_given_AJ <- 0.65
P_AJ <- 0.65

P_A <- P_A_given_DS * P_DS + P_A_given_JA * P_JA + P_A_given_AJ * P_AJ
P_A

# 1.5 Satz von Bayes ----
# Beispiel: Krankheit und Test

P_D_given_K <- 0.95
P_K <- 0.03
P_D_given_G <- 0.10
P_G <- 1 - P_K

P_D <- P_D_given_K * P_K + P_D_given_G * P_G
P_K_given_D <- (P_D_given_K * P_K) / P_D
P_K_given_D

# 1.9 Diskrete Zufallsvariablen ----
# 1.9.1 Binomialverteilung ----
# Beispiel: Erfolgswahrscheinlichkeit eines Medikaments

n <- 50
p <- 0.9
k <- 48

P_k <- dbinom(k, size = n, prob = p)
P_k

# 1.9.2 Hypergeometrische Verteilung ----
# Beispiel: Defekte Bauteile

N <- 40  # Gesamtzahl Bauteile
r <- 5   # Defekte Bauteile
n <- 4   # Auswahlgröße
k <- 2   # Anzahl defekter in der Auswahl

P_k <- dhyper(k, r, N - r, n)
P_k

# 1.9.3 Poissonverteilung ----
# Beispiel: Anzahl der Großunfälle 

lambda <- 3
k <- 2

P_k <- dpois(k, lambda)
P_k

# 1.10 Stetige Zufallsvariablen ----
# 1.10.1 Normalverteilung ----
# Beispiel: Normalverteilung plotten

mu <- 0
sigma <- 1
x <- seq(-4, 4, length = 100)

y <- dnorm(x, mean = mu, sd = sigma)
plot(x, y, type = "l", main = "Normalverteilung", xlab = "x", ylab = "Dichte")

# 1.10.2 Exponentialverteilung ----
# Beispiel: Lebensdauer eines Fernsehgeräts

lambda <- 0.08
x <- 10

P_X_greater <- exp(-lambda * x)
P_X_greater
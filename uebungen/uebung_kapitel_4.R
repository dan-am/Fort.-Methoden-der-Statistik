# =============================================================================
# Übung Kapitel 4: Konfidenzintervalle
# Themen: KI für mu (sigma bekannt / unbekannt), KI für sigma^2,
#         asymptotisches KI für p, Überdeckungswahrscheinlichkeit,
#         Stichprobenumfang
# Datensätze: sleep, mtcars (R-base)
# =============================================================================


# -----------------------------------------------------------------------------
# Aufgabe 1 – Exaktes KI für mu bei BEKANNTER Varianz
# -----------------------------------------------------------------------------
# Ein Hersteller geht davon aus, dass die Lebensdauer einer Glühbirne
# normalverteilt ist mit sigma = 50 Stunden. Aus n = 25 Birnen wurde der
# Mittelwert x_bar = 1020 berechnet. Berechne das 95 %-Konfidenzintervall
# für mu.
#
# Formel:  [ X_bar - z_{1-alpha/2} * sigma/sqrt(n) ,
#            X_bar + z_{1-alpha/2} * sigma/sqrt(n) ]

x_bar <- 1020
sigma <- 50
n     <- 25
alpha <- 0.05

z <- qnorm(1 - alpha/2)
KI_mu <- c(x_bar - z * sigma / sqrt(n),
           x_bar + z * sigma / sqrt(n))
cat("95% KI für mu (sigma bekannt):", round(KI_mu, 2), "\n")


# -----------------------------------------------------------------------------
# Aufgabe 2 – Exaktes KI für mu bei UNBEKANNTER Varianz (t-Verteilung)
# -----------------------------------------------------------------------------
# Verwende mtcars$mpg. Berechne ein 95 %-KI für den mittleren Verbrauch.
#
# Formel:  [ X_bar - t_{n-1; 1-alpha/2} * S/sqrt(n) ,
#            X_bar + t_{n-1; 1-alpha/2} * S/sqrt(n) ]
#
# Vergleiche das Ergebnis mit t.test().

x <- mtcars$mpg
n <- length(x)
x_bar <- mean(x)
S     <- sd(x)
alpha <- 0.05

t <- qt(1 - alpha/2, df = n - 1)
KI_t <- c(x_bar - t * S / sqrt(n),
          x_bar + t * S / sqrt(n))
cat("95% KI für mu (sigma unbekannt):", round(KI_t, 3), "\n")

# Vergleich mit der eingebauten Funktion
t.test(x, conf.level = 0.95)$conf.int


# -----------------------------------------------------------------------------
# Aufgabe 3 – KI für sigma^2 (Chi-Quadrat-Verteilung)
# -----------------------------------------------------------------------------
# Wieder mit mtcars$mpg: berechne ein 95 %-KI für die Varianz sigma^2.
#
# Formel:  [ (n-1) S^2 / chi2_{n-1; 1-alpha/2} ,
#            (n-1) S^2 / chi2_{n-1; alpha/2}   ]

S2 <- var(x)
chi_o <- qchisq(1 - alpha/2, df = n - 1)
chi_u <- qchisq(    alpha/2, df = n - 1)

KI_var <- c((n - 1) * S2 / chi_o,
            (n - 1) * S2 / chi_u)
cat("95% KI für sigma^2:", round(KI_var, 3), "\n")
cat("95% KI für sigma  :", round(sqrt(KI_var), 3), "\n")


# -----------------------------------------------------------------------------
# Aufgabe 4 – Asymptotisches KI für einen Anteil p
# -----------------------------------------------------------------------------
# In einer Umfrage von n = 200 Personen geben 78 an, ein Produkt zu kaufen.
# Bestimme ein 95 %-KI für den wahren Anteil p mittels Normal-Approximation.
#
# Formel:  p_hat +/- z_{1-alpha/2} * sqrt( p_hat (1 - p_hat) / n )

n     <- 200
k     <- 78
p_hat <- k / n
alpha <- 0.05

z <- qnorm(1 - alpha/2)
se <- sqrt(p_hat * (1 - p_hat) / n)
KI_p <- c(p_hat - z * se, p_hat + z * se)
cat("p_hat =", p_hat, "\n")
cat("95% KI für p:", round(KI_p, 4), "\n")

# Vergleich:
prop.test(k, n, conf.level = 0.95, correct = FALSE)$conf.int


# -----------------------------------------------------------------------------
# Aufgabe 5 – Überdeckungswahrscheinlichkeit (Simulationsstudie)
# -----------------------------------------------------------------------------
# Was bedeutet "Konfidenzniveau 95 %"?
#   -> Bei vielen wiederholten Stichproben überdeckt das KI in 95 % der Fälle
#      den wahren Parameter.
#
# Simuliere m = 1000 Stichproben vom Umfang n = 30 aus N(mu = 5, sigma = 2).
# Berechne in jeder Stichprobe das 95 %-KI für mu (sigma bekannt) und
# zähle, wie oft mu = 5 im Intervall liegt.

set.seed(123)
mu_wahr <- 5
sigma   <- 2
n  <- 30
m  <- 1000
alpha <- 0.05
z  <- qnorm(1 - alpha/2)

ueberdeckt <- replicate(m, {
  x  <- rnorm(n, mean = mu_wahr, sd = sigma)
  xq <- mean(x)
  u  <- xq - z * sigma / sqrt(n)
  o  <- xq + z * sigma / sqrt(n)
  (u <= mu_wahr) & (mu_wahr <= o)
})

cat("Empirische Überdeckungswahrscheinlichkeit:",
    round(mean(ueberdeckt), 3), "(erwartet ≈ 0.95)\n")


# -----------------------------------------------------------------------------
# Aufgabe 6 – Notwendiger Stichprobenumfang
# -----------------------------------------------------------------------------
# Wie groß muss n sein, damit das 95 %-KI für mu (sigma = 2 bekannt) eine
# halbe Länge (Schätzfehler) von höchstens epsilon = 0.5 hat?
#
# Formel:  n >= ( z_{1-alpha/2} * sigma / epsilon )^2

sigma   <- 2
epsilon <- 0.5
alpha   <- 0.05
z <- qnorm(1 - alpha/2)

n_min <- ceiling((z * sigma / epsilon)^2)
cat("Mindeststichprobenumfang n:", n_min, "\n")

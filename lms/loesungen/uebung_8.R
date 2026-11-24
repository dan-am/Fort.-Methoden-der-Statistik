# =============================================================================
# Lösung zu Übung 8 (Kapitel 5: Statistische Tests)
# Quelle: Aufgabe „5. Statistischer Test - Übung 8“ und Seite „Lösung Übung 8“ im LMS
# Überarbeitet am 21.11.2026; Änderungen gegenüber dem LMS stehen in lms/AENDERUNGEN.md
# Daten: Autofahrer.csv im Arbeitsverzeichnis oder in data/input/
# Benötigt nur Base R.
# =============================================================================
#
# Aufgabe: Reaktionszeiten telefonierender Autofahrer in Millisekunden,
# Annahme: normalverteilt.
#   a) Importiere den Datensatz mit read.csv.
#   b) Bestimme Stichprobenumfang, Mittelwert und Stichprobenstandardabweichung.
#   c) Überprüfe zum Signifikanzniveau 5 %, dass die Reaktionszeit 730 ms
#      nicht überschritten wird.
#   d) Bei welchem Niveau alpha wird die Hypothese aus c) noch abgelehnt? (p-Wert)

# a) Die Datei liegt im Arbeitsverzeichnis (Download aus dem LMS) oder im
#    Projektordner unter data/input/. Sie hat keine Kopfzeile, die Werte stehen in Spalte V1.
datei <- if (file.exists("Autofahrer.csv")) "Autofahrer.csv" else "data/input/Autofahrer.csv"
RZ <- read.csv(file = datei, header = FALSE)
x <- RZ$V1

# b) Kennzahlen
n  <- length(x)
xq <- mean(x)
s  <- sd(x)
cat("n =", n, "| Mittelwert =", round(xq, 2), "ms | s =", round(s, 2), "ms\n")
summary(x)

# c) Gezeigt werden soll mu < 730, also
#    H0: mu >= 730 gegen H1: mu < 730 (linksseitiger t-Test), alpha = 0,05
mu0   <- 730
alpha <- 0.05

# Variante 1: von Hand
t_stat <- (xq - mu0) / (s / sqrt(n))   # Teststatistik
t_krit <- qt(alpha, df = n - 1)         # kritischer Wert (linksseitig)
p_wert <- pt(t_stat, df = n - 1)        # p-Wert (linksseitig)
cat("t =", round(t_stat, 3), "| kritischer Wert =", round(t_krit, 3),
    "| p-Wert =", signif(p_wert, 3), "\n")
# t < kritischer Wert: H0 wird abgelehnt. Die mittlere Reaktionszeit liegt
# signifikant unter 730 ms.

# Variante 2: mit t.test(); das Konfidenzniveau ist 1 - alpha
t.test(x, alternative = "less", mu = mu0, conf.level = 1 - alpha)

# d) Die Hypothese wird bei jedem Niveau alpha abgelehnt, das größer als der
#    p-Wert ist; hier also bei jedem alpha über 8,4 * 10^(-7).

# Zusatz: Güte des Tests, wenn die wahre mittlere Reaktionszeit 726 ms beträgt
# (Normalapproximation, sigma durch s geschätzt)
mu1 <- 726
pnorm(qnorm(alpha) + (mu0 - mu1) / s * sqrt(n))

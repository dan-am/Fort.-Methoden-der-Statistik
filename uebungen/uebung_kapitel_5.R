# =============================================================================
# Übung Kapitel 5: Einstichproben-Hypothesentests
# Themen: Hypothesenformulierung, t-Test (einseitig/zweiseitig), kritische
#         Werte, p-Wert, Testvoraussetzungen (Normalverteilung, QQ-Plot,
#         Shapiro-Wilk, Kolmogorov-Smirnov)
# Datensätze: mtcars, sleep (R-base)
# =============================================================================


# falls noch nicht installiert
# install.packages("ggplot2")

library(ggplot2)


# -----------------------------------------------------------------------------
# Aufgabe 1 – Einseitiger t-Test (mu > mu_0)
# -----------------------------------------------------------------------------
# Behauptung: Der mittlere Verbrauch (mtcars$mpg) liegt ÜBER 18 mpg.
#
# H0:  mu <= 18
# H1:  mu  > 18   (einseitig nach oben)
# Signifikanzniveau alpha = 0.05.
#
# a) Berechne Mittelwert, Standardabweichung und Stichprobengröße.
# b) Führe einen einseitigen t-Test durch.
# c) Bestimme den kritischen t-Wert und vergleiche mit der Teststatistik.
# d) Treffe eine Testentscheidung.

data(mtcars)
x <- mtcars$mpg
n <- length(x)
mu0   <- 18
alpha <- 0.05

# a)
xq <- mean(x)
s  <- sd(x)
cat("n =", n, ", Mittelwert =", round(xq, 3),
    ", sd =", round(s, 3), "\n")

# b) t-Test einseitig
t_test <- t.test(x, mu = mu0, alternative = "greater", conf.level = 1 - alpha)
print(t_test)

# c) kritischer Wert + Teststatistik per Hand
t_stat  <- (xq - mu0) / (s / sqrt(n))
t_krit  <- qt(1 - alpha, df = n - 1)
cat("Teststatistik t =", round(t_stat, 3),
    " | krit. Wert t_(1-alpha) =", round(t_krit, 3), "\n")

# d) Entscheidung
if (t_stat > t_krit) {
  cat("-> H0 wird abgelehnt: mittlerer mpg ist signifikant > 18\n")
} else {
  cat("-> H0 wird nicht abgelehnt\n")
}


# -----------------------------------------------------------------------------
# Aufgabe 2 – Zweiseitiger t-Test (mu = mu_0)
# -----------------------------------------------------------------------------
# Frage: Beträgt das mittlere Gewicht (mtcars$wt, in 1000 lbs) genau 3.2?
#
# H0:  mu  = 3.2
# H1:  mu != 3.2   (zweiseitig)
#
# a) Berechne den t-Test mit alpha = 0.05.
# b) Lies das 95 %-Konfidenzintervall aus dem t.test()-Objekt aus.
# c) Argumentiere, warum KI und Test zur selben Entscheidung führen.

mu0 <- 3.2
res <- t.test(mtcars$wt, mu = mu0, alternative = "two.sided", conf.level = 0.95)
print(res)
cat("p-Wert =", round(res$p.value, 4),
    " | 95%-KI:", round(res$conf.int, 3), "\n")
# Liegt mu0 im KI -> H0 nicht ablehnen, sonst ablehnen.


# -----------------------------------------------------------------------------
# Aufgabe 3 – Voraussetzungen prüfen: Normalverteilung
# -----------------------------------------------------------------------------
# Vor jedem t-Test sollte man die Normalverteilungsannahme prüfen.
# Untersuche mtcars$mpg mit:
#   a) QQ-Plot (qqnorm + qqline)
#   b) Shapiro-Wilk-Test     (geeignet bei n <= 5000)
#   c) Kolmogorov-Smirnov-Test (gegen N(mean, sd) der Daten)

x <- mtcars$mpg

# a) QQ-Plot
qqnorm(x, main = "QQ-Plot mtcars$mpg", pch = 20)
qqline(x, col = "red", lwd = 2)

# b) Shapiro-Wilk
# H0: Daten sind normalverteilt
# H1: Daten sind nicht normalverteilt
sw <- shapiro.test(x)
cat("Shapiro-Wilk: W =", round(sw$statistic, 4),
    ", p =", round(sw$p.value, 4), "\n")

# c) Kolmogorov-Smirnov
# H0: Daten sind normalverteilt
# H1: Daten sind nicht normalverteilt
ks <- ks.test(x, "pnorm", mean = mean(x), sd = sd(x))
cat("KS-Test:      D =", round(ks$statistic, 4),
    ", p =", round(ks$p.value, 4), "\n")
# p > 0.05 -> Normalverteilung kann nicht verworfen werden.
# Achtung: Mittelwert und sd werden hier aus denselben Daten geschätzt. Dann ist
# der p-Wert des klassischen KS-Tests zu groß; korrekt ist der Lilliefors-Test:
# nortest::lillie.test(x). Die Warnung zu Bindungen entsteht durch doppelte Werte.


# -----------------------------------------------------------------------------
# Aufgabe 4 – p-Wert und kritischer Bereich grafisch
# -----------------------------------------------------------------------------
# Visualisiere den Annahme- und Ablehnungsbereich für einen zweiseitigen
# t-Test mit df = n - 1 Freiheitsgraden. Trage die Teststatistik aus
# Aufgabe 2 (mtcars$wt, mu0 = 3.2) ein und markiere den p-Wert.

n <- 32
df <- n - 1
alpha <- 0.05
krit <- qt(1 - alpha/2, df)
t_beob <- unname(res$statistic)          # Teststatistik aus Aufgabe 2

curve(dt(x, df = df), from = -4, to = 4, lwd = 2,
      ylab = "Dichte t-Verteilung", xlab = "t",
      main = paste("t-Verteilung, df =", df))
# Ablehnungsbereich: Flächen unter der Dichte jenseits der kritischen Werte (zusammen alpha)
links  <- seq(-4, -krit, length.out = 100)
rechts <- seq(krit, 4, length.out = 100)
polygon(c(-4, links, -krit), c(0, dt(links, df), 0), col = rgb(1, 0, 0, 0.4), border = NA)
polygon(c(krit, rechts, 4), c(0, dt(rechts, df), 0), col = rgb(1, 0, 0, 0.4), border = NA)
abline(v = c(-krit, krit), col = "red", lty = 2, lwd = 2)
abline(v = t_beob, col = "blue", lwd = 2)
legend("topright",
       legend = c(paste("krit. Werte = +/-", round(krit, 3)),
                  paste("t aus Aufgabe 2 =", round(t_beob, 3)),
                  paste("p-Wert =", round(res$p.value, 4))),
       col = c("red", "blue", NA), lty = c(2, 1, NA), lwd = 2, bty = "n")
# Der p-Wert ist die Fläche jenseits von |t_beob| auf beiden Seiten.


# -----------------------------------------------------------------------------
# Aufgabe 5 – Eingebauter sleep-Datensatz: gepaarter t-Test
# -----------------------------------------------------------------------------
# Der Datensatz sleep enthält die Schlafverlängerung extra für 10 Probanden
# unter zwei Mitteln (group 1 vs. group 2). Untersuche, ob es einen
# signifikanten Unterschied gibt.
#
# H0: mu_d  = 0  (kein Unterschied)
# H1: mu_d != 0
#
# Achtung: sleep ist gepaart -> paired = TRUE.

data(sleep)
g1 <- sleep$extra[sleep$group == 1]
g2 <- sleep$extra[sleep$group == 2]

paar_test <- t.test(g2, g1, paired = TRUE, alternative = "two.sided")
print(paar_test)

# Visualisierung
boxplot(extra ~ group, data = sleep, col = c("skyblue", "salmon"),
        main = "Schlafverlängerung pro Gruppe", ylab = "extra")



# Der gepaarte t-Test ist ein Einstichproben-t-Test auf die Differenzen
differenzen <- g2 - g1
mean(differenzen)
t.test(differenzen, mu = 0)   # gleiches Ergebnis wie paar_test

# Histogramm der gepaarten Differenzen
hist(differenzen,
     breaks = 8,
     col = "lightgray",
     border = "white",
     main = "Histogramm der gepaarten Differenzen",
     xlab = "Differenz: Gruppe 2 - Gruppe 1")

# Null-Linie: kein Unterschied
abline(v = 0, col = "black", lwd = 2, lty = 2)

# Mittelwert der Differenzen
abline(v = mean(differenzen), col = "red", lwd = 3)

# 95-%-Konfidenzintervall für den Mittelwert der Differenzen. Enthält es die 0
# nicht, wird H0 zum Niveau 5 % abgelehnt (gleiche Entscheidung wie der Test).
ki <- paar_test$conf.int
abline(v = ki, col = "blue", lwd = 2, lty = 3)

legend("topright",
       legend = c("kein Unterschied",
                  "Mittelwert der Differenzen",
                  "95-%-KI für den Mittelwert"),
       col = c("black", "red", "blue"),
       lty = c(2, 1, 3),
       lwd = c(2, 3, 2))

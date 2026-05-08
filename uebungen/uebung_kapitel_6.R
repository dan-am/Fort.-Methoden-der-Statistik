# =============================================================================
# Übung Kapitel 6: Zweistichprobentests
# Themen: Mittelwertvergleich zweier Gruppen, F-Test (Varianzhomogenität),
#         t-Test (gleiche/ungleiche Varianzen, Welch-Test),
#         Mann-Whitney-U-Test (nichtparametrisch)
# Datensätze: mtcars, ChickWeight, iris (R-base)
# =============================================================================

# install.packages("ggplot2")
library(ggplot2)


# -----------------------------------------------------------------------------
# Aufgabe 1 – Vergleich Automatik- vs. Schaltgetriebe (mtcars)
# -----------------------------------------------------------------------------
# mtcars$am: 0 = Automatik, 1 = Schaltgetriebe
# Frage: Unterscheidet sich der Verbrauch (mpg) zwischen den beiden Gruppen?
#
# Vorgehen:
# a) Boxplot zur ersten Sichtprüfung.
# b) Normalverteilung pro Gruppe (Shapiro-Wilk).
# c) Varianzhomogenität (var.test = F-Test).
# d) t-Test (var.equal abhängig von c) ).
# e) Zusatz: Mann-Whitney-U-Test als nichtparametrische Alternative.

data(mtcars)
g0 <- mtcars$mpg[mtcars$am == 0]   # Automatik
g1 <- mtcars$mpg[mtcars$am == 1]   # Schalt

# a)
boxplot(mpg ~ am, data = mtcars, names = c("Automatik", "Schalt"),
        col = c("lightblue", "salmon"),
        main = "Verbrauch nach Getriebe", ylab = "mpg")

# b) Normalverteilung pro Gruppe
# H0 
sw0 <- shapiro.test(g0)
sw1 <- shapiro.test(g1)
cat("SW Automatik: p =", round(sw0$p.value, 4), "\n")
cat("SW Schalt   : p =", round(sw1$p.value, 4), "\n")

# c) F-Test auf gleiche Varianz
vt <- var.test(g0, g1)
cat("F-Test auf gleiche Varianz: p =", round(vt$p.value, 4), "\n")

# d) zweiseitiger Zweistichproben t-Test
# H0 gleiche Mittelwerte
t_glh <- t.test(g0, g1, var.equal = TRUE)               # gleiche Varianzen
t_welch <- t.test(g0, g1, var.equal = FALSE)             # Welch-Test
print(t_glh)
print(t_welch)

# e) Mann-Whitney-U-Test
mw <- wilcox.test(g0, g1, paired = FALSE, alternative = "two.sided")
cat("Mann-Whitney-U: p =", round(mw$p.value, 4), "\n")


# -----------------------------------------------------------------------------
# Aufgabe 2 – Einseitiger Test: ist Schalt-mpg HÖHER als Automatik?
# -----------------------------------------------------------------------------
# H0: mu_schalt <= mu_auto
# H1: mu_schalt  > mu_auto
mean(g1)
mean(g0)

t_einseitig <- t.test(g1, g0, alternative = "greater", var.equal = FALSE)
print(t_einseitig)


# -----------------------------------------------------------------------------
# Aufgabe 3 – ChickWeight: zwei Diät-Typen vergleichen
# -----------------------------------------------------------------------------
# Vergleiche das Endgewicht (Tag 21) der Küken unter Diät 1 vs. Diät 2.

data(ChickWeight)
end <- subset(ChickWeight, Time == 21)

g_d1 <- end$weight[end$Diet == 1]
g_d2 <- end$weight[end$Diet == 2]

cat("Mittelwert Diät 1:", round(mean(g_d1), 2), "\n")
cat("Mittelwert Diät 2:", round(mean(g_d2), 2), "\n")

# F-Test auf gleiche Varianz
var.test(g_d1, g_d2)

# Welch-t-Test (n klein, ungleiche Gruppen)
welch <- t.test(g_d1, g_d2, var.equal = FALSE)
print(welch)

# Visualisierung mit ggplot2
ggplot(end[end$Diet %in% c(1, 2), ],
       aes(x = factor(Diet), y = weight, fill = factor(Diet))) +
  geom_boxplot(alpha = 0.7) +
  labs(title = "Endgewicht der Küken (Tag 21)",
       x = "Diät", y = "Gewicht") 

sub <- end[end$Diet %in% c(1, 2), ]
boxplot(weight ~ Diet, data = sub,
        col = c("lightgreen", "lightcoral"),
        main = "Endgewicht der Küken (Tag 21)")

# -----------------------------------------------------------------------------
# Aufgabe 4 – Iris: Sepal.Length zwischen setosa und versicolor
# -----------------------------------------------------------------------------
# Vergleiche Sepal.Length in den beiden Spezies.

data(iris)
sl_set <- iris$Sepal.Length[iris$Species == "setosa"]
sl_ver <- iris$Sepal.Length[iris$Species == "versicolor"]

# F-Test + t-Test / hier wieder ein sehr kleines n -> Welch-Test
v_iris <- var.test(sl_set, sl_ver)
t_iris <- t.test(sl_set, sl_ver, var.equal = (v_iris$p.value > 0.05))
cat("F-Test p-Wert:", round(v_iris$p.value, 4), "\n")
print(t_iris)


# -----------------------------------------------------------------------------
# Aufgabe 5 – Power: ab welcher Differenz wird ein t-Test sicher signifikant?
# -----------------------------------------------------------------------------
# Simulation: ziehe je 30 Werte aus N(mu1, 1) und N(mu2, 1) mit
# delta = mu2 - mu1 in {0, 0.2, 0.5, 0.8, 1}. Berechne die Ablehnungsrate
# bei alpha = 0.05.

set.seed(2024)
m <- 1000          # Anzahl Wiederholungen
n <- 30            # je Gruppe
deltas <- c(0, 0.2, 0.5, 0.8, 1)
power <- sapply(deltas, function(d) {
  rejects <- replicate(m, {
    x1 <- rnorm(n, mean = 0,  sd = 1)
    x2 <- rnorm(n, mean = d,  sd = 1)
    t.test(x1, x2, var.equal = TRUE)$p.value < 0.05
  })
  mean(rejects)
})

plot(deltas, power, type = "b", pch = 20, lwd = 2, col = "darkblue",
     ylim = c(0, 1), xlab = "wahre Differenz delta", ylab = "Power",
     main = "Power-Kurve t-Test, n = 30 pro Gruppe")
abline(h = 0.05, col = "red", lty = 2)

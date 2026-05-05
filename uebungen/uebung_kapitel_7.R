# =============================================================================
# Übung Kapitel 7: Tests für Zusammenhangsmaße
# Themen: Pearson- vs. Spearman-Korrelation, Korrelations-Signifikanztest,
#         Chi-Quadrat-Unabhängigkeitstest, Mosaikplot
# Datensätze: mtcars, iris, HairEyeColor (R-base)
# =============================================================================

# install.packages("ggplot2")
library(ggplot2)


# -----------------------------------------------------------------------------
# Aufgabe 1 – Pearson- und Spearman-Korrelation
# -----------------------------------------------------------------------------
# mtcars: Zusammenhang Gewicht (wt) und Verbrauch (mpg).
#
# a) Berechne Pearson- und Spearman-Korrelation.
# b) Teste, ob die Pearson-Korrelation signifikant von 0 verschieden ist.
# c) Streudiagramm + Regressionsgerade.

data(mtcars)

cor_p <- cor(mtcars$wt, mtcars$mpg, method = "pearson")
cor_s <- cor(mtcars$wt, mtcars$mpg, method = "spearman")
cat("Pearson  =", round(cor_p, 3), "\n")
cat("Spearman =", round(cor_s, 3), "\n")

cor_test_p <- cor.test(mtcars$wt, mtcars$mpg, method = "pearson")
print(cor_test_p)

# Visualisierung
ggplot(mtcars, aes(x = wt, y = mpg)) +
  geom_point(color = "steelblue", size = 2.5) +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  labs(title = "Zusammenhang Gewicht und Verbrauch",
       x = "Gewicht (1000 lbs)", y = "Verbrauch (mpg)") +
  theme_minimal()


# -----------------------------------------------------------------------------
# Aufgabe 2 – Pearson vs. Spearman bei nichtlinearen Daten
# -----------------------------------------------------------------------------
# Erzeuge x = 1..50 und y = x^2 + leichtes Rauschen. Welche der beiden
# Korrelationen ist näher bei 1? Was bedeutet das?

set.seed(2025)
x <- 1:50
y <- x^2 + rnorm(50, mean = 0, sd = 50)

cat("Pearson  =", round(cor(x, y, method = "pearson"),  3), "\n")
cat("Spearman =", round(cor(x, y, method = "spearman"), 3), "\n")

plot(x, y, pch = 20, col = "darkblue",
     main = "Monoton, aber nichtlinear -> Spearman > Pearson")


# -----------------------------------------------------------------------------
# Aufgabe 3 – Korrelationsmatrix für iris
# -----------------------------------------------------------------------------
# Berechne die Pearson-Korrelationsmatrix der vier numerischen Variablen.
# Visualisiere sie z. B. als Heatmap (base oder ggplot2 + reshape2).

iris_num <- iris[, 1:4]
M <- round(cor(iris_num), 2)
print(M)

# einfache Visualisierung mit base R
heatmap(M, symm = TRUE, margins = c(8, 8), main = "Korrelationsmatrix iris")


# -----------------------------------------------------------------------------
# Aufgabe 4 – Chi-Quadrat-Unabhängigkeitstest
# -----------------------------------------------------------------------------
# Untersuche im Datensatz mtcars, ob die Anzahl Zylinder (cyl) und das
# Getriebe (am) unabhängig sind.
#
# H0: cyl und am sind unabhängig
# H1: es gibt einen Zusammenhang

tab <- table(mtcars$cyl, mtcars$am)
print(tab)

chi <- chisq.test(tab)
print(chi)

# Bei kleinen Erwartungswerten ggf. Simulation:
chi_sim <- chisq.test(tab, simulate.p.value = TRUE, B = 5000)
print(chi_sim)


# -----------------------------------------------------------------------------
# Aufgabe 5 – Mosaikplot zur Kontingenztabelle
# -----------------------------------------------------------------------------
# Visualisiere den Datensatz HairEyeColor als Mosaikplot.

data(HairEyeColor)
# über Geschlecht aggregieren
he <- apply(HairEyeColor, c(1, 2), sum)
print(he)

mosaicplot(he, color = TRUE, las = 1,
           main = "Haarfarbe vs. Augenfarbe",
           xlab = "Haar", ylab = "Augen")

chi_he <- chisq.test(he)
cat("Chi-Quadrat-Test HairEyeColor: p =",
    format.pval(chi_he$p.value), "\n")


# -----------------------------------------------------------------------------
# Aufgabe 6 – Korrelation pro Gruppe
# -----------------------------------------------------------------------------
# Für iris: Korrelation zwischen Sepal.Length und Petal.Length GETRENNT
# pro Spezies. Was beobachtest du?

for (sp in unique(iris$Species)) {
  sub <- iris[iris$Species == sp, ]
  r <- cor(sub$Sepal.Length, sub$Petal.Length)
  cat("Korrelation in", sp, ":", round(r, 3), "\n")
}
# -> Simpson-Paradoxon-artige Effekte: Gesamt-Korrelation kann höher als
#    die Korrelationen innerhalb der Gruppen sein.

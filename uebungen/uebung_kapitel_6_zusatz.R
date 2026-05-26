# =============================================================================
# Übung Kapitel 6 – Zusatz: Mehr als zwei Gruppen & nichtparametrische Tests
# Themen: Einfache Varianzanalyse (ANOVA), Levene-Test (Varianzhomogenität),
#         Kruskal-Wallis-Test, Post-hoc-Test (TukeyHSD), Vergleich von
#         zwei Anteilen (prop.test), Bootstrap-KI für Mittelwertdifferenz
# Datensätze: PlantGrowth, iris, ChickWeight (R-base)
# =============================================================================

# install.packages("car")
library(car)        # für leveneTest
library(ggplot2)


# -----------------------------------------------------------------------------
# Aufgabe 1 – Einfaktorielle ANOVA (PlantGrowth)
# -----------------------------------------------------------------------------
# PlantGrowth: Pflanzengewicht in 3 Behandlungsgruppen (ctrl, trt1, trt2).
#
# H0: alle Gruppenmittelwerte sind gleich
# H1: mindestens ein Mittelwert weicht ab
#
# a) Boxplot je Gruppe.
# b) Levene-Test auf Varianzhomogenität (Voraussetzung).
# c) ANOVA mit aov() / Modellzusammenfassung.
# d) Tukey-HSD als Post-hoc-Test.

data(PlantGrowth)

# a)
boxplot(weight ~ group, data = PlantGrowth,
        col = c("lightblue", "salmon", "lightgreen"),
        main = "PlantGrowth: Gewicht je Gruppe", ylab = "weight")

# b)
lev <- leveneTest(weight ~ group, data = PlantGrowth)
print(lev)

# c)
aov_mod <- aov(weight ~ group, data = PlantGrowth)
summary(aov_mod)

# d) Post-hoc
tuk <- TukeyHSD(aov_mod, conf.level = 0.95)
print(tuk)
plot(tuk, las = 1)


# -----------------------------------------------------------------------------
# Aufgabe 2 – Kruskal-Wallis-Test (nichtparametrisch)
# -----------------------------------------------------------------------------
# Wenn die Voraussetzungen einer ANOVA verletzt sind, eignet sich der
# Kruskal-Wallis-Test als Alternative (Rang-basierte Variante).
#
# Vergleiche Sepal.Length zwischen den drei iris-Spezies.

data(iris)
kw <- kruskal.test(Sepal.Length ~ Species, data = iris)
print(kw)

# paarweise Wilcoxon-Tests mit Korrektur (Bonferroni)
pairwise.wilcox.test(iris$Sepal.Length, iris$Species,
                     p.adjust.method = "bonferroni")


# -----------------------------------------------------------------------------
# Aufgabe 3 – ANOVA: ChickWeight an Tag 21
# -----------------------------------------------------------------------------
# Vergleiche das Endgewicht (Tag 21) der Küken in allen 4 Diät-Gruppen.

end <- subset(ChickWeight, Time == 21)
aov_end <- aov(weight ~ Diet, data = end)
summary(aov_end)

ggplot(end, aes(x = Diet, y = weight, fill = Diet)) +
  geom_boxplot(alpha = 0.7) +
  labs(title = "Endgewicht je Diät (Tag 21)",
       x = "Diät", y = "Gewicht") +
  theme_minimal()

TukeyHSD(aov_end)


# -----------------------------------------------------------------------------
# Aufgabe 4 – Vergleich von zwei Anteilen
# -----------------------------------------------------------------------------
# In Filiale A kaufen 45 von 200 Kunden ein Premium-Produkt,
# in Filiale B 70 von 250.
#
# H0: p_A  = p_B
# H1: p_A != p_B

erfolge      <- c(45, 70)
stichprobe   <- c(200, 250)

p2 <- prop.test(erfolge, stichprobe, alternative = "two.sided",
                correct = FALSE)
print(p2)

cat("p_A =", round(erfolge[1]/stichprobe[1], 3),
    " | p_B =", round(erfolge[2]/stichprobe[2], 3), "\n")


# -----------------------------------------------------------------------------
# Aufgabe 5 – Bootstrap-KI für Mittelwertdifferenz
# -----------------------------------------------------------------------------
# Wenn keine Verteilungsannahme getroffen werden soll, kann man ein
# Konfidenzintervall für mu1 - mu2 mittels Bootstrap erzeugen.

set.seed(2025)
g1 <- iris$Sepal.Length[iris$Species == "setosa"]
g2 <- iris$Sepal.Length[iris$Species == "versicolor"]

B <- 5000
boot_diff <- replicate(B, {
  s1 <- sample(g1, replace = TRUE)
  s2 <- sample(g2, replace = TRUE)
  mean(s1) - mean(s2)
})

ki <- quantile(boot_diff, probs = c(0.025, 0.975))
cat("Bootstrap-95%-KI für mu1 - mu2:", round(ki, 3), "\n")

hist(boot_diff, breaks = 40, col = "lightgray",
     main = "Bootstrap-Verteilung der Mittelwertdifferenz",
     xlab = "mu_setosa - mu_versicolor")
abline(v = ki, col = "red", lwd = 2, lty = 2)


# -----------------------------------------------------------------------------
# Aufgabe 6 – Permutationstest als verteilungsfreie Alternative
# -----------------------------------------------------------------------------
# Idee: Die Gruppenzugehörigkeit wird zufällig vertauscht und die
# Teststatistik (z.B. Mittelwertdifferenz) neu berechnet. Der p-Wert
# ergibt sich aus der empirischen Verteilung.

set.seed(7)
diff_obs <- mean(g1) - mean(g2)

pool <- c(g1, g2)
n1 <- length(g1)

perm_diff <- replicate(B, {
  s <- sample(pool)
  mean(s[1:n1]) - mean(s[(n1+1):length(pool)])
})

p_perm <- mean(abs(perm_diff) >= abs(diff_obs))
cat("Permutations-p-Wert:", round(p_perm, 4),
    "(beobachtete Differenz =", round(diff_obs, 3), ")\n")

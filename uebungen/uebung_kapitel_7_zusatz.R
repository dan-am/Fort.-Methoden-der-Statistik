# =============================================================================
# Übung Kapitel 7 – Zusatz: Weitere Zusammenhangsmaße & Tests
# Themen: Kendall's Tau, Fisher's Exact Test (kleine 2x2-Tabellen),
#         McNemar-Test (gepaarte Anteile), Cramers V (Stärke des Zusammenhangs),
#         partielle Korrelation, Vergleich mehrerer Korrelationen
# Datensätze: mtcars, iris, UCBAdmissions, eigene 2x2-Tabellen
# =============================================================================

# install.packages(c("ggplot2", "ppcor"))
library(ggplot2)


# -----------------------------------------------------------------------------
# Aufgabe 1 – Kendall's Tau
# -----------------------------------------------------------------------------
# Kendall's Tau ist ein weiteres Rang-Korrelationsmaß. Es zählt konkordante
# vs. diskordante Paare und ist bei kleinen Stichproben oft stabiler als
# Spearman.
#
# Berechne für mtcars die Kendall-Korrelation zwischen wt und mpg und
# vergleiche sie mit Pearson und Spearman.

data(mtcars)

cor_p <- cor(mtcars$wt, mtcars$mpg, method = "pearson")
cor_s <- cor(mtcars$wt, mtcars$mpg, method = "spearman")
cor_k <- cor(mtcars$wt, mtcars$mpg, method = "kendall")
cat("Pearson  =", round(cor_p, 3),
    " | Spearman =", round(cor_s, 3),
    " | Kendall =", round(cor_k, 3), "\n")

# Signifikanztest:
ct <- cor.test(mtcars$wt, mtcars$mpg, method = "kendall", exact = FALSE)
print(ct)


# -----------------------------------------------------------------------------
# Aufgabe 2 – Fisher's Exact Test (kleine Stichproben)
# -----------------------------------------------------------------------------
# Bei kleinen Häufigkeiten in 2x2-Kontingenztabellen ist der
# Chi-Quadrat-Test ungenau. Fisher's exakter Test berechnet den p-Wert
# direkt über die hypergeometrische Verteilung.
#
# Beispiel: 14 Patienten bekommen Medikament A oder Placebo:
#                Genesung   keine Genesung
#   Medikament       6              1
#   Placebo          2              5
#
# H0: kein Zusammenhang zwischen Behandlung und Genesung

tab <- matrix(c(6, 2, 1, 5), nrow = 2,
              dimnames = list(Behandlung = c("Medikament", "Placebo"),
                              Ergebnis   = c("Genesung", "keine")))
print(tab)

fisher.test(tab, alternative = "two.sided")

# Vergleich mit Chi-Quadrat (mit Warnung, weil Erwartungswerte klein sind):
suppressWarnings(chisq.test(tab))


# -----------------------------------------------------------------------------
# Aufgabe 3 – McNemar-Test (gepaarte Anteile)
# -----------------------------------------------------------------------------
# Anwendung: Vorher-Nachher-Studie. 100 Personen werden vor und nach einer
# Schulung gefragt, ob sie ein Produkt kaufen würden.
#
#                  Nachher: Ja   Nachher: Nein
#   Vorher: Ja          30             5
#   Vorher: Nein        20            45
#
# Frage: Hat sich der Anteil der Ja-Stimmen signifikant verändert?

vorher_nachher <- matrix(c(30, 20, 5, 45), nrow = 2,
                         dimnames = list(Vorher  = c("Ja", "Nein"),
                                         Nachher = c("Ja", "Nein")))
print(vorher_nachher)

mn <- mcnemar.test(vorher_nachher, correct = TRUE)
print(mn)
cat("Diskordante Zellen:  Vorher Ja/Nachher Nein =",
    vorher_nachher[1, 2], "  vs.  Vorher Nein/Nachher Ja =",
    vorher_nachher[2, 1], "\n")


# -----------------------------------------------------------------------------
# Aufgabe 4 – Cramers V als Effektstärke
# -----------------------------------------------------------------------------
# Der Chi-Quadrat-Test sagt, OB ein Zusammenhang besteht; Cramers V sagt,
# WIE STARK er ist:
#   V = sqrt( chi^2 / (n * (min(rows, cols) - 1)) )    in [0, 1]
#
# Wende das auf die Kontingenztabelle (cyl x am) aus mtcars an.

tab2 <- table(mtcars$cyl, mtcars$am)
chi <- chisq.test(tab2)
n   <- sum(tab2)
V   <- sqrt(chi$statistic / (n * (min(dim(tab2)) - 1)))
names(V) <- NULL
cat("Chi^2 =", round(chi$statistic, 3),
    " | p =", round(chi$p.value, 4),
    " | Cramer V =", round(V, 3), "\n")
# Faustregel: V ≈ 0.1 schwach, 0.3 mittel, 0.5 stark.


# -----------------------------------------------------------------------------
# Aufgabe 5 – UCBAdmissions: Simpson-Paradoxon
# -----------------------------------------------------------------------------
# Klassisches Beispiel: Aggregierte Daten zeigen scheinbare Diskriminierung,
# die in den einzelnen Fakultäten verschwindet oder sich umkehrt.

data(UCBAdmissions)

# über alle Departments aggregiert
gesamt <- apply(UCBAdmissions, c(1, 2), sum)
print(gesamt)
chisq.test(gesamt)

# pro Department
for (d in dimnames(UCBAdmissions)$Dept) {
  t <- UCBAdmissions[, , d]
  p <- chisq.test(t)$p.value
  cat("Dept", d, ": p =", round(p, 4), "\n")
}


# -----------------------------------------------------------------------------
# Aufgabe 6 – Partielle Korrelation
# -----------------------------------------------------------------------------
# Was ist der Zusammenhang zwischen mpg und hp, NACHDEM man für wt
# kontrolliert hat? Beantwortet die Frage: bleibt der Zusammenhang
# bestehen, wenn das Gewicht herausgerechnet wird?
#
# Eigene Implementation mit lm() (ohne Zusatzpaket):

r1 <- residuals(lm(mpg ~ wt, data = mtcars))   # mpg bereinigt um wt
r2 <- residuals(lm(hp  ~ wt, data = mtcars))   # hp  bereinigt um wt

partiell <- cor(r1, r2)
einfach  <- cor(mtcars$mpg, mtcars$hp)

cat("einfache Korrelation mpg ~ hp     :", round(einfach,  3), "\n")
cat("partielle Korrelation (kontrolliert für wt):",
    round(partiell, 3), "\n")
# Falls |partiell| << |einfach|: ein großer Teil des Zusammenhangs läuft
# über das Gewicht.


# -----------------------------------------------------------------------------
# Aufgabe 7 – Konfidenzintervall für eine Korrelation (Fisher z)
# -----------------------------------------------------------------------------
# Die Fisher-z-Transformation liefert ein KI für die wahre Korrelation rho.

x <- mtcars$wt
y <- mtcars$mpg
r <- cor(x, y)
n <- length(x)
alpha <- 0.05

z   <- 0.5 * log((1 + r) / (1 - r))         # Fisher-z
se  <- 1 / sqrt(n - 3)
zq  <- qnorm(1 - alpha/2)
ci_z <- c(z - zq * se, z + zq * se)
ci_r <- (exp(2 * ci_z) - 1) / (exp(2 * ci_z) + 1)

cat("r =", round(r, 3),
    " | 95%-KI für rho:", round(ci_r, 3), "\n")

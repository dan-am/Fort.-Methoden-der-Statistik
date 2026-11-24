# =============================================================================
# R-Beispiele 28 bis 32 zu Kapitel 8: Lineare Regression
# Quelle: Textseiten „R Beispiel 28“ bis „R Beispiel 31-32“ im LMS
# Überarbeitet am 21.11.2026; Änderungen gegenüber dem LMS stehen in lms/AENDERUNGEN.md
# Benötigt nur Base R.
# =============================================================================

data(mtcars)
# mpg: Meilen je Gallone, wt: Gewicht in 1.000 lbs, hp: PS,
# am: Getriebe (0 = Automatik, 1 = Schaltgetriebe)


# -----------------------------------------------------------------------------
# Beispiel 28 (Kap. 8.1): Einfache lineare Regression mpg ~ wt
# -----------------------------------------------------------------------------
reg_wt <- lm(mpg ~ wt, data = mtcars)
summary(reg_wt)

plot(mtcars$wt, mtcars$mpg, main = "Lineare Einfachregression mpg ~ wt",
     xlab = "Gewicht (1.000 lbs)", ylab = "Meilen je Gallone (mpg)",
     pch = 20, ylim = c(5, 35), col = "darkblue")
abline(reg_wt, col = "darkred", lwd = 2)


# -----------------------------------------------------------------------------
# Beispiel 29 (Kap. 8.2): Prognose mit Vorhersageintervall
# -----------------------------------------------------------------------------
modell <- lm(mpg ~ wt, data = mtcars)
neue_daten <- data.frame(wt = seq(4, 5, length.out = 6))   # Gewichte für die Prognose
prognose <- predict(modell, neue_daten, interval = "prediction", level = 0.95)
cbind(neue_daten, prognose)

plot(mtcars$wt, mtcars$mpg, main = "Lineare Regression und Prognose",
     xlab = "Gewicht (1.000 lbs)", ylab = "Meilen je Gallone (mpg)", ylim = c(0, 36))
abline(modell, col = "red")
lines(neue_daten$wt, prognose[, "fit"], col = "blue", lty = 2, lwd = 2)
lines(neue_daten$wt, prognose[, "lwr"], col = "darkgreen", lty = 2, lwd = 2)
lines(neue_daten$wt, prognose[, "upr"], col = "darkgreen", lty = 2, lwd = 2)
legend("topright", legend = c("Regressionsgerade", "Prognose", "95-%-Vorhersageintervall"),
       col = c("red", "blue", "darkgreen"), lty = c(1, 2, 2), lwd = c(1, 2, 2), cex = 0.8)


# -----------------------------------------------------------------------------
# Beispiel 30 (Kap. 8.3): Multiple lineare Regression
# -----------------------------------------------------------------------------
# mpg in Abhängigkeit von Gewicht (wt), Getriebe (am) und PS (hp)
modell_3 <- lm(mpg ~ wt + am + hp, data = mtcars)
summary(modell_3)

# Vier Diagnoseplots: Residuen gegen geschätzte Werte, QQ-Plot der Residuen,
# Scale-Location, Residuen gegen Hebelwerte
par(mfrow = c(2, 2))
plot(modell_3)
par(mfrow = c(1, 1))


# -----------------------------------------------------------------------------
# Beispiel 31 (Kap. 8.4): Overfitting, Modell mit allen Variablen
# -----------------------------------------------------------------------------
modell_4 <- lm(mpg ~ ., data = mtcars)   # alle übrigen Variablen als Regressoren
summary(modell_4)
# Hohes R^2, aber kaum ein Koeffizient ist einzeln signifikant: Zehn Regressoren
# bei 32 Beobachtungen passen sich auch dem Zufall in den Daten an.


# -----------------------------------------------------------------------------
# Beispiel 32 (Kap. 8.4): Modellselektion mit dem AIC (Rückwärtsauswahl)
# -----------------------------------------------------------------------------
bestes_modell <- step(modell_4, direction = "backward", trace = 1)
summary(bestes_modell)

c(AIC_alle_Variablen = AIC(modell_4), AIC_Auswahl = AIC(bestes_modell))
# AIC() rechnet mit der vollständigen Log-Likelihood und zählt die Varianz als
# Parameter mit; step() nutzt extractAIC() ohne diese Konstanten. Der Abstand ist
# bei jedem Modell gleich, n * (log(2 * pi) + 1) + 2 = 92,81 für n = 32, und
# ändert die Rangfolge der Modelle nicht.
c(adj_R2_alle_Variablen = summary(modell_4)$adj.r.squared,
  adj_R2_Auswahl = summary(bestes_modell)$adj.r.squared)

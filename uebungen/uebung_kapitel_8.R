# =============================================================================
# Übung Kapitel 8: Lineare Regression (einfach & multiple) + Prediction
# Themen: lm(), summary(), Koeffizienten interpretieren, R^2 und adj. R^2,
#         Residuenanalyse, Train/Test-Split, RMSE, MAE, Vorhersage
# Datensätze: mtcars (R-base)
# =============================================================================

# Pakete installieren, falls sie noch fehlen
pakete  <- c("caret", "ggplot2", "corrplot")
fehlend <- setdiff(pakete, rownames(installed.packages()))
if (length(fehlend) > 0) install.packages(fehlend, repos = "https://cloud.r-project.org")

library(caret)
library(ggplot2)
library(corrplot)

# -----------------------------------------------------------------------------
# Aufgabe 1 – Einfache lineare Regression
# -----------------------------------------------------------------------------
# Modell: mpg ~ wt
#
# a) Schätze das Modell, gib summary() aus.
# b) Interpretiere Intercept, Steigung und R^2.
# c) Plotte die Regressionsgerade.
# d) Sage den Verbrauch für ein Auto mit wt = 3.0 voraus (mit 95 %-PI).

data(mtcars)

# a)
mod1 <- lm(mpg ~ wt, data = mtcars)
summary(mod1)

# b) Interpretation
# Steigung: Je 1.000 lbs mehr Gewicht sinkt der Verbrauchswert um rund 5,3 mpg.
# Intercept: rechnerischer Wert bei wt = 0; ein Auto ohne Gewicht gibt es nicht,
#            der Achsenabschnitt ist hier deshalb nicht inhaltlich interpretierbar.
# R^2: Das Gewicht erklärt rund 75 % der Streuung von mpg.

# c)
plot(mtcars$wt, mtcars$mpg, pch = 20, col = "steelblue",
     xlab = "wt", ylab = "mpg",
     main = "mpg ~ wt")
abline(mod1, col = "red", lwd = 2)

# d) Punktvorhersage + Vorhersageintervall
neu <- data.frame(wt = 3.0)
predict(mod1, newdata = neu, interval = "prediction", level = 0.95)


# -----------------------------------------------------------------------------
# Aufgabe 2 – Multiple Regression
# -----------------------------------------------------------------------------
# Modell: mpg ~ wt + hp + cyl
#
# a) Schätze das Modell.
# b) Welche Variablen sind signifikant (p-Wert)?
# c) Vergleiche R^2 und adjusted R^2 mit Modell 1.

mod2 <- lm(mpg ~ wt + hp + cyl, data = mtcars)
summary(mod2)

cat("R^2     mod1:", round(summary(mod1)$r.squared,    3),
    " | mod2:",     round(summary(mod2)$r.squared,    3), "\n")
cat("adj R^2 mod1:", round(summary(mod1)$adj.r.squared, 3),
    " | mod2:",     round(summary(mod2)$adj.r.squared, 3), "\n")


# -----------------------------------------------------------------------------
# Aufgabe 3 – Residuenanalyse
# -----------------------------------------------------------------------------
# a) Vier diagnostische Plots (Residuen, QQ, Skalierung, Hebel).
# b) Histogramm der Residuen.
# c) Shapiro-Wilk-Test auf Normalverteilung der Residuen.

# a)
par(mfrow = c(2, 2))
plot(mod2)
par(mfrow = c(1, 1))

# b)
hist(residuals(mod2), breaks = 12, col = "lightgray", freq = FALSE,
     main = "Histogramm der Residuen von Modell 2", xlab = "Residuen")

# Zum Vergleich: Residuen von Modell 1 in der Reihenfolge der Daten
plot(residuals(mod1), pch = 20, col = "darkgreen",
     xlab = "Index", ylab = "Residuen",
     main = "Residuen von Modell 1")
abline(h = 0, lty = 2)

# c)
sw <- shapiro.test(residuals(mod2))
cat("Shapiro-Wilk Residuen: p =", round(sw$p.value, 4), "\n")


# -----------------------------------------------------------------------------
# Aufgabe 4 – Train/Test-Split + Vorhersagegüte
# -----------------------------------------------------------------------------
# Aufteilung mtcars in 80 % Training und 20 % Test.
# Modell: mpg ~ wt + hp + cyl + am.
# Berechne MAE und RMSE auf den Testdaten.

# Vorab: Korrelationen aller Variablen als Orientierung für die Modellwahl
M <- cor(mtcars)
corrplot(M, method = "number", number.cex = 0.6)

set.seed(42)
idx_train <- createDataPartition(mtcars$mpg, p = 0.8, list = FALSE)
train <- mtcars[idx_train, ]
test  <- mtcars[-idx_train, ]

mod3 <- lm(mpg ~ wt + hp + cyl + am, data = train)
summary(mod3)

# In-sample-Fehler (auf den Trainingsdaten)
rse         <- sqrt(sum(residuals(mod3)^2) / df.residual(mod3)) # Residualstandardfehler, mit Freiheitsgraden
rmse_train  <- sqrt(mean(residuals(mod3)^2))                    # RMSE ohne Freiheitsgrade
cat("RSE Training :", round(rse, 3), "| RMSE Training:", round(rmse_train, 3), "\n")

# Out-of-sample-Fehler (auf den Testdaten), für Vorhersagen entscheidend
pred <- predict(mod3, newdata = test)
res  <- test$mpg - pred

mae_test  <- mean(abs(res))
rmse_test <- sqrt(mean(res^2))
cat("MAE  Test     :", round(mae_test,  3), "\n")
cat("RMSE Test     :", round(rmse_test, 3), "\n")


# -----------------------------------------------------------------------------
# Aufgabe 5 – Visualisierung: tatsächlich vs. vorhergesagt
# -----------------------------------------------------------------------------
plot_df <- data.frame(actual = test$mpg, predicted = pred)
grenzen <- range(c(plot_df$actual, plot_df$predicted))   # gleiche Skala auf beiden Achsen

ggplot(plot_df, aes(x = actual, y = predicted)) +
  geom_point(size = 3, color = "steelblue") +
  coord_equal(xlim = grenzen, ylim = grenzen) +
  geom_abline(slope = 1, intercept = 0, color = "red", linetype = "dashed") +
  labs(title = "Tatsächlich vs. Vorhergesagt (Testdaten)",
       x = "tatsächlicher mpg", y = "vorhergesagter mpg") +
  theme_minimal()

# Residuenplot
ggplot(data.frame(predicted = pred, residual = res),
       aes(x = predicted, y = residual)) +
  geom_point(size = 3, color = "darkgreen") +
  geom_hline(yintercept = 0, color = "red", linetype = "dashed") +
  labs(title = "Residuen vs. Vorhergesagt",
       x = "vorhergesagter mpg", y = "Residuum") +
  theme_minimal()


# -----------------------------------------------------------------------------
# Aufgabe 6 – Modellvergleich mit anova()
# -----------------------------------------------------------------------------
# Sind die zusätzlichen Variablen in Modell 2 statistisch nötig
# (gegenüber Modell 1 mit nur wt)?

mod_klein  <- lm(mpg ~ wt,           data = mtcars)
mod_mittel <- lm(mpg ~ wt + cyl,      data = mtcars)
mod_gross  <- lm(mpg ~ wt + hp + cyl, data = mtcars)

anova(mod_klein, mod_mittel, mod_gross)
# Kleiner p-Wert in einer Zeile: Das jeweils größere Modell erklärt signifikant
# mehr Varianz als das Modell in der Zeile darüber.

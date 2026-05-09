# =============================================================================
# Übung Kapitel 8: Lineare Regression (einfach & multiple) + Prediction
# Themen: lm(), summary(), Koeffizienten interpretieren, R^2 und adj. R^2,
#         Residuenanalyse, Train/Test-Split, RMSE, MAE, Vorhersage
# Datensätze: mtcars (R-base)
# =============================================================================
install.packages("corrplot")
install.packages(c("caret", "ggplot2"))
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

mod1 <- lm(mpg ~ wt, data = mtcars)
summary(mod1)

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

par(mfrow = c(2, 2))
plot(mod2)
par(mfrow = c(1, 1))

hist(residuals(mod2), breaks = 12, col = "lightgray",
     main = "Histogramm der Residuen", xlab = "Residuen")

sw <- shapiro.test(residuals(mod2))
cat("Shapiro-Wilk Residuen: p =", round(sw$p.value, 4), "\n")


# -----------------------------------------------------------------------------
# Aufgabe 4 – Train/Test-Split + Vorhersagegüte
# -----------------------------------------------------------------------------
# Aufteilung mtcars in 80 % Training und 20 % Test.
# Modell: mpg ~ wt + hp + cyl + am.
# Berechne MAE und RMSE auf den Testdaten.
M = cor(mtcars)
corrplot(M, method = "number")

set.seed(42)
idx_train <- createDataPartition(mtcars$mpg, p = 0.8, list = FALSE)
train <- mtcars[idx_train, ]
test  <- mtcars[-idx_train, ]

mod3 <- lm(mpg ~ wt + cyl , data = train)
summary(mod3)


# Freiheitsgrade berechenen
n <- nrow(train)  # Anzahl Beobachtungen im Training
p <- length(coef(mod3)) - 1  # Anzahl Prädiktoren (ohne Intercept)
df <- n - p - 1

sd(mod3$residuals)

# in-sample / RMSE vor allem für Vorhersagen, out-of sample relevant
rse = sqrt(sum(residuals(mod3)^2) / (nrow(train) - length(coef(mod3)) )) # genauere Berechnung mit Freiheitsgraden
rmse = sqrt(sum(residuals(mod3)^2)/  nrow(train) ) # einfachere Berechnung ohne Freiheitsgrade

# out-of-sample / test bzw. hold-out RMSE
pred <- predict(mod3, newdata = test)
res  <- test$mpg - pred

mae  <- mean(abs(res))
rmse <- sqrt(mean(res^2))
cat("MAE  Test     :", round(mae,  3), "\n")
cat("RMSE Test     :", round(rmse, 3), "\n")


# -----------------------------------------------------------------------------
# Aufgabe 5 – Visualisierung: tatsächlich vs. vorhergesagt
# -----------------------------------------------------------------------------
plot_df <- data.frame(actual = test$mpg, predicted = pred)

ggplot(plot_df, aes(x = actual, y = predicted)) +
  geom_point(size = 3, color = "steelblue") +
  coord_cartesian(xlim = c(10, 25), ylim = c(0, 30)) +
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

mod_klein <- lm(mpg ~ wt,           data = mtcars)
mod_mittel <- lm(mpg ~ wt + cyl, data = mtcars)
mod_gross <- lm(mpg ~ wt + hp + cyl, data = mtcars)

(anova(mod_klein, mod_mittel, mod_gross))
# kleiner p-Wert -> das größere Modell erklärt signifikant mehr Varianz.

# =============================================================================
# Übung Kapitel 8 – Zusatz: Erweiterungen der linearen Regression
# Themen: Polynomiale Regression, Interaktionseffekte, Dummy-Codierung,
#         Variablenselektion (AIC / step), Multikollinearität (VIF),
#         k-fache Kreuzvalidierung, Konfidenz- vs. Vorhersageintervall,
#         Log-Transformation
# Datensätze: mtcars, cars, airquality (R-base)
# =============================================================================

# install.packages(c("car", "ggplot2"))
library(car)        # für vif()
library(ggplot2)


# -----------------------------------------------------------------------------
# Aufgabe 1 – Polynomiale Regression
# -----------------------------------------------------------------------------
# Im cars-Datensatz (Bremsweg in Abhängigkeit der Geschwindigkeit) ist der
# Zusammenhang nicht ganz linear, weil der Bremsweg quadratisch mit der
# Geschwindigkeit wächst.
#
# a) Schätze ein Modell zweiter Ordnung: dist ~ speed + I(speed^2).
# b) Vergleiche mit dem linearen Modell mittels anova() und R^2.
# c) Visualisiere beide Anpassungen.

data(cars)

mod_lin <- lm(dist ~ speed,                    data = cars)
mod_qua <- lm(dist ~ speed + I(speed^2),       data = cars)

summary(mod_lin)$r.squared
summary(mod_qua)$r.squared

anova(mod_lin, mod_qua)   # Modellvergleich

# Plot
neue_speed <- data.frame(speed = seq(min(cars$speed), max(cars$speed), 0.1))
plot(cars$speed, cars$dist, pch = 20, col = "steelblue",
     xlab = "Geschwindigkeit", ylab = "Bremsweg",
     main = "Linear vs. quadratische Anpassung")
lines(neue_speed$speed, predict(mod_lin, neue_speed), col = "red",   lwd = 2)
lines(neue_speed$speed, predict(mod_qua, neue_speed), col = "green", lwd = 2)
legend("topleft", c("linear", "quadratisch"),
       col = c("red", "green"), lwd = 2)


# -----------------------------------------------------------------------------
# Aufgabe 2 – Interaktionseffekte (Wechselwirkung)
# -----------------------------------------------------------------------------
# Frage: Hängt der Effekt von hp auf mpg davon ab, ob der Wagen ein
# Schaltgetriebe hat (am = 1) oder nicht (am = 0)?
#
# Modell mit Wechselwirkung:  mpg ~ hp * factor(am)
#   - hp                 -> Haupteffekt von hp
#   - factor(am)1        -> Niveau-Unterschied zwischen am=0 und am=1
#   - hp:factor(am)1     -> unterschiedliche Steigung von hp je am

data(mtcars)
mod_int <- lm(mpg ~ hp * factor(am), data = mtcars)
summary(mod_int)

ggplot(mtcars, aes(x = hp, y = mpg, color = factor(am))) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Wechselwirkung hp x am",
       color = "am (0=auto, 1=manuell)") +
  theme_minimal()


# -----------------------------------------------------------------------------
# Aufgabe 3 – Dummy-Codierung kategorialer Variablen
# -----------------------------------------------------------------------------
# Die Variable cyl (4 / 6 / 8) soll nicht als metrische, sondern als
# kategoriale Variable in die Regression eingehen. R erzeugt automatisch
# Dummy-Variablen, wenn factor() verwendet wird.

mod_cat <- lm(mpg ~ factor(cyl) + wt, data = mtcars)
summary(mod_cat)

# Geschätzte Mittelwerte je Zylinderzahl bei wt = mean(wt)
neue <- data.frame(cyl = c(4, 6, 8), wt = mean(mtcars$wt))
neue$cyl <- factor(neue$cyl, levels = c(4, 6, 8))
predict(mod_cat, newdata = neue)


# -----------------------------------------------------------------------------
# Aufgabe 4 – Variablenselektion mit AIC (step)
# -----------------------------------------------------------------------------
# Beginne mit dem vollen Modell und reduziere es schrittweise rückwärts
# auf das beste Modell laut AIC.

voll  <- lm(mpg ~ ., data = mtcars)
beste <- step(voll, direction = "backward", trace = FALSE)
summary(beste)
cat("AIC voll =", AIC(voll), " | AIC reduziert =", AIC(beste), "\n")

# Vorwärts-Auswahl ausgehend vom Nullmodell:
nulle <- lm(mpg ~ 1, data = mtcars)
vor   <- step(nulle,
              scope = list(lower = nulle, upper = voll),
              direction = "forward", trace = FALSE)
formula(vor)


# -----------------------------------------------------------------------------
# Aufgabe 5 – Multikollinearität: Variance Inflation Factor (VIF)
# -----------------------------------------------------------------------------
# Wenn Regressoren stark untereinander korreliert sind, werden die
# Schätzer instabil. Faustregel:
#   VIF > 5  -> kritisch
#   VIF > 10 -> stark problematisch

mod_voll <- lm(mpg ~ disp + hp + wt + qsec + drat, data = mtcars)
vif(mod_voll)

# Korrelationsmatrix der Regressoren zur Veranschaulichung
round(cor(mtcars[, c("disp", "hp", "wt", "qsec", "drat")]), 2)


# -----------------------------------------------------------------------------
# Aufgabe 6 – k-fache Kreuzvalidierung (k = 5) per Hand
# -----------------------------------------------------------------------------
# Schätze die out-of-sample-RMSE des Modells mpg ~ wt + hp via 5-facher CV.

set.seed(123)
k <- 5
fold <- sample(rep(1:k, length.out = nrow(mtcars)))

rmse_folds <- numeric(k)
for (i in 1:k) {
  train <- mtcars[fold != i, ]
  test  <- mtcars[fold == i, ]
  m     <- lm(mpg ~ wt + hp, data = train)
  pred  <- predict(m, newdata = test)
  rmse_folds[i] <- sqrt(mean((test$mpg - pred)^2))
}
cat("RMSE pro Fold:", round(rmse_folds, 3), "\n")
cat("CV-RMSE (Mittel):", round(mean(rmse_folds), 3), "\n")


# -----------------------------------------------------------------------------
# Aufgabe 7 – Konfidenz- vs. Vorhersageintervall
# -----------------------------------------------------------------------------
# - Konfidenzintervall: Unsicherheit der mittleren Antwort E(Y|x).
# - Vorhersageintervall: Unsicherheit einer EINZELNEN neuen Beobachtung
#   y_neu (immer breiter, weil Residual-Streuung dazu kommt).

mod <- lm(mpg ~ hp, data = mtcars)
neue_hp <- data.frame(hp = seq(50, 330, by = 10))
ci  <- predict(mod, newdata = neue_hp, interval = "confidence", level = 0.95)
pi  <- predict(mod, newdata = neue_hp, interval = "prediction", level = 0.95)

plot(mtcars$hp, mtcars$mpg, pch = 20, col = "steelblue",
     xlab = "hp", ylab = "mpg",
     main = "Konfidenz- vs. Vorhersageintervall")
lines(neue_hp$hp, ci[, "fit"], col = "red", lwd = 2)
lines(neue_hp$hp, ci[, "lwr"], col = "red", lty = 2)
lines(neue_hp$hp, ci[, "upr"], col = "red", lty = 2)
lines(neue_hp$hp, pi[, "lwr"], col = "blue", lty = 3)
lines(neue_hp$hp, pi[, "upr"], col = "blue", lty = 3)
legend("topright",
       c("Regression", "95%-KI Mittelwert", "95%-Vorhersage-Intervall"),
       col = c("red", "red", "blue"), lty = c(1, 2, 3), lwd = c(2, 1, 1))


# -----------------------------------------------------------------------------
# Aufgabe 8 – Log-Transformation der Zielgröße
# -----------------------------------------------------------------------------
# Der airquality-Datensatz enthält rechtsschiefe Ozonwerte. Vergleiche
# Modelle mit und ohne Log-Transformation der Zielgröße.

aq <- na.omit(airquality)

mod1 <- lm(Ozone        ~ Temp + Wind + Solar.R, data = aq)
mod2 <- lm(log(Ozone)   ~ Temp + Wind + Solar.R, data = aq)

par(mfrow = c(2, 2))
plot(mod1, which = 1, main = "Residuen ~ Fitted (Ozone)")
plot(mod1, which = 2, main = "QQ (Ozone)")
plot(mod2, which = 1, main = "Residuen ~ Fitted (log Ozone)")
plot(mod2, which = 2, main = "QQ (log Ozone)")
par(mfrow = c(1, 1))

cat("R^2 (Ozone)     =", round(summary(mod1)$r.squared, 3), "\n")
cat("R^2 (log Ozone) =", round(summary(mod2)$r.squared, 3), "\n")
# -> die Log-Transformation linearisiert den Zusammenhang und stabilisiert
#    die Varianz der Residuen.

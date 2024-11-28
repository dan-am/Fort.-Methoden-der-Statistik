# Bibliotheken laden
installed.packages("caret")
library(caret) # Für Datenaufteilung und Modellbewertung
library(ggplot2)

# Daten einlesen
file_path_to <- "data/input/kc_house_data.csv"
# Datensatz einlesen
data <-read.csv(file_path_to) %>% 
  mutate(date = lubridate::as_date(date))

# Wählen relevanter Variablen für die Regression
# Preis ist die Zielvariable, Wohnfläche, Schlafzimmer und Waterfront sind Prädiktoren
data <- data[, c("price", "sqft_living", "waterfront", "bedrooms")] # das hier ist base R

# ---- 1. Aufteilen der Daten in Trainings- und Testdaten ------------------
set.seed(42)  # Reproduzierbarkeit
train_indices <- createDataPartition(data$price, p = 0.8, list = FALSE)  # 80% Training
train_data <- data[train_indices, ]
test_data <- data[-train_indices, ]

# ---- 2. Multiple Regression: Modelltraining -----------------------------
# Modell auf den Trainingsdaten erstellen
model <- lm(price ~ sqft_living + bedrooms + waterfront, data = train_data)
sqrt(mean((train_data$price - model$fitted.values )^2)) # in-sample RMSE Y_train - Y_train_pred
sqrt(mean((model$residuals )^2)) # in-sample RMSE epsilon

# Modellzusammenfassung
cat("Modellzusammenfassung:\n")
summary(model) 


# ---- 3. Modellbewertung -------------------------------------------------
# Vorhersagen auf den Testdaten
predicted_prices <- predict(model, newdata = test_data)

# Berechnung der Modellgüte (R² auf den Testdaten)
actual_prices <- test_data$price
r_squared <- cor(actual_prices, predicted_prices)^2
cat("R² auf den Testdaten:", r_squared, "\n")

# Berechnung des Mittleren Absoluten Fehlers (MAE) und des Root Mean Square Error (RMSE)
# für out-of-sample Vorhersagen von höherer Bedeutung
mae <- mean(abs(actual_prices - predicted_prices))
rmse <- sqrt(mean((actual_prices - predicted_prices)^2))
cat("Mittlerer Absoluter Fehler (MAE):", mae, "\n")
cat("Wurzel des mittleren Quadratischen Fehlers (RMSE):", rmse, "\n")

# ---- 4. Visualisierung der Ergebnisse -----------------------------------
# Vergleich von tatsächlichen und vorhergesagten Preisen
ggplot(data = test_data, aes(x = actual_prices, y = predicted_prices)) +
  geom_point(alpha = 0.5) +
  geom_abline(slope = 1, intercept = 0, color = "red", linetype = "dashed") +
  ggtitle("Tatsächliche vs. Vorhergesagte Preise") +
  xlab("Tatsächliche Preise") +
  ylab("Vorhergesagte Preise")

# Residuenplot
residuals <- actual_prices - predicted_prices
ggplot(data = test_data, aes(x = predicted_prices, y = residuals)) +
  geom_point(alpha = 0.5) +
  geom_hline(yintercept = 0, color = "red", linetype = "dashed") +
  ggtitle("Residuen vs. Vorhergesagte Preise") +
  xlab("Vorhergesagte Preise") +
  ylab("Residuen")

# ---- 5. Interpretation der Ergebnisse -----------------------------------
cat("Interpretation:\n")
cat("1. Das Modell erklärt, wie Wohnfläche, Anzahl der Schlafzimmer und die Wassersicht den Preis beeinflussen.\n")
cat("2. R² zeigt den Anteil der Varianz, den das Modell erklären kann. Höhere Werte deuten auf ein besseres Modell hin.\n")
cat("3. MAE und RMSE geben die durchschnittlichen Fehler zwischen den tatsächlichen und vorhergesagten Preisen an.\n")
cat("4. Der Residuenplot zeigt mögliche Abweichungen von den Modellannahmen.\n")
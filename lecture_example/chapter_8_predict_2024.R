# Kapitel 8: Vorhersage mit der multiplen Regression am King-County-Datensatz
# Präsenzveranstaltung 2024, überarbeitet am 21.11.2026
# Arbeitsverzeichnis: Projektordner (RStudio-Projekt Fort.-Methoden-der-Statistik.Rproj öffnen)

# Pakete einlesen
source("functions/packages.R") # installiert nur Pakete, die noch fehlen
source("functions/library.R")  # lädt die notwendigen Pakete
library(caret) # für Datenaufteilung und Modellbewertung

# Daten einlesen
file_path_to <- "data/input/kc_house_data.csv"
data <- read.csv(file_path_to) %>%
  mutate(date = lubridate::as_date(date))

# Wählen relevanter Variablen für die Regression
# Preis ist die Zielvariable, Wohnfläche, Schlafzimmer und Waterfront sind Prädiktoren
data <- data[, c("price", "sqft_living", "waterfront", "bedrooms")] # das hier ist base R

# ---- 1. Aufteilen der Daten in Trainings- und Testdaten ------------------
set.seed(42)  # Reproduzierbarkeit
train_indices <- createDataPartition(data$price, p = 0.8, list = FALSE)  # 80 % Training
train_data <- data[train_indices, ]
test_data <- data[-train_indices, ]

# ---- 2. Multiple Regression: Modelltraining -----------------------------
# Modell auf den Trainingsdaten erstellen
model <- lm(price ~ sqft_living + bedrooms + waterfront, data = train_data)
sqrt(mean((train_data$price - model$fitted.values)^2)) # in-sample RMSE: Y_train - Y_train_dach
sqrt(mean((model$residuals)^2)) # dasselbe über die Residuen epsilon

# Modellzusammenfassung
cat("Modellzusammenfassung:\n")
summary(model)


# ---- 3. Modellbewertung -------------------------------------------------
# Vorhersagen auf den Testdaten
predicted_prices <- predict(model, newdata = test_data)

# Modellgüte auf den Testdaten
actual_prices <- test_data$price
r_squared_test <- 1 - sum((actual_prices - predicted_prices)^2) /
  sum((actual_prices - mean(actual_prices))^2)
cat("R² auf den Testdaten (1 - SSE/SST):", r_squared_test, "\n")
cat("quadrierte Korrelation zum Vergleich:", cor(actual_prices, predicted_prices)^2, "\n")
# Die quadrierte Korrelation übersieht systematische Verschiebungen der Vorhersagen;
# außerhalb der Trainingsdaten ist 1 - SSE/SST das ehrlichere Maß.

# Berechnung des mittleren absoluten Fehlers (MAE) und des Root Mean Square Error (RMSE)
# für out-of-sample-Vorhersagen von höherer Bedeutung
mae <- mean(abs(actual_prices - predicted_prices))
rmse <- sqrt(mean((actual_prices - predicted_prices)^2))
cat("Mittlerer absoluter Fehler (MAE):", mae, "\n")
cat("Wurzel des mittleren quadratischen Fehlers (RMSE):", rmse, "\n")

# ---- 4. Visualisierung der Ergebnisse -----------------------------------
# Vergleich von tatsächlichen und vorhergesagten Preisen
ggplot(data = test_data, aes(x = actual_prices, y = predicted_prices)) +
  geom_point(alpha = 0.5) +
  geom_abline(slope = 1, intercept = 0, color = "red", linetype = "dashed") +
  ggtitle("Tatsächliche vs. vorhergesagte Preise") +
  xlab("Tatsächliche Preise") +
  ylab("Vorhergesagte Preise")

# Residuenplot
residuen_test <- actual_prices - predicted_prices
ggplot(data = test_data, aes(x = predicted_prices, y = residuen_test)) +
  geom_point(alpha = 0.5) +
  geom_hline(yintercept = 0, color = "red", linetype = "dashed") +
  ggtitle("Residuen vs. vorhergesagte Preise") +
  xlab("Vorhergesagte Preise") +
  ylab("Residuen")

# ---- 5. Interpretation der Ergebnisse -----------------------------------
cat("Interpretation:\n")
cat("1. Das Modell erklärt, wie Wohnfläche, Anzahl der Schlafzimmer und Wasserblick den Preis beeinflussen.\n")
cat("2. R² zeigt den Anteil der Varianz, den das Modell erklären kann. Höhere Werte deuten auf ein besseres Modell hin.\n")
cat("3. MAE und RMSE geben die durchschnittlichen Fehler zwischen den tatsächlichen und vorhergesagten Preisen an.\n")
cat("4. Der Residuenplot zeigt mögliche Abweichungen von den Modellannahmen.\n")

# Einlesen der pakete
source("functions/packages.R")
# Laden der notwendigen Bibliotheken

file_path_to <- "data/input/kc_house_data.csv"
# Datensatz einlesen
data <- read.csv(file_path_to)

# 1. Datenauswahl und Beschreibung -----
# Variablenbeschreibung:
summary(data)
str(data)

# 2. Explorative Datenanalyse
# Deskriptive Statistik für numerische Variablen
numerics <- data %>% select_if(is.numeric)
summary(numerics)

# Histogramm für den Verkaufspreis
ggplot(data, aes(x = price)) +
  geom_histogram(bins = 30, fill = "blue", color = "black") +
  ggtitle("Histogramm des Verkaufspreises") +
  xlab("Preis") +
  ylab("Häufigkeit")

# Boxplot: Wohnfläche nach Anzahl der Schlafzimmer
ggplot(data, aes(x = factor(bedrooms), y = sqft_living)) +
  geom_boxplot(fill = "orange") +
  ggtitle("Wohnfläche nach Anzahl der Schlafzimmer") +
  xlab("Anzahl der Schlafzimmer") +
  ylab("Wohnfläche (sqft)")

# Streudiagramm: Wohnfläche vs. Preis
ggplot(data, aes(x = sqft_living, y = price)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", se = TRUE, color = "red") +
  ggtitle("Wohnfläche vs. Verkaufspreis") +
  xlab("Wohnfläche (sqft)") +
  ylab("Preis")

# 3. Wahrscheinlichkeitstheorie anwenden
# Verteilungsanpassung für den Verkaufspreis
ggplot(data, aes(sample = price)) +
  stat_qq() +
  stat_qq_line() +
  ggtitle("QQ-Plot für den Verkaufspreis")

# Mittelwert und Standardabweichung der Preise
mean_price <- mean(data$price)
std_dev_price <- sd(data$price)

# Wahrscheinlichkeit, dass Preis > 500000
prob_price_greater_500k <- 1 - pnorm(500000, mean = mean_price, sd = std_dev_price)
prob_price_greater_500k


# 3.1. Kolmogorov-Smirnov Test
ks_test <- ks.test(data$price, "pnorm", mean = mean(data$price), sd = sd(data$price))
print(ks_test)
# Installieren Sie das Paket nortest, falls es nicht installiert ist
if (!require(nortest)) install.packages("nortest")
library(nortest)
# 3.2. Anderson-Darling-Test für Normalverteilung
ad_test <- ad.test(data$price)
print(ad_test)

# Histogramm der Daten mit theoretischer Normalverteilung
ggplot(data, aes(x = price)) +
  geom_histogram(aes(y = ..density..), bins = 30, fill = "blue", color = "black", alpha = 0.6) +
  stat_function(fun = dnorm, args = list(mean = mean(data$price), sd = sd(data$price)),
                color = "red", size = 1) +
  ggtitle("Vergleich: Histogramm vs. Normalverteilung") +
  xlab("Preis") +
  ylab("Dichte")

# 3.3 Maximum-Likelihood-Methode (MLE)
# Beispiel: Schätzung für Normalverteilung
log_likelihood <- function(params) {
  mu <- params[1]
  sigma <- params[2]
  -sum(dnorm(data$price, mean = mu, sd = sigma, log = TRUE))
}

mle_result <- optim(par = c(mean(data$price), sd(data$price)), fn = log_likelihood, method = "L-BFGS-B", lower = c(-Inf, 0))
cat("MLE-Schätzer für den Mittelwert:", mle_result$par[1], "\n")
cat("MLE-Schätzer für die Standardabweichung:", mle_result$par[2], "\n")

# 3.4. Mittlerer quadratischer Fehler (MSE) -----
predicted <- predict(model, data)
mse <- mean((data$price - predicted)^2)
cat("Mittlerer quadratischer Fehler (MSE):", mse, "\n")

# 4. Konfidenzintervalle
# 4.1 Konfidenzintervall für den Mittelwert des Verkaufspreises
mean_price <- mean(data$price)
std_error <- sd(data$price) / sqrt(nrow(data))
conf_interval <- mean_price + c(-1, 1) * qnorm(0.975) * std_error
conf_interval
cat("Konfidenzintervall für den Mittelwert des Preises (95%):", conf_interval, "\n")

# 4.2 Konfidenzintervall für den Anteil der Häuser mit Wasserblick
prop_waterfront <- mean(data$waterfront == 1)
std_error_prop <- sqrt(prop_waterfront * (1 - prop_waterfront) / nrow(data))
conf_interval_prop <- prop_waterfront + c(-1, 1) * qnorm(0.975) * std_error_prop
conf_interval_prop
cat("Konfidenzintervall für den Anteil der Häuser mit Wasserblick (95%):", conf_interval_prop, "\n")
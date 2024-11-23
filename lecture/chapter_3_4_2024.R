# Einlesen der pakete
source("functions/packages.R") # wenn dei paket nicht installiert sind, wird es installiert
source("functions/library.R")
# Laden der notwendigen Bibliotheken

file_path_to <- "data/input/kc_house_data.csv"
# Datensatz einlesen
data <-read.csv(file_path_to) %>% 
  mutate(date = lubridate::as_date(date))

# 1. Datenauswahl und Beschreibung: ----
#   
# 1.1. Datensatz wählen: 
# Ihr könnt die von mir zu Verfügung gestellten Daten nutzen. 
# Alternativ sucht oder erhebt Ihr einen Datensatz mit mindestens 100 Beobachtungen , 
# der sowohl numerische als auch kategoriale Variablen enthält.
# 1.2. Beschreibung: 
# Dokumentiert die Herkunft des Datensatzes und beschreibet alle Variablen 
# (Datentyp, Bedeutung, mögliche Werte).


# 1.2 Datenauswahl und Beschreibung -----
# Variablenbeschreibung anhand der Struktur des Datensatzes:
str(data)

# Ziel des Berichts ist es, die Hausverkäufe im King County, Washington (USA),
#mithilfe von Multipler Linearer Regression (MLR) vorherzusagen. Das Datenset umfasst
#Verkaufsdaten von Mai 2014 bis Mai 2015, mit dem Ziel, Verkäufe mit einer Genauigkeit
#von 75-80 % zu prognostizieren und Faktoren für hohe Immobilienwerte (über 650.000 USD) zu identifizieren.
# Der Datensatz enthält 21 Variablen und 21.613 Beobachtungen und umfasst die Region Seattle.

# A.Erstellen eines Profiling-Reports
create_report(data)

# B. Explorative Datenanalyse
names(data)
#21 Variablen
# [1] "id"            "date"          "price"         "bedrooms"      "bathrooms"     "sqft_living"  
# [7] "sqft_lot"      "floors"        "waterfront"    "view"          "condition"     "grade"        
# [13] "sqft_above"    "sqft_basement" "yr_built"      "yr_renovated"  "zipcode"       "lat"          
# [19] "long"          "sqft_living15" "sqft_lot15"

# Datentypen der 21 Variablen
# id (num)
# date (chr)
# price (num)
# bedrooms (int)
# bathrooms (num)
# sqft_living (int)
# sqft_lot (int)
# floors (num)
# waterfront (int)
# view (int)
# condition (int)
# grade (int)
# sqft_above (int)
# sqft_basement (int)
# yr_built (int)
# yr_renovated (int)
# zipcode (int)
# lat (num)
# long (num)
# sqft_living15 (int)
# sqft_lot15 (int)


# Variablenbeschreibung:
numerics <- data %>% select_if(is.numeric)
summary(numerics)


# Manipulation der Daten
data$waterfront <- factor(data$waterfront, labels = c("No", "Yes"))
# data$date <- as.POSIXct(data$date, format = "%Y%M%dT%H%M%S")
data <-  data %>% 
  mutate(year = as.factor(year(date)),
         view = as.factor(view)) 

summary(data)                         
sd(data$price)
mean(data$price)

# 2. Explorative Datenanalyse: -----
  
# 2.1 Deskriptive Statistik: 
# Berechnet zentrale Tendenzen und Streuungsmaße für relevante Variablen.
# 2.2 Grafische Darstellung: 
# Erstellt geeignete Diagramme (Histogramme, Boxplots, Streudiagramme), 
# um die Verteilung und Zusammenhänge der Variablen zu visualisieren.


# 2.1. Berechnung der deskriptiven Statistiken für alle numerischen Variablen ----
numerics <- data %>% select(where(is.numeric))
numeric_stats <- data.frame(
  Variable = colnames(numerics),
  Mittelwert = sapply(numerics, mean, na.rm = TRUE),
  Median = sapply(numerics, median, na.rm = TRUE),
  Minimum = sapply(numerics, min, na.rm = TRUE),
  Maximum = sapply(numerics, max, na.rm = TRUE),
  Standardabweichung = sapply(numerics, sd, na.rm = TRUE)
)

# Hinzufügen kurzer Beschreibungen (optional)
numeric_stats$Beschreibung <- c(
  "Preis der Immobilie",
  "Anzahl der Schlafzimmer",
  "Anzahl der Badezimmer",
  "Wohnfläche in Quadratfuß",
  "Grundstücksfläche in Quadratfuß",
  "Anzahl der Stockwerke",
  "Wohnfläche über der Erde",
  "Wohnfläche im Keller",
  "Baujahr der Immobilie",
  "Jahr der Renovierung",
  "Breitengrad",
  "Längengrad",
  "Wohnfläche der 15 nächsten Nachbarhäuser",
  "Grundstücksfläche der 15 nächsten Nachbarhäuser"
)[1:nrow(numeric_stats)]

# Ausgabe der Statistiken mit Beschreibungen
print(numeric_stats)

categoricals <- data %>% select_if(is.factor)
summary(categoricals)

# 2.2. Grafische Darstellung ----
# Korrelationsmatrix  
cor_matrix <- cor(numerics)
cor_matrix
# Heatmap der Korrelationsmatrix 
library(reshape2)
ggplot(data = melt(cor_matrix), aes(x = Var1, y = Var2, fill = value)) +
  geom_tile() +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", midpoint = 0) +
  theme_minimal() +
  ggtitle("Korrelationsmatrix der numerischen Variablen")

# Barplot: Anzahl der Häuser mit oder ohne Wasserblick
ggplot(data, aes(x = waterfront, fill = waterfront)) +
  geom_bar() +
  ggtitle("Anzahl der Häuser mit oder ohne Wasserblick") +
  xlab("Wasserblick") +
  ylab("Anzahl")

ggplot(data, aes(x = yr_built, fill = year)) +
  geom_bar() +
  ggtitle("Anzahl der Häuser nach Baujahr") +
  xlab("Baujahr") +
  ylab("Anzahl")

# Histogramm für den Verkaufspreis
# base R plot
hist(data$price, breaks = 30, col = "blue", xlab = "Preis" ,freq = FALSE, ylab = "Häufigkeit",
     main = "Histogramm des Verkaufspreises")

# ggplot2 Grafik
ggplot(data, aes(x = price)) +
  geom_histogram(bins = 30, fill = "blue", color = "black", ) +
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


# 3. Wahrscheinlichkeitstheorie anwenden ----
  
# 3.1  Verteilungsanpassung: 
# Überprüft, ob eine ausgewählte numerische Variable einer bestimmten 
# theoretischen Verteilung folgt (z. B. Normalverteilung) mittels QQ-Plot 
# und goodness-of-fit Tests.
# 3.2. Wahrscheinlichkeitsberechnungen: 
# Berechnet Wahrscheinlichkeiten für bestimmte Ereignisse, 
# basierend auf der angenommenen Verteilung.

# Verteilungsanpassung für den Verkaufspreis
ggplot(data, aes(sample = price)) +
  stat_qq() +
  stat_qq_line() +
  ggtitle("QQ-Plot für den Verkaufspreis")


# 3.1.a Kolmogorov-Smirnov Test -----
ks_test <- ks.test(data$price, "pnorm", mean = mean(data$price), sd = sd(data$price))
print(ks_test)
# 3.1.b Anderson-Darling-Test für Normalverteilung. -----
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

# Mittelwert und Standardabweichung der Preise
mean_price <- mean(data$price)
std_dev_price <- sd(data$price)

# Wahrscheinlichkeit, dass Preis > 500000 
# relative Häufigkeit für Preis > 500000 also unsere emp. Wahrscheinlichkeit
length(data$price[data$price > 500000]) / length(data$price)
# wenn wir annehmen, dass die Daten der Normalverteilung folgen
prob_price_greater_500k <- 1 - pnorm(500000, mean = mean_price, sd = std_dev_price)
prob_price_greater_500k

# Erstellen einer emprischen Verteilungsfunktion
options(scipen=99999) # 
plot(ecdf(data$price), xlim = c(-50000, 2000000),
     main = "Empirische Verteilungsfunktion des Verkaufspreises")
curve(pnorm(x,mean = mean_price, sd = std_dev_price ), add = T , col = "red" , lwd = 2 , lty = 2) #  pnorm ist - Wahrscheinlichkeit 

# noch einmal die 
plot(ecdf(data$price), xlim = c(-50000, 2000000),
     main = "Empirische Verteilungsfunktion des Verkaufspreises",
     xaxt = "n", yaxt = "n") # Verhindert das automatische Zeichnen der Achsenticks
# Hinzufügen der Normalverteilungskurve
curve(pnorm(x, mean = mean_price, sd = std_dev_price), 
      add = TRUE, col = "red", lwd = 2, lty = 2)

# Achsenticks und Beschriftungen formatieren
axis(1, at = pretty(data$price), 
     labels = format(pretty(data$price), big.mark = ".", decimal.mark = ",", scientific = FALSE))
axis(2, at = pretty(ecdf(data$price)(data$price)), 
     labels = format(pretty(ecdf(data$price)(data$price)), big.mark = ".", decimal.mark = ",", scientific = FALSE))
# Optional: Rahmen hinzufügen
box()

# 
hist(data$price, breaks = 40, col = "blue", xlab = "Preis", freq = FALSE, ylab = "Dichte",
     main = "Histogramm des Verkaufspreises", xlim = c(-500000, 2000000))
lines(density(data$price), col = 3, lwd = 2)
lines(density(rnorm(1000, mean = mean_price, sd = std_dev_price)), col= 2 , lwd = 2) # dichte der Normalverteilung aus den zufälligen Zahlen 
# mit density bekomnme ich eine durchgezogene Linie
lines(seq(-500000, 2000000, by = 1),dnorm(seq(-500000, 2000000, by = 1), mean = mean_price, sd = std_dev_price), col= 4 , lwd = 2)


# 3.3 Maximum-Likelihood-Methode (MLE). ------
# Beispiel: Schätzung für Normalverteilung
log_likelihood <- function(params) {
  mu <- params[1]
  sigma <- params[2]
  -sum(dnorm(data$price, mean = mu, sd = sigma, log = TRUE))
}

mle_result <- optim(par = c(mean(data$price), sd(data$price)), fn = log_likelihood, method = "L-BFGS-B", lower = c(-Inf, 0))
cat("MLE-Schätzer für den Mittelwert:", mle_result$par[1], "\n")
cat("MLE-Schätzer für die Standardabweichung:", mle_result$par[2], "\n")

# 3.4. Mittlerer quadratischer Fehler (MSE) und Wurzel des MSE -----
model <- lm(price ~ bedrooms , data = data) 
predicted <- predict(model, data)
mse <- mean((data$price - predicted)^2)
root_mse <- sqrt(mse)
cat("Wurzel des mittlerer quadratischer Fehler (RMSE):", root_mse, "\n")

# 4. Konfidenzintervalle
# 4.1 Konfidenzintervall für den Mittelwert des Verkaufspreises
mean_price <- mean(data$price)
std_error <- sd(data$price) / sqrt(nrow(data))
conf_interval <- mean_price + c(-1, 1) * qnorm(0.975) * std_error
conf_interval 
cat("Konfidenzintervall für den Mittelwert des Preises (95%):", conf_interval, "\n")

vec <- length(1000)
for (i in 1:1000){
  print(paste0(i, "--- reputation"))
  vec[i] <- ( mean(sample(data$price,size = 10000, replace = FALSE)))
}

# selbes beispiel aber mit t-Verteilung, da wir die Varianz nicht kennen
# Verteilung wird als Normalverteilung vorausgesetzt #

# 4.2 Konfidenzintervall für den Anteil der Häuser mit Wasserblick
prop_waterfront <- mean(data$waterfront == 1)
std_error_prop <- sqrt(prop_waterfront * (1 - prop_waterfront) / nrow(data))
conf_interval_prop <- prop_waterfront + c(-1, 1) * qnorm(0.975) * std_error_prop
conf_interval_prop
cat("Konfidenzintervall für den Anteil der Häuser mit Wasserblick (95%):", conf_interval_prop, "\n")
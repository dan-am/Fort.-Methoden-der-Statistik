# =============================================================================
# Lösungen zu „Zusatzaufgaben Teil 1: Konfidenzintervalle und Tests“
# (Konfidenzintervalle und statistische Tests, Kapitel 4 bis 6)
# Quelle: Datei „R-Code Aufgaben Teil 1“ (00 Tag 1.R) im LMS, Präsenztag 2023
# Überarbeitet am 21.11. und 23.11.2026; Änderungen gegenüber dem LMS stehen in lms/AENDERUNGEN.md
# Daten: explosionen.csv, Sonntagsfrage.csv, market_size.csv, market_share.csv
# Benötigt das Paket readr.
# =============================================================================

if (!"readr" %in% rownames(installed.packages())) {
  install.packages("readr", repos = "https://cloud.r-project.org")
}
library(readr)

# Die Datendateien liegen im Arbeitsverzeichnis (Download aus dem LMS)
# oder im Projektordner unter data/input/
pfad <- function(datei) if (file.exists(datei)) datei else file.path("data", "input", datei)


# =============================================================================
# Aufgaben zum Konfidenzintervall
# =============================================================================

# -----------------------------------------------------------------------------
# Aufgabe 1: Zeitintervalle zwischen Explosionen, Exponentialverteilung
# -----------------------------------------------------------------------------
expl <- read_csv(pfad("explosionen.csv"), show_col_types = FALSE)
expl <- expl$explosions          # 1.329 Zeitintervalle in Jahren

alpha <- 0.05
n <- length(expl)
lambda_hat <- 1 / mean(expl)     # ML-Schätzer für lambda
z <- qnorm(1 - alpha / 2)

# Asymptotisches 95-%-Konfidenzintervall: 1/x_quer * (1 -/+ z / sqrt(n))
KI <- c(untere_grenze = lambda_hat * (1 - z / sqrt(n)),
        obere_grenze  = lambda_hat * (1 + z / sqrt(n)))
lambda_hat
KI

# Simulationsstudie: Wie oft liegt das wahre lambda außerhalb des Intervalls?
# Erwartet wird ein Anteil von etwa alpha = 5 %.
# N = 10^5 Wiederholungen mit je n = 100 Beobachtungen, Startwert 5513.
N <- 1e5
n <- 100
lambda <- lambda_hat             # als „wahrer“ Parameter dient der Schätzwert von oben
set.seed(5513)
ausserhalb <- logical(N)
for (i in seq_len(N)) {
  x <- rexp(n, rate = lambda)                     # simulierte Stichprobe
  lambda_hat_2 <- 1 / mean(x)                     # Schätzung
  u <- lambda_hat_2 * (1 - z / sqrt(n))           # untere Grenze
  o <- lambda_hat_2 * (1 + z / sqrt(n))           # obere Grenze
  ausserhalb[i] <- (lambda < u) | (lambda > o)    # TRUE: lambda liegt außerhalb
}
mean(ausserhalb)                 # nahe 0,05


# =============================================================================
# Aufgaben zum statistischen Test (Kapitel 5 und 6)
# =============================================================================

# -----------------------------------------------------------------------------
# Aufgabe 1: Sonntagsfrage
# -----------------------------------------------------------------------------
sonntagsfrage <- read_csv(pfad("Sonntagsfrage.csv"), show_col_types = FALSE)
sum(is.na(sonntagsfrage$Wahl))   # fehlende Angaben?

# a) Weicht der Anteil der CDU/CSU signifikant von 24,1 % ab?
#    H0: p = 0,241 gegen H1: p != 0,241
cdu <- sonntagsfrage$Wahl == "CDU/CSU"
prop.test(sum(cdu), n = length(cdu), p = 0.241, alternative = "two.sided")  # approximativ
binom.test(sum(cdu), n = length(cdu), p = 0.241)                           # exakt

# b) Unterscheidet sich das Alter von CDU/CSU-Wählern und übrigen Wählern?
#    H0: mu_CDU = mu_Rest gegen H1: mu_CDU != mu_Rest (unabhängige Stichproben)
alter_cdu  <- sonntagsfrage$Alter[cdu]
alter_rest <- sonntagsfrage$Alter[!cdu]
mean(alter_cdu)
mean(alter_rest)
var.test(alter_cdu, alter_rest)                       # gleiche Varianzen?
t.test(alter_cdu, alter_rest, var.equal = FALSE)      # Welch-Test


# -----------------------------------------------------------------------------
# Aufgabe 2: Stromerzeugung 1999 und 2010
# -----------------------------------------------------------------------------
# Hinweis zu den Daten: 28 europäische Länder, darunter Norwegen und die Türkei;
# Bulgarien und die Niederlande fehlen.
market_size <- read_csv(pfad("market_size.csv"),
                        locale = locale(encoding = "ISO-8859-1"), show_col_types = FALSE)
market_size <- market_size[order(market_size$GEO, market_size$TIME), ]

x1999 <- market_size$Value[market_size$TIME == 1999]
x2010 <- market_size$Value[market_size$TIME == 2010]

# Dieselben Länder zu zwei Zeitpunkten: verbundene Stichproben
# H0: mu_d = 0 gegen H1: mu_d != 0 mit d = 2010 - 1999
t.test(x2010, x1999, paired = TRUE, alternative = "two.sided")


# -----------------------------------------------------------------------------
# Aufgabe 3: Anteil des größten Stromerzeugers 2010
# -----------------------------------------------------------------------------
# Jede GWh gilt als unabhängiger Versuch: „stammt vom Marktführer“ ja oder nein.
# H0: p <= 0,43 gegen H1: p > 0,43
market_share <- read_csv(pfad("market_share.csv"),
                         locale = locale(encoding = "ISO-8859-1"), show_col_types = FALSE)

daten_2010 <- merge(market_size[market_size$TIME == 2010, c("GEO", "Value")],
                    market_share[market_share$TIME == 2010, c("GEO", "Value")],
                    by = "GEO", suffixes = c("_GWh", "_Anteil"))

# Für Österreich und die Türkei fehlt 2010 der Marktanteil; sie fließen
# weder in die Menge des Marktführers noch in die Gesamtmenge ein.
daten_2010 <- daten_2010[!is.na(daten_2010$Value_Anteil), ]

menge_marktfuehrer <- sum(daten_2010$Value_Anteil / 100 * daten_2010$Value_GWh)
menge_gesamt       <- sum(daten_2010$Value_GWh)
menge_marktfuehrer / menge_gesamt          # geschätzter Anteil

prop.test(round(menge_marktfuehrer), n = menge_gesamt, p = 0.43, alternative = "greater")


# -----------------------------------------------------------------------------
# Die bisherigen Aufgaben 4 bis 6 (Korrelationstest, Chi-Quadrat-Test,
# Spearman-Test) stehen seit November 2026 als Übung 9 in Kapitel 7.
# Ihre Lösung steht dort auf der Seite „Lösung Übung 9“.
# -----------------------------------------------------------------------------

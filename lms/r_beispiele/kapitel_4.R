# =============================================================================
# R-Beispiel 18 zu Kapitel 4: Konfidenzintervalle
# Quelle: Textseite „R Beispiel 18 - Kapitel 4.3“ im LMS (bis 21.11.2026 „R Beispiel 18-19“).
# Beispiel 19 behandelt Tests und steht in lms/r_beispiele/kapitel_5.R.
# Überarbeitet am 21.11.2026; Änderungen gegenüber dem LMS stehen in lms/AENDERUNGEN.md
# Benötigt nur Base R.
# =============================================================================


# -----------------------------------------------------------------------------
# Beispiel 18 (Kap. 4.3): Asymptotisches Konfidenzintervall für einen Anteil p
# -----------------------------------------------------------------------------
# Für wachsende Stichprobenumfänge wird jeweils eine Stichprobe mit wahrem
# Anteil p = 0,3 gezogen und das 95-%-Konfidenzintervall berechnet:
#   p_dach +/- z_(1 - alpha/2) * sqrt(p_dach * (1 - p_dach) / n)
# Das Intervall wird mit wachsendem n schmaler.
set.seed(123)
alpha <- 0.05
p_wahr <- 0.3
z_alpha <- qnorm(1 - alpha / 2)               # 1 - alpha/2-Quantil der N(0, 1)

n_vec <- seq(from = 10, to = 500, by = 10)    # Stichprobengrößen
untere_grenze <- numeric(length(n_vec))
obere_grenze  <- numeric(length(n_vec))

for (i in seq_along(n_vec)) {
  stichprobe <- rbinom(n_vec[i], size = 1, prob = p_wahr)
  p_dach <- mean(stichprobe)                            # Schätzer für p
  se <- sqrt(p_dach * (1 - p_dach) / n_vec[i])          # Standardfehler
  untere_grenze[i] <- p_dach - z_alpha * se
  obere_grenze[i]  <- p_dach + z_alpha * se
}

plot(n_vec, obere_grenze, type = "l", ylim = c(0, 1), col = "blue",
     main = "Asymptotisches 95-%-Konfidenzintervall für p",
     xlab = "Stichprobengröße n", ylab = "Konfidenzintervall für p")
lines(n_vec, untere_grenze, col = "red")
abline(h = p_wahr, lty = 2)
legend("topright", legend = c("obere Grenze", "untere Grenze", "wahrer Wert p = 0,3"),
       col = c("blue", "red", "black"), lty = c(1, 1, 2), cex = 0.8, bg = "white")

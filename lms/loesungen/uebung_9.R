# =============================================================================
# Lösung zu Übung 9 (Kapitel 7: Tests für Zusammenhangsmaße)
# Quelle: Aufgabe „Übung 9: Tests für Zusammenhangsmaße“ und Seite „Lösung Übung 9“ im LMS,
# seit 22.11.2026 in Kapitel 7 (vorher Zusatzaufgaben Teil 1, Aufgaben 4 bis 6)
# Benötigt nur Base R.
# =============================================================================

# -----------------------------------------------------------------------------
# Aufgabe 1 (7.1): Alter und Blutdruck, Test auf lineare Korrelation
# -----------------------------------------------------------------------------
X <- c(47, 52, 30, 35, 59, 44, 63, 38, 49, 41, 32, 55, 46, 51, 63)
Y <- c(129, 139, 112, 119, 145, 133, 152, 117, 145, 136, 115, 137, 134, 141, 157)

# H0: rho = 0 gegen H1: rho != 0, alpha = 0,05
cor.test(X, Y, method = "pearson", conf.level = 0.95)
# Teststatistik t = r * sqrt(n - 2) / sqrt(1 - r^2), unter H0 t-verteilt mit n - 2 Freiheitsgraden


# -----------------------------------------------------------------------------
# Aufgabe 2 (7.3): Längere Pause und Geschlecht, Chi-Quadrat-Test auf Unabhängigkeit
# -----------------------------------------------------------------------------
antworten <- matrix(c(85, 41, 74,
                      150, 52, 98), nrow = 2, byrow = TRUE,
                    dimnames = list(Geschlecht = c("weiblich", "männlich"),
                                    Antwort = c("positiv", "unentschieden", "negativ")))
antworten
# H0: Geschlecht und Antwort sind unabhängig, alpha = 0,05
test <- chisq.test(antworten)
test
test$expected            # erwartete Häufigkeiten unter H0, alle deutlich über 5
qchisq(0.95, df = 2)     # kritischer Wert 5,99


# -----------------------------------------------------------------------------
# Aufgabe 3 (7.2): Radwege und Radanteil, Spearman-Rangkorrelation
# -----------------------------------------------------------------------------
# Gegeben ist nur der Rangkorrelationskoeffizient R = 0,89 aus n = 17 Städten.
# H0: R <= 0 gegen H1: R > 0 (monotoner positiver Zusammenhang), alpha = 0,01
# Näherung: T = R * sqrt(n - 1) ist unter H0 etwa N(0, 1)-verteilt.
R <- 0.89
n <- 17
T_stat <- R * sqrt(n - 1)
c(T = T_stat, kritischer_Wert = qnorm(0.99), p_Wert = 1 - pnorm(T_stat))

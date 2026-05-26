# Agenda: Fortgeschrittene Methoden der Statistik

Überblick über die Kerninhalte aller Kapitel der Vorlesung.

---

## Kapitel 1: Einführung und Wiederholung

- Ziele und Aufbau der Veranstaltung
- Wiederholung deskriptive Statistik: Lage- und Streuungsmaße, Quantile, Boxplot, Histogramm
- Wiederholung Wahrscheinlichkeitsrechnung: Zufallsvariable, Verteilungsfunktion, Dichte, Erwartungswert, Varianz
- Wichtige Verteilungen: Binomial-, Poisson-, Exponential-, Normalverteilung
- Einstieg in R und RStudio: Datenobjekte, Zuweisung, Vektoren, data.frame, Plot-Grundlagen

---

## Kapitel 2: Ergänzungen zur Wahrscheinlichkeitstheorie

### 2.1 Zweidimensionale Verteilungsfunktionen
- Gemeinsame Verteilung, Randverteilung, bedingte Verteilung
- Diskreter und stetiger Fall, gemeinsame Dichte
- Zweidimensionale Normalverteilung, Bedeutung der Korrelation rho

### 2.2 Ungleichung von Tschebyscheff
- Untere Schranke für P(|X - mu| <= epsilon)
- Verbindung zwischen Varianz und Konzentration um den Erwartungswert

### 2.3 Grenzwertsätze
- Schwaches Gesetz der großen Zahlen
- Empirische Verteilungsfunktion und stochastische Konvergenz
- Zentraler Grenzwertsatz: asymptotische Normalverteilung des Mittelwertes
- Konsequenzen für Inferenz und Konfidenzintervalle

---

## Kapitel 3: Induktive Statistik

### 3.1 Einführung
- Stichprobe vs. Grundgesamtheit
- Ziel: Aussagen über die unbekannte Verteilungsfunktion F

### 3.2 Parameterschätzung
- Schätzer, Schätzung, Schätzfunktion
- Mittelwert als Schätzer für mu, empirische Varianz als Schätzer für sigma^2
- Empirische Verteilungsfunktion und Histogramm

### 3.3 Maximum-Likelihood-Methode
- Likelihood-Funktion, Log-Likelihood
- Vorgehensweise: Likelihood aufstellen, Logarithmieren, Ableiten, Nullsetzen
- Beispiele: Bernoulli, Exponentialverteilung, Normalverteilung

### 3.4 Methode der kleinsten Fehlerquadrate (OLS)
- Lineare Einfachregression Y = beta_0 + beta_1 X + eps
- Herleitung der Schätzer beta_0 und beta_1
- Regressionsgerade durch (x_bar, y_bar)

### 3.5 Anforderungen an Schätzfunktionen
- Erwartungstreue (Unverzerrtheit) und Bias
- Mittlerer quadratischer Fehler (MSE) = Varianz + Bias^2
- Schwache Konsistenz

---

## Kapitel 4: Konfidenzintervalle

### 4.1 Einführung
- Begriff Konfidenzintervall, Konfidenzniveau 1 - alpha
- Interpretation als Überdeckungswahrscheinlichkeit

### 4.2 Konfidenzintervalle bei bekannter Verteilung
- KI für mu bei bekanntem sigma (z-Quantil)
- KI für mu bei unbekanntem sigma (t-Quantil)
- KI für sigma^2 (Chi-Quadrat-Verteilung)

### 4.3 Konfidenzintervalle bei unbekannter Verteilung
- Asymptotisches KI über den zentralen Grenzwertsatz
- Faustregel n >= 30
- Einflussfaktoren: Konfidenzniveau, Stichprobenumfang, Streuung
- Bestimmung des notwendigen Stichprobenumfangs

---

## Kapitel 5: Einstichproben-Hypothesentests

- Hypothesenformulierung: H0 vs. H1, einseitig und zweiseitig
- Teststatistik, Annahme- und Ablehnungsbereich, kritische Werte
- p-Wert und Testentscheidung
- Fehler 1. Art (alpha) und Fehler 2. Art (beta), Power
- Einstichproben-t-Test (Mittelwert)
- Gepaarter t-Test (Differenzen)
- Tests auf Verteilungsannahme: QQ-Plot, Shapiro-Wilk, Kolmogorov-Smirnov
- Nichtparametrische Variante: Wilcoxon-Vorzeichen-Rang-Test
- Tests für Anteile: Binomialtest, prop.test
- Stichprobenumfangsplanung mit power.t.test

---

## Kapitel 6: Zweistichprobentests und Mehrgruppenvergleiche

- Vergleich zweier Mittelwerte
  - t-Test für unabhängige Stichproben (gleiche Varianzen)
  - Welch-Test (ungleiche Varianzen)
  - F-Test auf Gleichheit der Varianzen
- Nichtparametrische Alternative: Mann-Whitney-U-Test (Wilcoxon-Rangsummentest)
- Vergleich zweier Anteile (prop.test)
- Mehr als zwei Gruppen
  - Einfaktorielle Varianzanalyse (ANOVA)
  - Levene-Test auf Varianzhomogenität
  - Post-hoc-Test: Tukey HSD
  - Nichtparametrisch: Kruskal-Wallis-Test
- Bootstrap-Konfidenzintervalle und Permutationstests als verteilungsfreie Methoden

---

## Kapitel 7: Tests für Zusammenhangsmaße

- Korrelationskoeffizienten
  - Pearson (linearer Zusammenhang, metrische Daten)
  - Spearman (monoton, Ränge)
  - Kendalls Tau
- Signifikanztest für eine Korrelation (cor.test)
- Konfidenzintervall für rho über die Fisher-z-Transformation
- Partielle Korrelation
- Kontingenztabellen und Tests auf Unabhängigkeit
  - Chi-Quadrat-Test
  - Fisher's exakter Test (kleine Stichproben)
  - McNemar-Test (gepaarte Anteile, Vorher/Nachher)
- Effektstärken: Cramers V, Kontingenzkoeffizient
- Visualisierung: Mosaikplot, Korrelationsmatrix
- Simpson-Paradoxon

---

## Kapitel 8: Lineare Regression

- Einfache lineare Regression
  - Modell, OLS-Schätzung, Interpretation der Koeffizienten
  - Bestimmtheitsmaß R^2, F-Statistik, t-Tests der Koeffizienten
- Multiple lineare Regression
  - Mehrere Regressoren, adjustiertes R^2
  - Variablenselektion mit AIC (step, vorwärts/rückwärts)
- Kategoriale Variablen
  - Dummy-Codierung über factor()
  - Interaktionseffekte (Wechselwirkungen)
- Modellannahmen und Diagnostik
  - Linearität, Homoskedastizität, Normalverteilung der Residuen, Unabhängigkeit
  - Residuenanalyse, QQ-Plot der Residuen
- Multikollinearität: Variance Inflation Factor (VIF)
- Vorhersage
  - Konfidenzintervall für den Mittelwert E(Y|x)
  - Vorhersageintervall für eine neue Beobachtung
- Modellvergleich und Validierung
  - anova() für geschachtelte Modelle
  - Train-/Test-Split, k-fache Kreuzvalidierung
  - Gütemaße: RMSE, MAE
- Modellerweiterungen
  - Polynomiale Regression
  - Log-Transformation der Zielgröße bei Schiefe oder Heteroskedastizität

---

## R-Werkzeuge im Überblick

- Basis-Funktionen: `mean`, `var`, `sd`, `cor`, `lm`, `aov`, `t.test`, `wilcox.test`, `chisq.test`, `prop.test`, `binom.test`, `fisher.test`, `mcnemar.test`, `shapiro.test`, `ks.test`, `power.t.test`
- Pakete: `MASS` (mvrnorm), `car` (leveneTest, vif), `ggplot2`, `plotly`
- Beispiel-Datensätze: `iris`, `mtcars`, `cars`, `sleep`, `PlantGrowth`, `ChickWeight`, `airquality`, `UCBAdmissions`
- Übungsmaterial: `uebungen/uebung_kapitel_2.R` bis `uebung_kapitel_8.R` sowie die Zusatzdateien `*_zusatz.R`

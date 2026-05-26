# Fortgeschrittene Methoden der Statistik

Begleitmaterial zur Vorlesung „Fortgeschrittene Methoden der Statistik" an der Digital Business University (DBU). Enthalten sind die Vorlesungsfolien (PDF), Code-Beispiele aus der Vorlesung, eigene Übungsaufgaben mit Lösungsskizze und ein Datensatz für angewandte Analysen.

---

## Inhalt der Vorlesung

Eine ausführliche Agenda mit den Lernzielen aller Kapitel steht in [`AGENDA.md`](AGENDA.md).

Kurzüberblick:

1. **Kapitel 1** Einführung und Wiederholung (deskriptive Statistik, Wahrscheinlichkeit, R-Grundlagen)
2. **Kapitel 2** Ergänzungen zur Wahrscheinlichkeitstheorie (2-dim Verteilungen, Tschebyscheff, Grenzwertsätze)
3. **Kapitel 3** Induktive Statistik (Parameterschätzung, Maximum Likelihood, OLS)
4. **Kapitel 4** Konfidenzintervalle
5. **Kapitel 5** Einstichproben-Hypothesentests
6. **Kapitel 6** Zweistichprobentests und Mehrgruppenvergleich (ANOVA)
7. **Kapitel 7** Tests für Zusammenhangsmaße
8. **Kapitel 8** Lineare Regression

---

## Repository-Struktur

```
Fort.-Methoden-der-Statistik/
├── AGENDA.md                  # Agenda mit Kerninhalt aller Kapitel
├── README.md                  # diese Datei
├── Fort.-Methoden-der-Statistik.Rproj
├── data/                      # Datensätze (kc_house_data etc.)
│   └── input/
├── functions/                 # eigene Hilfsfunktionen / Paket-Loader
│   ├── library.R
│   └── packages.R
├── lecture_pdf/               # Vorlesungsfolien als PDF (Kap. 2-4)
├── lecture_example/           # R-Beispiele aus der Vorlesung
│   ├── prelude_chapter_1_2024.R
│   ├── prelude_chapter_2_2024.R
│   ├── chapter_3_4_2024.R
│   ├── chapter_5_2024.R
│   ├── chapter_6_7_8_2024.R
│   └── chapter_8_predict_2024.R
└── uebungen/                  # eigene Übungsaufgaben mit Lösungsskizze
    ├── uebung_kapitel_2.R
    ├── uebung_kapitel_3.R
    ├── uebung_kapitel_4.R
    ├── uebung_kapitel_5.R
    ├── uebung_kapitel_5_zusatz.R
    ├── uebung_kapitel_6.R
    ├── uebung_kapitel_6_zusatz.R
    ├── uebung_kapitel_7.R
    ├── uebung_kapitel_7_zusatz.R
    ├── uebung_kapitel_8.R
    └── uebung_kapitel_8_zusatz.R
```

---

## Übungsaufgaben

Pro Kapitel gibt es eine Hauptübung; für die Kapitel 5–8 zusätzlich eine Zusatzdatei (`*_zusatz.R`) mit Verfahren, die im Vorlesungstext besprochen, aber dort nicht als Beispiel gezeigt wurden.

| Kapitel | Hauptübung | Zusatz | Schwerpunkte |
|---|---|---|---|
| 2 | `uebung_kapitel_2.R` | – | 2-dim Verteilung, Tschebyscheff, GGZ, ZG |
| 3 | `uebung_kapitel_3.R` | – | Parameterschätzung, ML, OLS, MSE, Konsistenz |
| 4 | `uebung_kapitel_4.R` | – | KI für mu, sigma^2, Anteile, Überdeckung |
| 5 | `uebung_kapitel_5.R` | `uebung_kapitel_5_zusatz.R` | t-Test, Wilcoxon, Binomial, Power, Fehler 1./2. Art |
| 6 | `uebung_kapitel_6.R` | `uebung_kapitel_6_zusatz.R` | t/Welch, Mann-Whitney, ANOVA, Tukey, Bootstrap, Permutation |
| 7 | `uebung_kapitel_7.R` | `uebung_kapitel_7_zusatz.R` | Pearson/Spearman/Kendall, Chi^2, Fisher, McNemar, Cramers V |
| 8 | `uebung_kapitel_8.R` | `uebung_kapitel_8_zusatz.R` | Einfache/multiple Regression, Polynom, Interaktion, VIF, CV |

Jede Datei enthält Aufgabenstellung als Kommentar und direkt darunter eine ausführbare Lösungsskizze.

---

## Voraussetzungen

- R (>= 4.0)
- RStudio (empfohlen)
- Pakete (werden bei Bedarf installiert):
  - `MASS`, `car`, `ggplot2`, `plotly`

Installation aller in den Übungen genutzten Pakete:

```r
install.packages(c("MASS", "car", "ggplot2", "plotly"), dependencies = TRUE)
```

---

## Datensatz: King County House Sales

Für angewandte Beispiele (Regression, Korrelation, Hypothesentests) wird der Kaggle-Datensatz **kc_house_data** verwendet:
<https://www.kaggle.com/datasets/shivachandel/kc-house-data>

### Variablenübersicht

| Variable | Typ | Beschreibung |
|---|---|---|
| `id` | Integer | Eindeutige Identifikation jeder Beobachtung |
| `date` | String | Datum des Verkaufs |
| `price` | Float | Verkaufspreis der Immobilie |
| `bedrooms` | Integer | Anzahl der Schlafzimmer |
| `bathrooms` | Float | Anzahl der Badezimmer |
| `sqft_living` | Integer | Wohnfläche in Quadratfuß |
| `sqft_lot` | Integer | Grundstücksfläche in Quadratfuß |
| `floors` | Float | Anzahl der Stockwerke |
| `waterfront` | Integer | 0 = kein Wasserblick, 1 = Wasserblick |
| `view` | Integer | Index für den Ausblick (0–4) |
| `condition` | Integer | Zustand (1 = schlecht bis 5 = ausgezeichnet) |
| `grade` | Integer | Bauqualität und Design (1 bis 13) |
| `sqft_above` | Integer | Fläche über der Erde in Quadratfuß |
| `sqft_basement` | Integer | Fläche des Kellers in Quadratfuß |
| `yr_built` | Integer | Baujahr |
| `yr_renovated` | Integer | Jahr der Renovierung (0 = nie renoviert) |
| `zipcode` | Integer | Postleitzahl |
| `lat` | Float | Breitengrad |
| `long` | Float | Längengrad |
| `sqft_living15` | Integer | Durchschnittliche Wohnfläche der 15 nächsten Häuser |
| `sqft_lot15` | Integer | Durchschnittliche Grundstücksfläche der 15 nächsten Häuser |

### Weitere genutzte Datensätze

In den Übungen werden zusätzlich Datensätze aus R-Base bzw. mitgelieferten Paketen verwendet, damit die Aufgaben ohne Download nachvollziehbar sind: `iris`, `mtcars`, `cars`, `sleep`, `PlantGrowth`, `ChickWeight`, `airquality`, `UCBAdmissions`.

---

## Nutzung

1. Repository klonen oder herunterladen.
2. RStudio-Projekt `Fort.-Methoden-der-Statistik.Rproj` öffnen.
3. Eine Übungsdatei aus `uebungen/` öffnen und blockweise ausführen.
4. Bei Bedarf die Vorlesungsfolien in `lecture_pdf/` und die Code-Beispiele in `lecture_example/` zur Vertiefung heranziehen.

---

## Lizenz und Kontakt

Lehrmaterial für die Veranstaltung an der DBU.
Kontakt: Prof. Dr. Daniel Ambach.

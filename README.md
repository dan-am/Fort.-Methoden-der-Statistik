# Fortgeschrittene Methoden der Statistik

Begleitmaterial zur Vorlesung „Fortgeschrittene Methoden der Statistik“ (FMS01) an der Digital Business University (DBU). Enthalten sind der R-Code aus dem LMS-Kurs (R-Beispiele und Lösungen), die Code-Beispiele der Präsenzveranstaltung, eigene Übungsaufgaben mit Lösungsskizze, die Datensätze und die Vorlesungsfolien (PDF).

## Inhalt der Vorlesung

Die Kapitel folgen dem LMS-Kurs. Eine ausführliche Agenda mit den Lernzielen steht in [`AGENDA.md`](AGENDA.md).

1. **Kapitel 1** Wahrscheinlichkeitstheorie
2. **Kapitel 2** Ergänzungen zur Wahrscheinlichkeitstheorie (zweidimensionale Verteilungen, Tschebyscheff, Grenzwertsätze)
3. **Kapitel 3** Induktive Statistik (Parameterschätzung, Maximum Likelihood, kleinste Quadrate)
4. **Kapitel 4** Konfidenzintervalle
5. **Kapitel 5** Statistische Tests
6. **Kapitel 6** Zweistichprobentests
7. **Kapitel 7** Tests für Zusammenhangsmaße
8. **Kapitel 8** Lineare Regression

Die Zusatzübungen in `uebungen/*_zusatz.R` gehen teilweise über den LMS-Kurs hinaus, etwa mit Varianzanalyse, Bootstrap und Kreuzvalidierung.

## Repository-Struktur

```
Fort.-Methoden-der-Statistik/
├── README.md                  # diese Datei
├── AGENDA.md                  # Agenda mit Kerninhalt aller Kapitel
├── LMS_AGENDA_DRAFT.md        # Entwurf der LMS-Agenda mit Videotexten
├── LMS_BILDPROMPTS.md         # Bildprompts für Kapitelbilder (erster Entwurf)
├── Fort.-Methoden-der-Statistik.Rproj
├── data/
│   └── input/                 # alle Datensätze, siehe unten
├── functions/
│   ├── packages.R             # installiert fehlende Pakete für lecture_example/
│   └── library.R              # lädt diese Pakete
├── lms/                       # R-Code aus dem LMS-Kurs, berichtigt
│   ├── AENDERUNGEN.md         # was gegenüber dem LMS geändert wurde
│   ├── r_beispiele/           # R-Beispiele 1 bis 32, je Kapitel eine Datei
│   │   ├── kapitel_1.R … kapitel_8.R
│   └── loesungen/             # R-Lösungen zu den LMS-Übungen
│       ├── uebung_4.R
│       ├── uebung_6.R
│       ├── uebung_8.R
│       ├── uebung_9.R
│       ├── uebung_10.R
│       ├── zusatzaufgaben_teil_1.R
│       └── zusatzaufgaben_teil_2.R
├── lecture_example/           # Code der Präsenzveranstaltung 2024 (King-County-Daten)
│   ├── prelude_chapter_1_2024.R
│   ├── prelude_chapter_2_2024.R
│   ├── chapter_3_4_2024.R
│   ├── chapter_5_2024.R
│   ├── chapter_6_7_8_2024.R
│   └── chapter_8_predict_2024.R
├── lecture_pdf/               # Vorlesungsfolien als PDF (Kapitel 2 bis 8)
└── uebungen/                  # eigene Übungsaufgaben mit Lösungsskizze
    ├── uebung_kapitel_2.R … uebung_kapitel_8.R
    └── uebung_kapitel_5_zusatz.R … uebung_kapitel_8_zusatz.R
```

## Zuordnung zum LMS-Kurs

| Im LMS | Im Repository |
|---|---|
| Textseiten „R-Beispiel 1“ bis „R-Beispiel 7“ (Kapitel 1; Beispiel 3 und 4 seit 22.11.2026 im Archiv des Kurses) | `lms/r_beispiele/kapitel_1.R` |
| „R-Beispiele 8 bis 11“, „R-Beispiel 12“, „13“ (Kapitel 2) | `lms/r_beispiele/kapitel_2.R` |
| „R-Beispiel 14“, „15“, „R-Beispiele 16 und 17“ (Kapitel 3) | `lms/r_beispiele/kapitel_3.R` |
| „R-Beispiel 18“ (Kapitel 4) | `lms/r_beispiele/kapitel_4.R` |
| „R-Beispiel 19“ und „R-Beispiel 20“ (Kapitel 5) | `lms/r_beispiele/kapitel_5.R` |
| „R-Beispiele 21 bis 24“ (Kapitel 6) | `lms/r_beispiele/kapitel_6.R` |
| „R-Beispiel 25“, „26“, „27“ (Kapitel 7) | `lms/r_beispiele/kapitel_7.R` |
| „R-Beispiel 28“ bis „R-Beispiele 31 und 32“ (Kapitel 8) | `lms/r_beispiele/kapitel_8.R` |
| Lösung Übung 4, 6, 8, 9 und 10 | `lms/loesungen/uebung_4.R`, `uebung_6.R`, `uebung_8.R`, `uebung_9.R`, `uebung_10.R` |
| R-Code zu den Zusatzaufgaben Teil 1 und 2 | `lms/loesungen/zusatzaufgaben_teil_1.R`, `zusatzaufgaben_teil_2.R` |
| Präsenztage als Zip-Archiv (2024; seit 21.11.2026 im verborgenen Archiv des Kurses) | `lecture_example/`, `functions/`, `data/input/` (berichtigt) |

Die Lösungen zu Übung 5 und Übung 7 sowie die Übungsblätter 1 bis 3 sind Rechenaufgaben ohne R-Code. Die Lösungen zu Übung 5 und 7 stehen im LMS seit 22.11.2026 als Text (vorher als Bild), die Übungsblätter mit ihren Lösungen als PDF; Blatt 1 und 3 liegen seit 22.11.2026 im verborgenen Archiv des Kurses.

## Voraussetzungen

- R (>= 4.0)
- RStudio (empfohlen)
- Jedes Skript installiert fehlende Pakete beim ersten Lauf selbst. Alle Pakete auf einmal:

```r
pakete <- c("tidyverse", "lubridate", "reshape2", "knitr", "kableExtra", "car",
            "MASS", "psych", "nortest", "moments", "ggpubr", "corrplot", "lmtest",
            "sandwich", "DataExplorer", "caret", "plotly", "mnormt", "scatterplot3d")
install.packages(setdiff(pakete, rownames(installed.packages())))
```

## Nutzung

1. Repository klonen oder herunterladen.
2. RStudio-Projekt `Fort.-Methoden-der-Statistik.Rproj` öffnen. Damit ist der Projektordner das Arbeitsverzeichnis; alle Skripte lesen ihre Daten relativ dazu aus `data/input/`. Die Lösungen in `lms/loesungen/` finden ihre Daten auch im Arbeitsverzeichnis, so wie nach einem Download aus dem LMS.
3. Eine Datei aus `lms/`, `uebungen/` oder `lecture_example/` öffnen und blockweise ausführen.
4. Bei Bedarf die Vorlesungsfolien in `lecture_pdf/` zur Vertiefung heranziehen.

## Datensätze

| Datei | Inhalt | genutzt in |
|---|---|---|
| `kc_house_data.csv` | Hausverkäufe im King County (USA), 21.613 Beobachtungen | `lecture_example/`, `lms/loesungen/uebung_10.R` |
| `Autofahrer.csv` | Reaktionszeiten telefonierender Autofahrer in ms, 1.000 Werte ohne Kopfzeile | `lms/loesungen/uebung_8.R` |
| `explosionen.csv` | 1.329 Zeitintervalle zwischen Explosionen in Bergwerken | `lms/loesungen/zusatzaufgaben_teil_1.R` |
| `Sonntagsfrage.csv` | Sonntagsfrage mit Wahlabsicht und Alter, 2.501 Befragte | `lms/loesungen/zusatzaufgaben_teil_1.R` |
| `market_size.csv`, `market_share.csv` | Stromerzeugung und Marktanteil des größten Erzeugers, 28 Länder, 1999 bis 2010 (Kodierung ISO-8859-1) | `lms/loesungen/zusatzaufgaben_teil_1.R` |
| `wine.csv`, `wine_38.csv` | Weinbewertungen, 20 bzw. 38 Weine | `lms/loesungen/zusatzaufgaben_teil_2.R` |
| `Verstecke.csv` | derzeit von keinem Skript genutzt | |

Daneben nutzen die Übungen Datensätze aus R selbst, damit die Aufgaben ohne Download nachvollziehbar sind: `iris`, `mtcars`, `cars`, `sleep`, `PlantGrowth`, `ChickWeight`, `airquality`, `HairEyeColor`, `UCBAdmissions`.

### King County House Sales

Für angewandte Beispiele (Regression, Korrelation, Hypothesentests) wird der Kaggle-Datensatz **kc_house_data** verwendet:
<https://www.kaggle.com/datasets/shivachandel/kc-house-data>

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

## Lizenz und Kontakt

Lehrmaterial für die Veranstaltung an der DBU.
Kontakt: Prof. Dr. Daniel Ambach.

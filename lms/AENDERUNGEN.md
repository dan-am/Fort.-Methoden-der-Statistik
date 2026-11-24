# Änderungen gegenüber dem LMS

Stand: 22.11.2026. Die Dateien in `lms/` enthalten den R-Code aus dem LMS-Kurs FMS01 (Kurs-ID 528), fachlich berichtigt und lauffähig gemacht. Die Nummern H1 bis H9 und M1 verweisen auf den Kurs-Review vom 21.11.2026. Alle Dateien laufen fehlerfrei mit R 4.6.1; Arbeitsverzeichnis ist der Projektordner.

Am 21.11.2026 wurde der Code in den LMS-Kurs übertragen: 23 Codeseiten und 3 Lösungsseiten als `<pre>`-Block ohne geschützte Leerzeichen (Befund M1), Beispiel 19 als neue Seite „R Beispiel 19 - Kapitel 5.1“ in Kapitel 5, die Dateien „R-Code Aufgaben Teil 1/2“ durch `zusatzaufgaben_teil_1.R` und `zusatzaufgaben_teil_2.R` ersetzt. `<code>` wird im Snap-Theme kursiv mit Zeilenhintergrund dargestellt und ist deshalb nicht verwendet.

Die Lösungen lesen ihre Daten aus dem Arbeitsverzeichnis (Download aus dem LMS) oder aus `data/input/` (Repository).

## Für alle Dateien

- Code als Datei statt als Absatztext; keine geschützten Leerzeichen mehr, an denen R beim Kopieren abbricht.
- Pakete werden nur installiert, wenn sie fehlen; kein `install.packages()` mitten im Code, kein `View()` und kein `?`.
- Keine Semikolons am Zeilenende, keine Variablen mit den Namen von R-Funktionen (`data`, `density`, `mean`, `sd`, `diff`, `t`, `pi`).
- Kommentare in der Anrede „du“ statt „Sie“, Tippfehler berichtigt, Achsen deutsch beschriftet.

## R-Beispiele (`lms/r_beispiele/`)

Seit 22.11.2026 (Paket 5) heißen die Seiten im LMS „R-Beispiel N (Kapitel x.y)“, bei mehreren Beispielen „R-Beispiele 8 bis 11 (Kapitel 2.1)“; die Tabelle nennt die Nummern. Die Dateien zu den Zusatzaufgaben heißen „R-Code zu den Zusatzaufgaben Teil 1“ und „Teil 2“.

| Datei | LMS-Seiten | Änderungen |
|---|---|---|
| `kapitel_1.R` | R Beispiel 1 bis 7 | Bsp. 3 (H6): Kommentar „2 Kugeln“ an den Code (4 Kugeln) angeglichen, Verteilungsfunktion als Treppe statt als Stäbe, Wahrscheinlichkeitsfunktion nur für 0 bis n. Bsp. 5: Startwert gesetzt, y-Achse „Dichte“ statt „Wahrscheinlichkeit“. Bsp. 6 (H7): Titel zeigte keine Werte, Legende ergänzt. Bsp. 7: Dichte bei lambda = 1,5 war oben abgeschnitten (ylim), Achse „Dichte“. |
| `kapitel_2.R` | R Beispiel 8-11, 12, 13 | Bsp. 8 (H5): `p_load()` ohne pacman brach ab, jetzt `library(MASS)`. Bsp. 11: Achse „f(x1, x2)“ statt „f(x1|x2)“, weil die Schnitte nicht durch die Randdichte geteilt sind. Bsp. 12 (H1): Normalkurve war um den Faktor √10 zu schmal und zeigte den zentralen Grenzwertsatz; ersetzt durch das laufende Mittel von Würfelwürfen und eine Simulation von P(\|Mittel − 3,5\| > 0,1). Bsp. 13: Schleifen durch `replicate()` ersetzt (gleiche Zufallszahlen), Titel ergänzt. |
| `kapitel_3.R` | R Beispiel 14, 15, 16-17 | Bsp. 14 (H8): schätzte keinen Parameter; ergänzt um Mittelwert, S², ML-Varianz und `MASS::fitdistr()` mit angepasster Normalverteilung. Bsp. 15: ML-Schätzer ausgegeben und mit `fitdistr()` geprüft. Bsp. 16: Residuenquadrate mit `fitted()` statt mit der Matrix aus `predict(..., interval = "prediction")`; die Warnung entfällt. Bsp. 17: Tippfehler „transmissin“, Mittelwerte je Gruppe zum Vergleich. |
| `kapitel_4.R` | R Beispiel 18 (bis 21.11.2026 „R Beispiel 18-19“) | Bsp. 18: Startwert gesetzt, Legende „obere/untere Grenze“ statt „oberes/unteres Konfidenzintervall“. Bsp. 19 behandelt Tests und steht jetzt in `kapitel_5.R`. |
| `kapitel_5.R` | R Beispiel 19 (neue Seite in Kapitel 5), R Beispiel 20 | Bsp. 19 (H2): Ablehnungsbereiche als Flächen unter der Dichte statt als flache Rechtecke, Farben richtig zugeordnet, Achse „Teststatistik z“, beobachtete Teststatistik und p-Wert ergänzt. Bsp. 20: Hypothesen und Ergebnis kommentiert. |
| `kapitel_6.R` | R Beispiel 21-24 | Kapitelangaben berichtigt (Bsp. 21 und 22 gehören zu 6.1). Bsp. 23: Der F-Test setzt unabhängige Stichproben voraus und lief auf den gepaarten Cholesterinwerten; jetzt auf den unabhängigen Stichproben aus Bsp. 22. Bsp. 24: `var.equal` entfernt (beim gepaarten Test ohne Bedeutung), Gleichwertigkeit mit dem Test auf die Differenzen gezeigt. |
| `kapitel_7.R` | R Beispiel 25, 26, 27 | Bsp. 27 (H3): Chi-Quadrat-Test auf der stetigen Variablen mpg ersetzt durch einen Unabhängigkeitstest (Getriebe und Motorbauart), Fishers exakten Test und einen echten Anpassungstest (Würfel); `View()` entfernt. |
| `kapitel_8.R` | R Beispiel 28 bis 31-32 | Bsp. 29: Titel „Lineare Regression und Prognose“, Prognose einmal berechnet, Vorhersageintervall in der Legende. Bsp. 30: Kommentar „Motor“ für am berichtigt (Getriebe), Diagnoseplots im 2×2-Raster. Bsp. 31-32: Erläuterung zum Overfitting, Vergleich von AIC und angepasstem R². |

## Lösungen (`lms/loesungen/`)

| Datei | LMS-Quelle | Änderungen |
|---|---|---|
| `uebung_4.R` | Lösung Übung 4 | Seit 22.11.2026 (Paket 10) Lösung der neuen „Übung 4: Auffrischung in R“: Normalverteilung mit pnorm und qnorm, Binomialverteilung, Exponentialverteilung, Kovarianz und Korrelation. Die frühere Übung (Simulation von Verteilungen) stand wortgleich in AMS02. |
| `uebung_6.R` | Lösung Übung 6 | Seit 22.11.2026 neue Übung zu Kapitel 3.5 (Erwartungstreue und mittlerer quadratischer Fehler der Varianzschätzer, Konsistenz des Mittelwerts). Die alte Übung zum zentralen Grenzwertsatz glich R-Beispiel 13 bis zur Saat (H9) und ist entfallen. |
| `uebung_9.R` | Lösung Übung 9 (neu, Kapitel 7) | Seit 22.11.2026. Die Aufgaben sind die früheren Zusatzaufgaben 4 bis 6 aus Teil 1 (Korrelationstest, Chi-Quadrat-Test, Spearman-Test); Code aus `zusatzaufgaben_teil_1.R`, ergänzt um die erwarteten Häufigkeiten und den kritischen Wert. |
| `uebung_10.R` | Lösung Übung 10 (neu, Kapitel 8) | Seit 22.11.2026. Ersetzt die Aufgabe „Multiple lineare Regression - Übung“, die polynomiale und logistische Regression verlangte (Review M8). Vier Aufgaben zu 8.1 bis 8.4 mit den King-County-Daten. |
| `uebung_8.R` | Lösung Übung 8 | (H4) `conf.level = alpha` lieferte ein 5-%-Intervall, jetzt `1 - alpha`. Die Gütefunktion brach ab (`sd()` auf einen Data Frame). `?t.test` entfernt, Hypothesen ausgeschrieben, Daten aus `data/input/`. |
| `zusatzaufgaben_teil_1.R` | R-Code Aufgaben Teil 1 (00 Tag 1.R) | Fester OneDrive-Pfad durch `data/input/` ersetzt, Umlaute repariert. Aufgabe 3 brach ab: Für Österreich und die Türkei fehlt 2010 der Marktanteil, die Summe wurde `NA`; jetzt über `merge()` nach Land zugeordnet und unvollständige Länder ausgeschlossen. Aufgabe 2: überflüssigen F-Test und `var.equal` beim gepaarten Test entfernt. Aufgabe 6 fehlte, ergänzt. Simulation mit N = 10^5 statt 105 Wiederholungen (siehe unten). Seit 23.11.2026 (Paket 16) ohne die Lösungen zu den früheren Aufgaben 4 bis 6; sie stehen als Übung 9 in Kapitel 7 (Lösung in `uebung_9.R`), an ihrer Stelle steht ein Verweis wie auf der Seite im LMS. Titel und Kapitelangaben wie die Seite („Zusatzaufgaben Teil 1: Konfidenzintervalle und Tests“, Kapitel 4 bis 6), Datenhinweis zu Aufgabe 2 auf „28 europäische Länder“ angepasst. |
| `zusatzaufgaben_teil_2.R` | R-Code Aufgaben Teil 2 (00 Tag 2.R) | Fester Pfad ersetzt, `library(readr)` ergänzt (fehlte im Skript), Umlaute repariert. Modellvergleich als Tabelle mit R², angepasstem R², AIC und BIC. Aufgabe 2 schätzt dieselben drei Modelle wie Aufgabe 1; das LMS-Skript nahm dort als drittes Modell flavor + oakiness + aroma. Seit 23.11.2026 (Paket 16) Titel wie die Seite im LMS („Zusatzaufgaben Teil 2: Regression“). |

## Im LMS erledigt am 22.11.2026 (Paket 6)

- **Zusatzaufgaben Teil 1, Aufgabe 1:** „N = 100.000“ statt „N = 105“, vom User bestätigt.
- **Zusatzaufgaben Teil 1, Aufgaben 2 und 3:** „von 28 europäischen Ländern“ statt „der 28 Mitgliedsstaaten der EU“.
- **Datei zu Übung 8:** Link im Textfeld entfernt, die Datei heißt „Autofahrer.csv“.
- **Übung 6:** neu zu Kapitel 3.5, siehe oben.

## Im LMS erledigt am 22.11.2026 (Paket 10)

- **Kapitel 1 ist eine Auffrischung:** Es ist derselbe Foliensatz wie AMS02 VI und VII. Sechs Videoeinheiten (1.1, 1.2, 1.4, 1.5, 1.9.2, 1.9.3), **R-Beispiel 3 und 4** und die Aufgabenblätter 1 und 3 liegen seit 22.11.2026 verborgen im Archiv des Kurses. Der Code von Beispiel 3 und 4 bleibt in `kapitel_1.R`.
- **Übung 4:** neu als „Auffrischung in R“, siehe oben.

## Im LMS erledigt am 22.11.2026 (Paket 4)

- **Text statt Bild:** Übung 5 und 7 mit Lösungen, die Lösungsskizze zu Übung 9 und die Formel für das Konfidenzintervall von λ in den Zusatzaufgaben Teil 1 stehen als Text, Tabellen als HTML-Tabellen.
- **Berichtigt in der Lösung zu Übung 7:** In Aufgabe 2 d gilt Φ(0,7906) = 0,7854, also α ≥ 0,4292 (bisher 0,4296 mit falscher Ungleichung); die Formel zu Aufgabe 1 zeigt das Quantil 2,5758, mit dem gerechnet wurde.
- **Zusatzaufgaben Teil 1:** Aufgaben 2 und 3 zu den Tests in der Anrede „du“ statt „Sie“.

## Im LMS erledigt am 22.11.2026 (Paket 5)

- **Namen:** R-Beispielseiten nach dem Schema oben; R-Beispiele 21 bis 24 mit der Kapitelangabe 6.1 und 6.2, R-Beispiele 31 und 32 stehen jetzt hinter dem Video 8.4.
- **Zusatzaufgaben Teil 2:** Anrede „du“ statt „Sie“, Modell als Y = Xβ + ε, „R²“ statt „R^2“.
- **Video 7.3:** Ein Hinweis unter dem Video verweist auf R-Beispiel 27, weil das Video den Chi-Quadrat-Test noch auf der stetigen Variablen mpg zeigt (H3).

## Im LMS erledigt am 22.11.2026 (Paket 7)

- **Lösungsseiten zu Übung 4, 6, 8, 9 und 10:** Der Satz vor dem Code nennt den Menüpfad so, wie RStudio ihn zeigt: „File > New File > R Script“. RStudio hat keine deutsche Oberfläche; vorher stand dort „Datei > Neue Datei > R-Skript“. Der Code ist unverändert.
- **Posit Cloud:** Der Link „RStudio Cloud“ heißt jetzt „Posit Cloud“ und führt auf https://posit.cloud/.
- **Literatur:** Fahrmeir u. a. in der 9. Auflage (2023) mit Leseliste je Kapitel; alle Seitenangaben im Kurs gelten für die 9. Auflage.

## Im LMS erledigt am 22.11.2026 (Paket 11)

- **Empfehlung zur Bearbeitung:** Neues Textfeld „Empfehlung zur Bearbeitung (Folien)“ in „Begrüßung & Allgemeines“ hinter „Aufbau des Moduls“, mit zwei Folien als Bildslider und dem Link „Folien als PDF“. Die Folien nennen die Reihenfolge der Kapitel bis zur Prüfung entlang der Termine im Sommersemester 2027 (virtuelle Klassenzimmer dienstags 04.05., 11.05., 18.05. und 25.05.2027) und den Weg durch ein Kapitel. Einen Wochenplan oder Umfangsangaben je Kapitel gibt es im Kurs nicht.

## Im LMS erledigt am 22.11.2026 (Paket 12)

- **Kapitel 1 neu nummeriert:** Seit Teile von Kapitel 1 im Archiv liegen, zählt das Kapitel durchgehend 1.1 bis 1.8 mit Unterabschnitten (bisher 1.1.1, 1.3, 1.6 bis 1.11.2). Unter jedem Video steht ein Hinweis, dass das Video noch die alte Nummer und die alte Agenda zeigt.
- **`r_beispiele/kapitel_1.R`:** Codeköpfe der Beispiele 1, 2, 5, 6 und 7 mit den neuen Nummern 1.5, 1.6.1, 1.7, 1.7.1 und 1.7.2, zeichengleich mit den Textseiten im LMS. Die archivierten Beispiele 3 und 4 behalten die alten Nummern 1.9.2 und 1.9.3.
- **Skript zu Kapitel 1:** Agenda und Folientitel mit den neuen Nummern; Seitenzahlen auf den Folien gleich den PDF-Seiten.

## Im LMS erledigt am 23.11.2026 (Paket 13)

- **Ein Skript für das ganze Modul:** Die Skripte zu Kapitel 2 bis 8 sind neu gesetzt; die fachlichen und sprachlichen Fehler aus der Skriptprüfung sind berichtigt, Grafiken und R-Ausgaben neu erstellt. Alle acht Kapitel stehen jetzt in einer Datei „Skript zum Modul (Kapitel 1 bis 8)“ in „Begrüßung & Allgemeines“ (285 Seiten, Lesezeichen je Kapitel und Lerneinheit, Seitenzahl auf jeder Folie gleich der PDF-Seite).
- **Kapitel 1 bis 8:** Statt der Skriptdatei steht in jedem Kapitel ein Link „Skript zu Kapitel N“, der das Skript an der ersten Seite des Kapitels öffnet; die Beschreibung nennt die Seiten zu den Videos. Die bisherigen acht Skriptdateien liegen verborgen im Archiv.

## Im LMS erledigt am 23.11.2026 (Paket 14 und 15)

- **Hinweise unter elf Videos:** Unter 2.2, 2.3, 2.3.2, 3.3, 3.5, 4.3, 5.2, 5.3, 6.1, 6.2 und 7.3 steht ein „Hinweis zum Video“ bzw. „Richtigstellungen zum Video“: was die Folie im Video falsch zeigt, was richtig ist und auf welcher Seite des Skripts es berichtigt steht.
- **`r_beispiele/kapitel_2.R`:** Beispiele 8 bis 11 mit „(Kap. 2.1.1)“; Beispiel 8 mit `asp = 1`; Beispiel 13 mit `ylab = "Dichte"` in allen drei Histogrammen. Im LMS heißt die Seite „R-Beispiele 8 bis 11 (Kapitel 2.1.1)“.
- **`r_beispiele/kapitel_3.R`:** Beispiel 14 mit `ylim = c(0, 0.12)` (der höchste Balken wurde abgeschnitten); Beispiel 15 mit „(Kap. 3.3.2)“, im LMS „R-Beispiel 15 (Kapitel 3.3.2)“. Das Video heißt „3.4.2 Beispiel: mtcars-Datensatz in R“.
- **`r_beispiele/kapitel_7.R`:** Beispiel 27 zusätzlich mit `chisq.test(tabelle, correct = FALSE)`, der Rechnung ohne Yates-Korrektur wie in der Formel.
- **`r_beispiele/kapitel_8.R`:** Beispiel 32 mit einem Kommentar, warum `AIC()` um 92,81 über den Werten von `step()` liegt.

## Im LMS erledigt am 23.11.2026 (Paket 16)

- **R-Code zu den Zusatzaufgaben Teil 1 und 2 (38606, 38607):** Dateien gegen `loesungen/zusatzaufgaben_teil_1.R` und `loesungen/zusatzaufgaben_teil_2.R` getauscht, Dateinamen gleich, jetzt Revision 5; im LMS bytegleich mit dem Repository geprüft. Teil 1 enthält statt der Lösungen zu den früheren Aufgaben 4 bis 6 einen Verweis auf Übung 9 in Kapitel 7.

## Offen

Derzeit nichts (Stand 23.11.2026).

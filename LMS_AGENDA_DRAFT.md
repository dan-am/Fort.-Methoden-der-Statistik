# LMS-Agenda: Fortgeschrittene Methoden der Statistik

Gesäuberte Draft-Version der aktuellen Kursstruktur im LMS. Pro Kapitel und Unterkapitel folgt eine kurze Motivation für das jeweilige Video. Im Anschluss werden R-Beispiele, Vorlesungsskript und Aufgaben aufgeführt.

Legende:
- **Inhalt** = Folien- und Textbausteine im LMS
- **R-Beispiel** = R-Codepage im LMS
- **Skript** = Vorlesungsskript als PDF (Portable Document Format)
- **Aufgabe und Lösung** = Übungsblätter und Musterlösungen

## 2.1 Wahrscheinlichkeitstheorie

Dieses Kapitel legt das gesamte begriffliche und rechnerische Fundament der Vorlesung. Wer Wahrscheinlichkeiten sauber formuliert und berechnet, kann später Schätzer, Tests und Konfidenzintervalle problemlos einordnen.

### Inhalte

**1.1 Wahrscheinlichkeit von Ereignissen, Teil 1**
Im ersten Teil klären wir, was eine Wahrscheinlichkeit überhaupt aussagt und wie wir sie für ein einzelnes Ereignis bestimmen. Damit ist die Sprache gesetzt, mit der alle weiteren Themen formuliert werden.

**1.1.1 Wahrscheinlichkeit von Ereignissen, Teil 2**
Der zweite Teil zeigt, wie sich zusammengesetzte Ereignisse aus einfachen Ereignissen aufbauen lassen. Du lernst, Schnitte und Vereinigungen sauber zu zerlegen und ihre Wahrscheinlichkeiten zu berechnen.

**1.2 Axiomatik der Wahrscheinlichkeitstheorie**
Die drei Axiome nach Kolmogorov bilden das Fundament der gesamten Stochastik. Wer sie verstanden hat, weiß, warum alle späteren Rechenregeln zwingend gelten.

**1.3 Multiplikationssatz für bedingte Wahrscheinlichkeiten**
Treten zwei Ereignisse hintereinander auf, hilft der Multiplikationssatz, ihre gemeinsame Wahrscheinlichkeit zu bestimmen. Das Video zeigt, wie aus bedingten Wahrscheinlichkeiten die Wahrscheinlichkeit eines ganzen Pfades wird.

**1.4 Satz von der totalen Wahrscheinlichkeit**
Kann ein Ergebnis auf mehreren Wegen entstehen, zerlegt der Satz von der totalen Wahrscheinlichkeit das Problem in disjunkte Teilfälle. Im Video setzen wir diese Teilfälle gewichtet zur Gesamtwahrscheinlichkeit zusammen.

**1.5 Satz von Bayes**
Der Satz von Bayes dreht eine bedingte Wahrscheinlichkeit um, etwa von „Test positiv, gegeben krank" zu „krank, gegeben Test positiv". Damit beantworten wir typische Diagnose- und Risikofragen, die in der Praxis ständig vorkommen.

**1.6 Unabhängigkeit von Ereignissen**
Was bedeutet es genau, dass zwei Ereignisse unabhängig sind, und wie prüft man das rechnerisch? Das Video grenzt die Unabhängigkeit klar von der reinen Disjunktheit ab.

**1.7 Zufallsvariable und Verteilungsfunktionen**
Eine Zufallsvariable ordnet jedem Versuchsausgang eine Zahl zu und macht aus sprachlichen Ereignissen Rechenobjekte. Die Verteilungsfunktion fasst zusammen, wie wahrscheinlich kleine und große Werte sind.

**1.8 Berechnung von Wahrscheinlichkeiten**
Sobald die Verteilungsfunktion vorliegt, lassen sich Intervallwahrscheinlichkeiten direkt ablesen. Im Video üben wir, diese Berechnungen sauber zu notieren und in R nachzuvollziehen.

**1.9 Diskrete Zufallsvariable und Verteilungsfunktionen**
Diskrete Variablen nehmen nur einzelne, abzählbare Werte an, etwa die Anzahl der Treffer in einer Stichprobe. Hier zeigen wir, wie Wahrscheinlichkeitsfunktion und Verteilungsfunktion in diesem Fall zusammenhängen.

**1.9.1 Binomialverteilung**
Die Binomialverteilung beschreibt die Anzahl der Erfolge in n unabhängigen Versuchen mit gleicher Erfolgswahrscheinlichkeit. Damit lassen sich Qualitätstests, Klickraten und Wahlumfragen direkt modellieren.

**1.9.2 Hypergeometrische Verteilung**
Wird ohne Zurücklegen gezogen, passt die Binomialverteilung nicht mehr exakt. Die hypergeometrische Verteilung schließt diese Lücke und ist typisch für Lottoziehungen oder Stichproben aus einer endlichen Grundgesamtheit.

**1.9.3 Poissonverteilung**
Die Poissonverteilung modelliert die Anzahl seltener Ereignisse in einem festen Zeitraum, zum Beispiel die Zahl der Anrufe in einem Callcenter pro Minute. Wir zeigen auch, wann sie als gute Näherung der Binomialverteilung dient.

**1.10 Stetige Zufallsvariable und Verteilungsfunktionen**
Bei stetigen Variablen besitzt jeder Einzelwert die Wahrscheinlichkeit null, nur Intervalle haben eine positive Wahrscheinlichkeit. Die Dichtefunktion ersetzt hier die Wahrscheinlichkeitsfunktion und ist die Grundlage für Integrale.

**1.10.1 Die Normalverteilung**
Die Normalverteilung ist die wichtigste Verteilung der Statistik, weil sie viele reale Phänomene und vor allem Mittelwerte gut beschreibt. Wir rechnen mit Erwartungswert, Standardabweichung und standardisierten z-Werten.

**1.10.2 Die Exponentialverteilung**
Die Exponentialverteilung beschreibt Wartezeiten bis zum nächsten Ereignis, etwa bis zum nächsten Maschinenausfall. Ihre besondere Eigenschaft, die Gedächtnislosigkeit, wird im Video an Beispielen veranschaulicht.

**1.11 Einführung: Kenngrößen einer Verteilungsfunktion**
Eine Verteilung lässt sich durch wenige Kennzahlen wie Erwartungswert oder Varianz zusammenfassen. Diese Kenngrößen bilden die Brücke zur deskriptiven Statistik und zur späteren Schätztheorie.

**1.11.1 Streuungsmaße für Verteilungsfunktionen**
Streuungsmaße beantworten die Frage, wie weit die Werte um den Erwartungswert schwanken. Du siehst, warum Varianz und Standardabweichung trotz ihrer Verwandtschaft unterschiedlich interpretiert werden.

**1.11.2 Kenngrößen für zweidimensionale Verteilungsfunktionen**
Sobald zwei Merkmale gleichzeitig betrachtet werden, treten Kovarianz und Korrelation hinzu. Sie messen, wie stark zwei Größen gemeinsam variieren.

**Übersicht diskrete Verteilungsfunktionen**
Die Übersicht stellt die wichtigsten diskreten Verteilungen mit ihren Annahmen und Kenngrößen gegenüber. Sie eignet sich als Nachschlagewerk für Übungsaufgaben und Klausurvorbereitung.

### R-Beispiele
- R-Beispiel 1, Kapitel 1.8
- R-Beispiel 2, Kapitel 1.9, Binomialverteilung
- R-Beispiel 3, Kapitel 1.9.2, Hypergeometrische Verteilung
- R-Beispiel 4, Kapitel 1.9.3, Poissonverteilung
- R-Beispiel 5, Kapitel 1.10, Stetige Verteilungen
- R-Beispiel 6, Kapitel 1.10.1, Normalverteilung
- R-Beispiel 7, Kapitel 1.10.2, Exponentialverteilung

### Skript
- Kapitel 1, Vorlesungsskript

### Aufgaben und Lösungen
- Reflexion der Wahrscheinlichkeitstheorie
- Aufgabenblatt 1, Wahrscheinlichkeitstheorie  |  Lösung Übungsblatt 1
- Aufgabenblatt 2, Wahrscheinlichkeitstheorie  |  Lösung Übungsblatt 2
- Aufgabenblatt 3, Wahrscheinlichkeitstheorie  |  Lösung Übungsblatt 3
- Aufgabe 4, Wahrscheinlichkeitstheorie: Simulation von einhundert Zufallsvariablen  |  Lösung 4

## 2.2 Ergänzungen zur Wahrscheinlichkeitstheorie

Aufbauend auf Kapitel 1 betrachten wir zwei Merkmale gleichzeitig und untersuchen das Verhalten von Mittelwerten in großen Stichproben. Damit liegen die theoretischen Werkzeuge bereit, die jede spätere Inferenz nutzt.

### Inhalte

**2.1 Zweidimensionale Verteilungsfunktionen**
Hier wird die Wahrscheinlichkeitsrechnung auf zwei Variablen gleichzeitig erweitert, was die Grundlage für Korrelation und Regression bildet. Du lernst die gemeinsame Verteilung sowie die zugehörigen Rand- und bedingten Verteilungen kennen.

**2.1.1 Beispiele: Zweidimensionale Normalverteilung**
Die zweidimensionale Normalverteilung ist das wichtigste konkrete Modell für zwei stetige Merkmale, etwa Körpergröße und Gewicht. Anhand von Höhenlinien und Streudiagrammen siehst du, wie sich verschiedene Korrelationen visuell auswirken.

**2.2 Die Ungleichung von Tschebyscheff**
Die Ungleichung von Tschebyscheff liefert eine Schranke dafür, wie wahrscheinlich Werte weit weg vom Erwartungswert sind. Sie gilt für jede Verteilung mit endlicher Varianz und ist daher ein universelles Werkzeug.

**2.3 Grenzwertsätze**
Grenzwertsätze beschreiben das Verhalten von Mittelwerten, wenn der Stichprobenumfang wächst. Sie erklären, warum große Stichproben besonders verlässliche Aussagen erlauben.

**2.3.1 Das schwache Gesetz der großen Zahlen**
Das schwache Gesetz der großen Zahlen besagt, dass das arithmetische Mittel mit wachsendem Stichprobenumfang gegen den Erwartungswert strebt. Damit wird die intuitive Idee „je mehr Daten, desto stabiler der Mittelwert" mathematisch sauber begründet.

**2.3.2 Zentraler Grenzwertsatz**
Der zentrale Grenzwertsatz erklärt, warum Mittelwerte fast immer annähernd normalverteilt sind, selbst wenn die einzelnen Werte es nicht sind. Auf diesem Ergebnis beruhen Konfidenzintervalle und Tests in den späteren Kapiteln.

### R-Beispiele
- R-Beispiele 8 bis 11, Kapitel 2.1, Zweidimensionale Verteilungen
- R-Beispiel 12, Kapitel 2.3.1, Schwaches Gesetz der großen Zahlen
- R-Beispiel 13, Kapitel 2.3.2, Zentraler Grenzwertsatz

### Skript
- Kapitel 2, Vorlesungsskript

### Aufgaben und Lösungen
- Aufgabe 5, Ergänzung Wahrscheinlichkeitstheorie: aus langjähriger Erfahrung  |  Lösung 5

## 2.3 Induktive Statistik

In diesem Kapitel beginnt der Schluss von der Stichprobe auf die Grundgesamtheit. Du lernst, wie aus Beobachtungen konkrete Schätzwerte für unbekannte Parameter entstehen und wie man die Güte solcher Schätzer beurteilt.

### Inhalte

**3.1 Einführung: Induktive Statistik**
Die induktive Statistik schließt von einer Stichprobe auf die unbekannte Grundgesamtheit. Das Video ordnet die zentralen Begriffe Schätzer, Test und Konfidenzintervall in dieses Vorgehen ein.

**3.2 Parameterschätzung**
Aus den Daten werden Schätzer für unbekannte Parameter wie Erwartungswert und Varianz bestimmt. Du lernst die wichtigsten Schätzer und ihre Interpretation in der Praxis kennen.

**3.3 Maximum-Likelihood-Methode**
Die Maximum-Likelihood-Methode wählt den Parameter so, dass die beobachteten Daten am wahrscheinlichsten geworden sind. Sie ist die meistverwendete Schätzstrategie in der modernen Statistik und im maschinellen Lernen.

**3.3.1 Allgemeine Beschreibung der Likelihood-Funktion**
Die Likelihood-Funktion fasst die Wahrscheinlichkeit der Daten als Funktion des unbekannten Parameters auf. Aus ihr leiten wir die Log-Likelihood her, die für die Maximierung handlicher ist.

**3.3.2 Beispiele: Die Maximum-Likelihood-Methode**
Anhand der Bernoulli- und der Exponentialverteilung rechnen wir die Methode konkret durch. So siehst du Schritt für Schritt, wie aus der Formel der vertraute Mittelwert oder Kehrwert des Mittelwerts entsteht.

**3.4 Die Methode der kleinsten Fehlerquadrate**
Die Methode der kleinsten Fehlerquadrate, auch OLS (Ordinary Least Squares) genannt, schätzt Parameter so, dass die Summe der quadrierten Abweichungen minimal wird. Sie bildet die Basis der linearen Regression und vieler weiterer Verfahren.

**3.4.1 Beispiel zur Methode der kleinsten Fehlerquadrate**
An einem einfachen Datenbeispiel führen wir die Schätzung der Regressionsgerade Schritt für Schritt durch. Damit verstehst du, was R im Hintergrund eigentlich berechnet.

**3.4.2 Beispiel: mtcars-Datensatz in R**
Der Beispieldatensatz mtcars enthält technische Daten von 32 Automodellen und eignet sich gut für die erste Regression in R. Du siehst, wie aus Befehl und Datensatz eine fertige Modellgleichung entsteht.

**3.5 Anforderungen an Schätzfunktionen**
Damit ein Schätzer brauchbar ist, sollte er bestimmte Eigenschaften erfüllen, etwa keine systematische Verzerrung haben. Im Video lernst du die zentralen Kriterien Erwartungstreue, mittlerer quadratischer Fehler und Konsistenz kennen.

**3.5.1 Der mittlere quadratische Fehler**
Der mittlere quadratische Fehler, kurz MSE (Mean Squared Error), fasst Varianz und Bias eines Schätzers in einer einzigen Zahl zusammen. Damit lassen sich konkurrierende Schätzer fair vergleichen.

**3.5.2 Erwartungstreue Schätzer**
Ein erwartungstreuer Schätzer trifft im Mittel den wahren Parameter und ist damit frei von systematischer Verzerrung. Das Video zeigt, warum der Mittelwert erwartungstreu ist, die empirische Varianz aber erst nach Korrektur.

### R-Beispiele
- R-Beispiel 14, Kapitel 3.2, Parameterschätzung
- R-Beispiel 15, Kapitel 3.3, Maximum-Likelihood
- R-Beispiele 16 und 17, Kapitel 3.4.2, mtcars-Regression

### Skript
- Kapitel 3, Vorlesungsskript

### Aufgaben und Lösungen
- Übung 6, Induktive Statistik: Veranschaulichung von Schätzern  |  Lösung 6

## 2.4 Konfidenzintervalle

Punktschätzer liefern eine einzige Zahl, sagen aber nichts über deren Unsicherheit aus. Konfidenzintervalle ergänzen den Schätzwert um eine Spanne, in der der wahre Parameter mit hoher Wahrscheinlichkeit liegt.

### Inhalte

**4.1 Einführung**
Ein Konfidenzintervall ergänzt einen Punktschätzer um eine Aussage zur Unsicherheit. Du lernst, wie das Konfidenzniveau korrekt zu interpretieren ist und warum „95 Prozent" nicht bedeutet, dass der wahre Wert mit dieser Wahrscheinlichkeit im Intervall liegt.

**4.2 Konfidenzintervalle bei bekannter Verteilung**
Bei normalverteilten Daten lassen sich exakte Konfidenzintervalle für Erwartungswert und Varianz angeben. Das Video stellt die Formeln mit z-Quantilen und t-Quantilen Schritt für Schritt vor.

**4.3 Konfidenzintervalle bei unbekannter Verteilung**
Wenn die Verteilungsannahme nicht gesichert ist, liefert der zentrale Grenzwertsatz asymptotische Konfidenzintervalle. Damit lassen sich auch in der Praxis robuste Intervalle berechnen.

### R-Beispiele
- R-Beispiele 18 und 19, Kapitel 4.3

### Skript
- Kapitel 4, Vorlesungsskript

### Aufgaben und Lösungen
- Übung 7, Konfidenzintervalle  |  Lösung 7

## 2.5 Statistische Tests

Mit einem statistischen Test beantwortest du die Frage, ob ein beobachteter Effekt durch Zufall erklärbar ist oder nicht. Dieses Kapitel führt die Logik des Hypothesentests am Beispiel des t-Tests ein.

### Inhalte

**5.1 Einführung: Statistischer Test**
Ein statistischer Test überprüft eine Vermutung, also eine Hypothese, anhand der vorliegenden Daten. Du lernst die Bestandteile Nullhypothese, Alternativhypothese, Teststatistik und Ablehnungsbereich kennen.

**5.2 t-Test und asymptotischer Test**
Der t-Test ist das Standardverfahren für Mittelwerte bei kleinen Stichproben und normalverteilten Daten. Für große Stichproben tritt der asymptotische Test mit Hilfe des zentralen Grenzwertsatzes an seine Stelle.

**5.3 Fehler 2. Art und der p-Wert**
Neben dem Fehler 1. Art, also einem falschen Alarm, betrachten wir den Fehler 2. Art, also das Übersehen eines echten Effekts. Der p-Wert verdichtet diese Idee zu einer Kennzahl, die in Berichten und Software überall auftaucht.

### R-Beispiele
- R-Beispiel 20, Kapitel 5.2

### Skript
- Kapitel 5, Vorlesungsskript

### Aufgaben und Lösungen
- Übung 8, Statistischer Test  |  Lösung 8

## 2.6 Zweistichprobentests

Wenn zwei Gruppen verglichen werden sollen, kommen die Zweistichprobentests zum Einsatz. Dieses Kapitel unterscheidet, ob die Gruppen unabhängig voneinander sind oder gepaart erhoben wurden.

### Inhalte

**6.1 Unabhängige Stichproben**
Werden zwei Gruppen unabhängig voneinander gezogen, etwa Kunden in zwei Filialen, vergleichen wir ihre Mittelwerte mit dem Zweistichproben-t-Test. Das Video zeigt, wann gleiche und wann ungleiche Varianzen angenommen werden.

**6.2 Verbundene Stichproben**
Bei verbundenen Stichproben werden dieselben Personen oder Objekte zweimal gemessen, etwa vor und nach einer Schulung. Hier verwenden wir den gepaarten t-Test, der die Differenzen je Paar auswertet.

### R-Beispiele
- R-Beispiele 21 bis 24, Kapitel 6.2

### Skript
- Kapitel 6, Vorlesungsskript

### Aufgaben
- Reflexion: Interpretation der Tests

## 2.7 Tests für Zusammenhangsmaße

In diesem Kapitel prüfen wir, ob zwischen zwei Merkmalen ein systematischer Zusammenhang besteht. Je nach Skalenniveau und Verteilung kommen unterschiedliche Tests zum Einsatz.

### Inhalte

**7.1 Normalverteilte Grundgesamtheit (Pearson)**
Sind beide Merkmale normalverteilt, messen wir den linearen Zusammenhang mit dem Pearson-Korrelationskoeffizienten und testen ihn auf Signifikanz. Das Video zeigt, wann diese Annahme tragfähig ist und wie der Test in R aufgerufen wird.

**7.2 Spearman-Test**
Der Spearman-Test misst monotone Zusammenhänge auf Basis von Rängen und kommt ohne Verteilungsannahme aus. Damit lässt sich auch bei schiefen Verteilungen oder Ausreißern ein verlässliches Maß angeben.

**7.3 Chi-Quadrat-Test**
Der Chi-Quadrat-Test prüft, ob zwei kategoriale Merkmale unabhängig voneinander sind. Du lernst, wie aus einer Kontingenztabelle Erwartungswerte und Teststatistik entstehen.

### R-Beispiele
- R-Beispiel 25, Kapitel 7.1
- R-Beispiel 26, Kapitel 7.2
- R-Beispiel 27, Kapitel 7.3

### Skript
- Kapitel 7, Vorlesungsskript

## 2.8 Lineare Regression

Die lineare Regression ist das wichtigste Werkzeug, um Zusammenhänge zwischen einer Zielgröße und mehreren Einflussfaktoren zu modellieren. Sie verbindet die Methoden der vorigen Kapitel zu einem praktisch einsetzbaren Verfahren.

### Inhalte

**8.1 Einfache lineare Regression**
Die einfache lineare Regression beschreibt den linearen Zusammenhang zwischen einer Zielgröße und einem Einflussfaktor. Sie liefert nicht nur eine Vorhersageformel, sondern auch eine inhaltlich interpretierbare Steigung.

**8.2 Prognose und Podcast**
Hier zeigen wir, wie aus einem geschätzten Regressionsmodell konkrete Prognosewerte und Vorhersageintervalle entstehen. Der begleitende Podcast vertieft die Idee an einem Anwendungsbeispiel.

**8.3 Multiple Regression**
Die multiple Regression bezieht mehrere Einflussfaktoren gleichzeitig in das Modell ein. Damit lassen sich Effekte herausrechnen, die in der einfachen Regression noch vermischt waren.

**8.4 Modellselektion**
Bei vielen möglichen Variablen stellt sich die Frage, welche tatsächlich in das Modell gehören. Die Modellselektion über Kriterien wie das AIC (Akaike Information Criterion) liefert eine systematische Antwort.

### R-Beispiele
- R-Beispiel 28, Kapitel 8.1
- R-Beispiel 29, Kapitel 8.2
- R-Beispiel 30, Kapitel 8.3
- R-Beispiele 31 und 32, Kapitel 8.4

### Skript
- Kapitel 8, Vorlesungsskript

### Aufgaben
- Hands-on-Aufgabe zur linearen Regression

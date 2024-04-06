# Clusteranalyse: Anwendungsorientierte Einführung in Klassifikationsverfahren. Bacher & Wenzig, 2010

## Einleitung

### Zielsetzung clusteranalytischer Verfahren
- Primäres Ziel: Menge von Klassifikationsobjekten in homogene Gruppen zusammenzufassen
  - Auffinden einer empirischen Klassifikation
    - Wenn Klassifikation auf empirischen Beobachtungen basiert
- Bei Personen: Objektorientierte Datenanalyse, Datenmatrix bildet Klassifikationsobjekte
#### Anforderungen/Kriterien für gute Clusterlösung
1. Cluster in sich homogen
2. Cluster voneinander isoliert
3. Clusster sollen den Daten gut angepasst sein. Klassifikation soll in Lage sein, Variation in Daten zu erklären
4. Cluster solle stabil sein. Geringfügige Änderungen in Daten o. im Verfahren solle in keine gravierenden Änderungen der Ergebnisse resultieren
5. Cluster sollen inhaltlich gut interpretierbar sein. Clustern sollen inhaltlich sinnvolle Namen gegeben werden. Im Idealfall sollen die Namen aus einer Theorie abgeleitet werden
6. Cluster sollen (inhaltlich) valide sein. Cluster sollen mit externen Variablen korrelieren, von denen bekannt ist, dass sie im Zusammenhang mit den Typen stehen, die aber nicht in die Bildung der Cluster eingehen
7. Zahl der Cluster soll klein und damit überschaubar sein. Angenommen wird, dass dies die inhaltliche Interpretierbarkeit (Kriterium 5) erleichtert und die Stabilität erhöht (Kriterium 4)
8. Cluster selbst sollen gewisse Mindestgröße haben. Dies soll zur Stabilität (Kriterium 4) beitragen

### Clusteranalyseverfahren
- **Unvollständige Clusteranalyseverfahren**
- **Deterministische Clusteranalyseverfahren**
  - Klassifikationsobjekte werden mit einer W. von 1 einem o. mehreren Clustern zugeordnet
  - **hierarchische Verfahren**
  - **Partitionierende Verfahren**
  - Gibt auch **überlappende Clusteranalyseverfahren**
    - Klassifikationsobjekt mit "W." von 1 zwei o. mehreren Clustern angehören
- **Probabilistische Clusteranalyseverfahren**
  - Nicht mit W. 1 o. 0, sondern mit dazwischen liegender W. zugeordnet
  - Als Verallgemeinerung der deterministischen Clusteranalyseverfahren vorzustellen
- Unterscheidung in **modellbasierte** & **heuristische Verfahren** vorzufinden
  - **heuritsisch** = unvollständige & deterministische (kein zugrunde liegendes W.modell)
  - **modellbasiert** = deterministische & probabilistische (hat zugrunde liegendes W.modell)
  - Vorteil modellbasiert: Formal besser abgesicherte Teststatisken zur Bestimmung der Clusterzahl bereitstellen
  - modellbasiertes Verfahren mit deterministischer Zuordnung: **Two-Step-Clusterverfahren**
    - Nimmt deterministische Zuordnung vor, basiert aber auf Wahrscheinlichkeitsmodell
    - Beu Clusterbildung selbst wird hierarchisch vorgegangen 

### Grundlage der Clusterbildung
- Für Ähnlichkeit von Objekten innerhalb Clusters bestehen unterschiedliche Möglichkeiten. Zwei grundlegenede können unterschieden werden:
  1. Ähnlichkeit hinsichtlich Merkmalsausprägungen: Objekte haben ähnliche Werte in Merkmalen u. unterscheiden sich in Merkmalsausprägungen von anderen Clustern. (Auch abgeleitete Variablen können eingesetzt werden)
     - In diesem Buch beandelten basieren auf diesem hier
  3. Ähnlichkeit hinsichtlich Zusammenhangsmuster von Variablen: Für Objekte eines Clusters ist charakteristisch, dass für sie dieselben Zusammenhänge zwischen den Variablen gelten. Objekte unterschiedlicher Cluster sind durch unterschiedliche Zusammenhänge gekennzeichnet

### Konfirmatorische & explorative Clusteranalyse
- Werden vielfach als **explorative Verfahren** betrachtet, ist aber nur zum Teil zutreffend
  - Viele Verfahren erfordern nur Spezifikation von Klassifijationsmerkmalen & Objekten, die geclustert werden sollen
  - Zahl der Cluster kann ebenso offen gelassen werden wie die ein Cluster kennzeichnenden Merkmalsausprägungen (Clusterprofil)
  - In diesem Sinne werden Clusterverfahren explorativ eingesetzt
- Können aber auch **konfirmatorisch** verwendet werden
  - Zahl der Cluster & Charakteristiken der Cluster vorab definiert
- **explorative Clusteranalyse**
  - Zahl der Cluster unbekannt, muss bestimmt werden
  - Merkmale der Cluster sind unbekannt & müssen ermittelt werden
  - Inhaltliche Interpretation der Cluster kann schwierig sein
  - Anpassung an Daten wird maximiert
- **konfirmatorische Clusteranalyse**
  - Zahl der Cluster ist bekannt, muss nicht bestimmt werden (Vorteil)
  - Merkmale der Cluster (z.B. Clustermittelwerte bei K-Means-Verfahren) sind zumindest teilweise bekannt & müssen nicht ermittelt werden
  - Cluster haben bereits vorab eine inhaltliche Bedeutung (Vorteil)
  - Anpassung an Daten kann schlecht sein (Nachteil)
  - Standard-Statistikprogramme erhalten keine konfirmatorischen Clusteranalyseverfahren. Daher trotz genannten Vorteilen selten eingesetzt

### Methodische Darstellung sollte folgendes ersichtlich sein:
- Ausgewählte Variablen
- Ausgewählte Objekte
- Gegebenenfalls die eingesetzten Datentransformationen
- Gewählte (Un-)Ähnlichkeitsmaß
- Ausgewählte Verfahren (genaue Bezeichnung)
- Eingesetzte Computerprogramm (inklusive Versionsnummer)
- Technische Voreinstellungen
- Verwendeten Kriterien zur Bestimmung Clusteranzahl
- Durchgeführte Stabilitätsprüfung
- Durchgeführte Validitätsprüfung

- Methodische Beschreibung, stichpunktartig skizziert, wie folgt aussehen:
  - »Für die Analyse wurden die Variablen X1, X2, usw. ausgewählt. In die Analyse wurden alle Befragten einbezogen. Die Variablen wurden z-transformiert, um Vergleichbarkeit zu erreichen. Die Clusteranalyse wurde mit IBM-SPSS 18 gerechnet. Eingesetzt wurde das Ward-Verfahren mit quadrierten euklidischen Distanzen und den technischen Standardeinstellungen, da das Verschmelzungsschema eine klare Interpretation besitzt. Die Bestimmung der Clusterzahl erfolgte mittels des inversen Scree-Tests. Zur Stabilitätsprüfung wurden für die ausgewählte Clusterzahl noch drei weitere Analysen mit dem Complete-, Single- und Weighted-Average-Linkage gerechnet. Die Übereinstimmung wurde mittels des adjustierten Randindex (Hubert und Arabie 1985) beurteilt. Zur Validitätsprüfung wurde auf die Variablen Z1, Z2 usw. zurückgegrien.«

### Modellprüfung und Validierung
Inwiefern die Vorstellungen, die mit der Wahl eines bestimmten Clusteranalyseverfahrens verbunden sind, zutreffen, ist in jedem konkreten Anwendungsfall zu prüfen. Prüfung umfasst folgende Schritte:
1. **Prüfung der Modellanpassung**
   - Gute Modellanpassung liegt dann vor, wenn Kriterien 1-3 (u. eventuell 7 & 8) erfüllt sind
   - Notwendige Voraussetzung für inhaltliche Interpretation
   - Bei nicht vorlage, ist Fehleranalyse für schlechte Modellanpassung durchzuführen
2. **Prüfung der inhaltlichen Interpretierbarkeit**
   - Bei guter Modellanpassung ist inhaltliche Interpretation durchzuführen. Ist nicht möglich? Dann Fehleranalyse
3. **Stabilitätsprüfung**
   - Wird untersucht, wie stark Ergebnisse sich ändern, wen geringfügige Modifikation an Daten u./o. getroffenen Spezifikationen des gewählten Verfahrens vorgenommen werden
   - Führen geringfügige Veränderungen zu strk abweichenden Ergebnissen, liegt Instabilität vor -> Fehleranalyse
4. **Inhaltliche Validitätsprüfung**
   - Besteht darin, dass Hypothesen über den Zusammenhang der ermittelten Cluster mit nicht in die Clusterbildung einbezogenen Merkmalen formuliert werden
     - Können Hypothesen nicht bestätigt werden -> Fehleranalyse
    
### Fehleranalyse
- Steht im Buch was zu, wenn ich es brauche. Wir hoffen mal nicht. :D

### Computerprogramme
- Standardprogramme:
  - IBM-SPSS
  - SAS
  - STATA
  - SYSTAT
- Unendgeldlich:
  - R
  - WEKA
  - KNIME
  - AutoClass (bayesianische Clusteranalyse)
  - CLUSTER 3.0 Software zum K-Median-Verfahren
- Programmpaket: ALMO

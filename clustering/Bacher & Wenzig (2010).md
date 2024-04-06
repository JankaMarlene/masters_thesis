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
  - Vorteil modellbasiert: Formal besser abgesicherte Teststatisken zur Bestimmung der Clusterzahl bereitstellen
  - modellbasiertes Verfahren mit deterministischer Zuordnung: **Two-Step-Clusterverfahren**
    - Nimmt deterministische Zuordnung vor, basiert aber auf Wahrscheinlichkeitsmodell
    - Beu Clusterbildung selbst wird hierarchisch vorgegangen 

### Grundlage der Clusterbildung

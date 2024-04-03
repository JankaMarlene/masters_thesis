# Multivariate Datenanalyse. Wentura & Pospeschill, 2015

## Allgemein
- Clusteranalyse = Verfahrensgruppe, mit denen sich Personen/Objeket (generell: Variablen) anhand empirischer Daten, di spezifische Eigenschaftsstruktur aufweisen, zu möglichst ähnlichen Teilmengen (Gruppen) zusammenfassen lassen
- Alle Eigenschaften werden gleichermaßen zur Gruppenbildung verwendet *(Spät, 1977; Steinhausen&Langer, 1977)*
- Herausfinden, ob es Gruppen (Cluster) von Personen gibt, deren Berwertungsprofile hinreichend ähnlich zueinander, aber unähnlich gegenüber anderen Clustern ist
- Unterscheidung *(Backhaus et al., 2011)*:
  - **Partitionierende** Verfahren
    - Vorgegebene Gruppeneinteilung
    - Durch Umgruppierung einzelner Objekte wird bestehende Lösung optimiert
  - **Hierarchische** Verfahren
    - Anzahl der Cluster wird erst im nachhinein bestimmt
    - Die (Un-)Ähnlichkeiten werden unter Verwendung spezifischer Distanz- oder Ähnlichkeitsmaße gewonnen, ein Algorithmus zur          Zusammenführung der Objekte gewäht und schließlich Anzahl der Cluster bestimmmt
      
## Proximitätsmaße
- Umfassen Distanz- und Ähnlichkeitsmaße
  - Dienen dazu, die Stärke der (Un-)Ähnlichkeit zweier Objekte zu quantifizieren
  - **Distanzmaße** bestimmt Abweichung
  - **Ähnlichkeitsmaße** bestimmt Übereinstimmungen der Werte
  - Da Distanzmaße Gegenstück der Ähnlichkeitsmaße ist Unterschied nicht wesentlich
- In Abhängigkeit vom Skalenniveau (intervall- oder nominalskaliert) der zu transformierenden Daten stehen verschiedene Maße zur    Verfügung *(Pospeschill, 2012)*
  
### Proximitätsmaße für intervallskalierte Daten
- Maße basieren nahezu auf gleichem Prinzip: Ergeben sich aus Differenzen der einzelnen Wertepaare der beiden zu vergleichenden Objekte
- Maße unterscheiden sich in Art, mit der einzelne Differenzen der verschiedenen Wertepaare zu einer Maßzahl zusammengefasst werden
- **Euklidische Distanz** quadriert einzelne Wertepaare und addiert Ergebnisse zu einer Summe über die Variablen hinweg
  - Quadratwurzel der Summe ergibt Maßzahl zur Charakterisierung der Unähnlichkeit
  - häufig Entscheidung für **quadrierte Euklidische Distanz**
  - Ist ein Fall der allgemeineneren **Minkowski-Distanz** (auch **L-Norm** genannt)
  - Resultiert aus der p-ten Wurzel der Summe der p-ten Potenzen des Betrags der Wertepaardifferenzen
- **Korrelation**
  - Typisches Ähnlichkeitsmaß
  - z-standardisierten Werte der Fälle/Variablen werden paarweise multipliziert, summiert und durch Anzahl der Wertepaare minus eins dividiert
  - Korrelationskoeffizient
    
### Proximitätsmaße für nominalskalierte Daten
- Große Fülle an Vorschlägen
- Im Falle **binärer Daten** (vorhanden = 1; nicht vorhanden = 0) folgende Ähnlichkeitsindizes vorgeschlagen:
  - **simple matching**
    - Stärke der Ähnlichkeit errechnet aus Quotienten der Anzahl der Wertepaare mit gleichen Werten und Anzahl aller Wertepaare
  - **Jaccard (Tanimoto)**
    - Der Quotient aus Anzahl der Wertepaare, in denen bei beiden Objekten das jeweilige Merkmal vorliegt, und Anzahl der Wertepaare, in denen bei mindestens einem Objekt ein Merkmal vorliegt
  - (Beschrieben, wann was am besten geigenet ist anhand von Beispielen)

## Clusteralgorithmen
### Partitionierungsverfahren 
  1. Start mit Ausgangspartition, für die pro Cluster ein Mittelwert für jede Eigenschaft bestimmt wird -> Dadurch wird **Gruppenzentroid** definiert
  2. Für bestehende Gruppenzuordnung kann dann ein Maß der Heterogenität berechnet werden
  3. Wird untersucht, durch welche Objektverlagerung von einem Cluster zu einem anderen das Maß der Heterogenität verkleinert werden kann
  4. Das Objekt, das dieses Kriterium maximal erfüllt, wird verschoben
  5. Spiel beginnt von vorne, bis es keine Objektverlagerung mehr gibt, die zu einer Verbesserung führen würde
- Da aufgrund kombinatorischer Gegebenheiten nicht alle möglichen Gruppenzuordnungen geprüft werden können (n Objekte auf k Gruppen aufgeteilt ergeben k^n Aufteilungsmöglichkeiten), wird im Ergebnis nur ein **lokales**, aber **kein globales** Optimum erreicht
- Auch Startposition entscheidet mit über das erzielbare Optimum des Clusterprozesses
  - Empirisch ermittelte Anfangspositionen z.B. durch **Agglomerierungsverfahren** sind daher zufälligen Aufteilungen vorzuziehen
#### Agglomerierungsverfahren
- Verschiedene Algorithmen kommen zur Anwendung
1. Ausgangspunkt: Jedes Objekt zunächst als eigenen Cluster zu betrachten und sämtliche Distanzen zu berechnen
2. Cluster mit geringsten Distanz werden zu neuen Cluster zusammengefasst
3. Für die um eins reduzierte Clusteranzahl werden neue Distanzen bestimmt, die in Abhängigkeit vom verwendeten agglomerativen Verfahren unterschiedlich berechnet werden
4. Nach Abschluss werden wiedrum die Cluster mit größten Ähnlichkeit/geringsten Distanz zusammengeführt
5. Prozess wird fortgeführt, bis sich sämtliche Objekte in einem Cluster befinden
- Distanzberechnung zwischen zwei Clustern A und B erfolgtv in Abhängigkeit vom agglomerativen Verfahren unterschiedlich
  - **Average Linkage**
    - Distanz aller Objektpaare wird berechnet, die sich zwischen den beiden Clustern bilden lassen. Durchschnitt dieser Distanzen wird als Distanz zwischen den Clustern angesehen und bildet Kriterium für Agglomerierung
    - Alle Objekte eines Clusters werden berücksichtigt. Distanz kann nicht von einzelnen Objekten bestimmt werden
  - **Average Group Linkage**
    - Alle Objekte beider Cluster werden zusammengenommen und alle möglichen Objektpaare aus ihnen gebildet
    - Auch zwei Objekte desselben Clusters können ein Paar bilden
    - Für jedes Objektpaar wird Distanz berechnet; aus allen Distanzen wird dann der Durchschnitt ermittelt
    - Methode führt dazu, dass durchschnittliche Distanz des neuen Clusters möglichst gering ist
  - **Single Linkage** (nächstgelegener Nachbar) (**kontrahierender Algirithmus** -> neigt zur Bildung weniger großer Cluster, denen viele kleine Cluster gegenüberstehen; Geeignet um Objekte mit extremen Merkmalen zu identifizieren)
    - Aus allen Objekten beider Cluster werden die beiden am nächsten beieinanderliegenden Objekte ausgewählt, die aus unterschiedlichen Clustern stammen
    - Für diese beiden Objekte wird Distanz berechnet und dann als Distanz zwischen den Clustern betrachtet
    - Eignet sich für erste Analyse, um Objekte mit extremen Ausprägungen zu identifizieren und gegebenenfalls zu entfernen
  - **Complete Linkage** (entferntester Nachbar) (**dilatierendes Verfahren** -> neigt zur Bildung etwa gleich großer Cluster)
    - Objektpaar mit größten Distanz aus beiden Clustern gewählt, die aus unterschiedlichen Clustern stammen
    - Distanz zwischen diesen Objekten gilt als Distanz zwischen den beiden Clustern
  - **Zentroid-Clustering** (konservatives Verfahren)
    - Für jeden Cluster werden Objektmittelwerte der in dem Cluster enthaltenen Objekte berechnet
      - Dies bedeutet für einen neuen Cluster, dass sein Zentroid dem gewogenen Mittel der beiden Zentroiden der Ausgangscluster entspricht, wobei die Objektzahlen der Ausgangscluster die Gewichte bilden
      - Distanz zwischen zwei Clustern ergibt sich dann aus den Mittelwerten
  - **Median-Clustering** (konservatives Verfahren)
    - Zentroid eines neuen Clusters wird aus Zentroiden der Ausgangscluster berechnet, die beide mit gleichen Gewicht in das neue Zentroid eingehen
  - **Ward-Methode** (konservatives Verfahren)
    1. Mittelwerte der einzelnen Objekte für jeden Cluster berechnen
    2. Die quadierten Euklidischen Distanzen der einzelnen Objekte eines Clusters werden zu dem Clustermittelwert ermittelt
    3. Die sich so ergebenden Distanzen der einzelnen Objekte zu den jeweiligen Clustermittelwerten werden für alle Objekte aufsummiert
    4. Es werden jeweils die beiden Cluster zu einem neuen Cluster vereinigt, durch deren Vereinigung sich der geringste Zuwachs in der Gesamtsumme der quadrierten Distanzen ergibt
    - Gilt als besonders verlässlicher Algorithmus, vorausgestezt, Variablen besitzen **metrisches** Skalenniveau, sind unkorreliert und weisen keine Ausreißer auf
    - Annahme: In etwa gleich große Cluster mit gleicher Varianz; In Größe stark variierende Cluster werden durch Algorithmus häufig nicht erkannt
- Bei **konservativen Verfahren** nur Verwendung von Distanzmaßen sinnvoll
- Bei verbleibenden Verfahren kann jedes Proximitätsmaß zum Einsatz kommen

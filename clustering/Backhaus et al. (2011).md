# Multivariate Analysemethoden. Backhaus et al., 2011

## Allgemein
- Personen in einer Gruppe hohe Homogenität (**Intragruppen-Homogenität**)
- Zwischen Gruppen hohe Heterogenität (**Intergruppen-Heterogenität**)
- Clusterananlyse methodisches Instrument, um heterogene Erhebungsergebnisse in homogene Gruppen zu zerlegen
  - Anwendung um Ähnlichkeiten zu bestimmen
- Exploratives Datenanalyseverfahren
  - Da sie im *Ergebnis* zu Vorschlägen für eine Gruppierung erhobener Untersuchungsobjekte führt und damit "neue Erkenntnisse" generiert bzw. Strukturen in Datensätzen entdeckt
- Zwei Untersuchungsobjekte können im Koordinatensystem grafisch verdeutlicht werden. Bei mehr Objekten kann Ergebnis der Clusteranalyse nicht mehr visualisiert werden
  - Im Mehr-Variablen-Fall grafische Verdeutlichung
    - Variablenmenge vorab z.B. mit **Faktorenanalyse** verdichtet werden
    - Wenn interessiert an detaillierten Kenntnissen zu Unterschieden einer gefundenen Cluster-Lösung
      - z.B. **Diskriminanzanalyse**
        - Wird Ergebnis der Clysteranalyse (Anzahl der Gruppen) als abhängige Variable vorgegeben und die Unterschiede zwischen den Gruppen auf Basis der unabhängigen Variablen untersucht
       
## Vorgehensweise
1. Entscheidung darüber, welche Variablen zur Clusterung einer Objektmenge herangezogen werden
   - Von Wahl der Clustervariable ist es abhängig, wodurch später homohene Gruppen beschrieben sind
2. Entscheidung, wie Ähnlichkeiten/Unähnlichkeiten zwischen Objekten bestimmt werden soll
   - Vielzahl an Kriterien stehen zur Verfügung (**Proximitätsmaße**), mit deren Hilfe die (Un-)Ähnlichkeit (Distanz) zwischen Untersuchungsobjekten auf Basis der Clusterbariable bestimmt werden kann
     - **Proximitätsmaße** drücken Ähnlichkeit bzw. Unähnlichkeit zwischen zwei Objekten durch numerische Werte aus
3. Entscheidung über Clusterverfahren, mit dessen Hilfe Clusterung der Objekte vorgenommen werden soll
   - Vielzahl an Cluster-Algorithmen, mit deren Hilfe gleichartige Objekte zu einem Cluster zusammengefasst werden können
4. Entscheidung darüber, wie viele Cluster (Entweder vorgeben oder Kriterien verwenden, von denen sich "bestmögliche" Anzahl an Clustern ableiten lassen
5. Inhaltliche Interpretation der gefundenen Cluster

### 1. Auswahl der Clustervariablen
- "Korrekte" Bestimmung der Clustervariablen entscheidet darüber, wie gut die Ergebnisse einer Clusteranalyse später genutzt werden können
- Folgende Eigenschaften sollten durch Clustervariablen erfüllt sein:
  - **Gruppierungsrelevanz**
    - Für inhaltliche Zielsetzung einer Clusteranalyse müssen Variablen hohe Relevanz besitzen
      - z.B. Ziel: Gruppe von Personen mit ähnlichem Verhalten zu finden
        - Clustervariablen müssen Verhaltensrelevanz aufweisen
    - Wichtig: Nur sollche Merkmale im Gruppierungsprozess berücksichtigen, die aus theorteischen bzw. sachlogischen Überlegungen als relevant für den zu untersuchenden Sachverhalt anzusehen sind
    - Merkmale, die für Untersuchungszusammenhang bedeuutungslos, müssen ausgeschlossen werden
  - **Unabhängigkeit**
    - Wichtig, dass Variablen keine hohen Korrelationen aufweisen sonst "**implizite Gewichtung**" bestimmter Aspekte im Clusteringprozess -> Verzerrung der Ergebnisse
    - Bei korrelierenden Clustervariablen folgende Möglichkeiten
      - *Ausschluss von Variablen*
        - Informationen, die hoch korrelierte Variablen liefern, werden größtenteils durch andere Variable miterfasst und können daher als redundant angesehen werden. Daher Ausschluss sinvoll, um Gleichgewichtung der Daten sicherzustellen
      - *Vorschaltung einer Faktorenanalyse (Hauptkomponentenanalyse)*
        - Hoch korrelierte Variablen können auf unabhängige Faktoren verdichtet werden
        - Hauptkomponentenanalyse als Extraktionsverfahren
      - *Mahalanobis-Distanz als Proximitätsmaß*
        - Bei Verwendung lassen sich dadurch bereits im Rahmen der Distanzberechnung zwischen den Objekten etwaige Korrelationen zwischen Variablen ausschließen
        - Stellt allerdings bestimmte Voraussetzungen an Daten, die häufig bei Clusteranalyseproblemen nicht erfüllt sind
  - **Messbarkeit**
    - Nach Möglichkeit manifeste Variablen, in Wirklichkeit auch beobachtbar sind und gemessen werden können
  - **Vergleichbarkeit der Messdimensionen**
    - Verschiedene Messdimensionen (Skalen) führen allein dadurch zu Vergrößerung von Distanzen zwischen Objekten
    - Um Vergleichbarkeit zwischen Variablen herzustellen, sollte deshalb bei **mentrisch** skalierten Clustervariablen vorab eine Standardisierung vorgenommen werden
      - Dies hat zur Folge, dass alle (standardisierten) Variablen einen Mittelwert von 0 und Varianz von 1 besitzen
      - *Vgl. zur Standardisierung von Variablen in Abschnitt 1.2.*
  - **Beeinflussbarkeit**
    - Anwender:innen möchten i. d. R. die gefundenen Cluster durch spezifische Maßnahmen bearbeiten. Maßnahmen auf Besonderheiten der Cluster ausgerichtet. Daher bei Auwahl der Variablen schon drauf achten, ob sie beeinflussbar sind
  - **Trennkraft**
    - Sollen Cluster untereinander möglichst heterogen sein, Clustervariablen so wählen, dass sie hohe Trennkraft zur Unterscheidung von Clustern aufweisen
    - Clustervariablen, die bei allen Objekten nahezu gleiche ausprägung aufweisen (sog. konstante Merkmale), führen zu einer Nivellierung der Unterschiede zwischen den Objekten und rufen dadurch Verzerrungen bei Fusionierung hervor
    - Konstante Merkmale sind nicht trennungswirksam, daher vorab aus Analyse ausschließen (Besonders für Merkmale, die fast überall Null-Werte aufweisen
  - **Repräsentativität**
    - Wird Clusteranalyse auf Basis einer Stichprobe durchgeführt & sollen aufgrund der gefundenen Gruppierung Rückschlüsse auf Grundgesamtheit gezogen werden, so muss sichergestellt werdem dass auch genügend Elemente in den einzelnen Gruppen enthalten sind, um die entsprechende Teilgruppen in der Grundgesamtheit zu repräsentieren
    - Da i. d. R. im Vorfeld nicht bekannt, welche Gruppen in einer Grundgesamtheit vertreten sind (Auffinden sollcher Gruppen ja Ziel der Clusteranalyse) Ausreißer in Datenmengen eliminieren
      - Ausreißer beeinflussen Fusionierungsprozess, erschweren das Erkennen von Zusammenhöngen zwischen Objekten und führen insgesamt zu Verzerrungen in Ergebnissen
        - *Vgl. zur Analyse von Ausreißern in Abschnitt 1.5.1. sowie Darstellung Single Linkage-Verfahren in Abschnitt 8.2.3.2.*
  - **Clusterstabilität**
    - Charakter von Clustern kann sich im Zeitablauf verändern
    - Für Bearbeitung aber wichtig, dass Charakter zumindest für gewisse Zeit stabil bleibt, da Maßnahmen erst auf Basis einer Clusterung entwickelt werden und sie dann auch meist eine gewisse Zeit benötigen, bis sie sich entfalten können

### 2. Bestimmung der Ähnlichkeiten 
- Ausgangspunkt Clusteranalyse: Rohdatenmatrix mit *N* Objekten (z.B.Personen), die durch *J* Variablen berschrieben werden
  - Im Inneren der Matrix objektbezogene metrische und/oder nicht metrische Variablenwerte
1. Quantifizierung der Ähnlickeit zwischen Objekten duch eine statistische Maßzahl
   - Dafür in Distanz- und Ähnlichkeitsmatrix überführt, die immer eine quadratische (NxN)-Matrix darstellt
   - Matrix enthält Ähnlichkeits- oder Unähnlichkeitswerte (Distanzwerte) zwischen den betrachteten Objekten, die unter Verwendung der objektbezogenen Variablenwerte aus der Rohdatenmatrix berechnet werden
   - Maße, die Ähnlichkeit oder Distanz zwischen Objekten ermöglichen = **Proximitätsmaße**
#### Proximitätsmaße
- **Ähnlichkeitsmaße** spiegeln die Ähnlichkeit zwischen zwei Objekten wider: Je größer
der Wert eines Ähnlichkeitsmaßes wird, desto ähnlicher sind sich zwei Objekte
- **Distanzmaße** messen die Unähnlichkeit zwischen zwei Objekten: Je größer die
Distanz wird, desto unähnlicher sind sich zwei Objekte. Sind zwei Objekte als vollkommen
identisch anzusehen, so ergibt sich eine Distanz von Null.
- Ähnlichkeits- und Distanzmaße sind komplementär d.h. Ähnlichkeit = 1-Unähnlichkeit
- In Abhängigkeit des Skalenniveaus der betrachteten Merkmale Vielzahl an Proximitätsmaßen
  - *Tab.8.4* Übersicht über Proximitätsmaße für hierarchische Clusteranalyse
- Im folgenden konzentriert auf metrisch skalierte Variablen (Wenn Variablen nominal skaliert oder liegen binäre Variablen vor, so ändern sich auch die Maße die zur Bestimmung der Proximität herangezogen werden können
##### Proximitätsmaße bei metrisch skalierten Variablen (genauer angucken, wenn nötig)
- **Einfache und quadrierte Euklidische Distanz (L2-Norm)** 
- **City-Block-Metrik (L1-Norm)**
- **Minkowski-Metrik (L-Normen)**
- **Pearson Korrelationskoeffizient als Ähnlichkeitsmaß**

### 3. Auswahl des Fusionierungsalgorithmus
- Die gewonnene Distanz- o. Ähnlichkeitsmatrix (mit Hilfe von Proximitätsmaßen ermittelt) bildet nun Ausgangspunkt der Clusteralgorithmen, die Zusammenfassung der Objekte zum Ziel haben
- Breites Spektrum an Algorithmen zur Grupierung einer gegebenen Objektmenge
- Vorteil Clusteranalyse: Simultan können eine Vielzahl an Variablen zur Gruppierung der Objekte herangezogen werden
- Einteilung der Clusterverfahren lässt sich entsprechend der Vorgehensweise im Fusionierungsprozess vornehmen
#### Clusterverfahren Übersicht
- **Hierarchische Verfahren**
  - Bei praktischen Anwendungen große Bedeutung beizumessen
  - **Agglomerative Verfahren**
    - Große praktische Bedeutung
    - Ausgangspunkt: Feinste Partition (Entspricht der Anzahl der Untersuchungsobjekte)
      - Somit charakterisiert durch Zusammenfassung von Gruppen
    - Link zwischen Gruppen (beliebiges Skalenniveau)
    - Link innerhalb Gruppen (beliebiges Skalenniveau)
    - Nächstgelegener Nachbar (Single Linkage) (beliebiges Skalenniveau)
    - Entferntester Nachbar (Complete Linkage) (beliebiges Skalenniveau)
    - Zentroid-Clustering (metrisches Skalenniveau (bevorzugt: Quadr. Euklid))
    - Median-Clustering (metrisches Skalenniveau (bevorzugt: Quadr. Euklid))
    - Ward-Methode (metrisches Skalenniveau (bevorzugt: Quadr. Euklid))
  - **Divisive Verfahren**
    - Ausgangspunkt: Gröbste Partition (alle Untersuchungsobjekte befinden sich in einer Gruppe)
      - Somit charakterisiert durch Aufteilung einer Gesamtheit in Gruppen
- **Partionierende Verfahren**
  - **Teilungsverfahren**
    - Optimales Verfahren
    - Paralleles Verfahren
  - **Minimal-Distanz-Verfahren (K-Means)**
    - Sequentielles Verfahren
    - Paralleles Verfahren

#### Ablaufschritte der hierarchisch-agglomerativen Verfahren
- 

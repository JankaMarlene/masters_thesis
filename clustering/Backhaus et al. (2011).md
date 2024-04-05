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
    - **Verfahren, die in Praxis häufig zur Anwendung kommen**:
      - Link zwischen Gruppen (Alle Proximitätsmaße möglich) (beliebiges Skalenniveau)
        - Charakteristiker: Konservativ, alle Proximitätsmaße verwendbar
      - Link innerhalb Gruppen (Alle Proximitäsmaße möglich) (beliebiges Skalenniveau)
        - Charakteristiker: Konservativ, alle Proximitätsmaße verwendbar
      - Nächstgelegener Nachbar (Single Linkage) (Alle Proximitäsmaße möglich) (beliebiges Skalenniveau)
        - Charakteristiker: Kontrahierend, neigt zur Kettenbildung, alle Proximitätsmaße verwendbar
      - Entferntester Nachbar (Complete Linkage) (Alle Proximitäsmaße möglich) (beliebiges Skalenniveau)
        - Charakteristiker: Dilatierend, neigt zu kleinen Gruppen, alle Proximitätsmaße verwendbar
      - Zentroid-Clustering (Nur sinnvoll Verwendung Distanzmaß) (metrisches Skalenniveau (bevorzugt: Quadr. Euklid))
        - Charakteristiker: Konservativ, nur Distanzmaße als Proximitätsmaße verwendbar
      - Median-Clustering (Nur sinnvoll Verwendung Distanzmaß) (metrisches Skalenniveau (bevorzugt: Quadr. Euklid))
        - Charakteristiker: Konservativ, nur Distanzmaße als Proximitätsmaße verwendbar
      - Ward-Methode (Nur sinnvoll Verwendung Distanzmaß) (metrisches Skalenniveau (bevorzugt: Quadr. Euklid))
        - Skalenniveau scheint doch auch bei den letzten drei Verfahren beliebig sein zu können. Wichtig ist nur, dass die verwendeten Proximitätsmaße auf das Skalenniveau der Daten (metrisch o. nicht-metrisch) abgestimmt werden
        - Charakteristiker: Konservativ, Bildet etwa gleich große Gruppen, nur Distanzmaße als Proximitätsmaße verwendbar
  - **Divisive Verfahren**
    - Ausgangspunkt: Gröbste Partition (alle Untersuchungsobjekte befinden sich in einer Gruppe)
      - Somit charakterisiert durch Aufteilung einer Gesamtheit in Gruppen
    - Eher geringe Bedeutung
- **Partionierende Verfahren**
  - **Teilungsverfahren**
    - Optimales Verfahren
    - Paralleles Verfahren
  - **Minimal-Distanz-Verfahren (K-Means)**
    - Sequentielles Verfahren
    - Paralleles Verfahren

#### Ablaufschritte der hierarchisch-agglomerativen Verfahren
- Konkrete Ablauf eines Fusionierungsprozessses wird i.d.R. anhand einer Tabelle (sog. Zuordnungsübersicht) und auch grafisch mittels Dendrogramm o. Eiszapfendiagramm verdeutlicht
- **Startpunkt**: Gestartet wird mit feinster Partition, d.h. jeses Objekt stellt ein Cluster dar. Bei *N* Objekten bestehen *N* Ein-Objekt-Cluster
- **Schritt 1**: Für die auf einer Fusionierungsstufe enthaltenen Objekte (Cluster) werden die paarweisen Distanzen bzw. Ähnlichkeiten zwischen den Objekten (Clustern) berechnet
- **Schritt 2**: Die beiden Objekte (Cluster) mit geringsten Distanz (o. größten Ähnlichkeit) gesucht & zu neuem Cluster zusammengefasst. Zahl der Objekte bzw. bisher gebildeten Gruppen nimmt damit um 1 ab.
- **Schritt 3**: Abstände zwischen den neuen und den übrigen Objekten/Gruppen werden berechnet, wodurch sich die sog. **reduzierte Distanzmatrix** ergibt
- **Schritt 4**: Schritte 2 & 3 werden so lange wiederholt, bis alle Untersuchungsobjekte in einer Gruppe enthalten sind (sog. Ein-Cluster-Lösung). Bei *N* Objekten werden insgesamt *N*-1 Fusionierungsschritte durchlaufen
##### Unterschiede in der Distanzberechnung hierarchisch-agglomerativer Clusterverfahren
- Unterschied liegt in Art und Weise, wie Distanz zwischen einem Objekt (Cluster) *R* & dem neuen Cluster (*P*+*Q*) in **Schritt 4** gebildet wird
- Sind zwei Objekte (Cluster) *P* & *Q* zu vereinigen, so ergibt sich die Distanz *D*(*R;P*+*Q*) zwischen irgendeiner Gruppe R & der neuen Gruppe (*P*+*Q*) durch folgende Transformation *(vgl. Kaufmann & Rousseeuw 2005, S. 225 ff.; Steinhausen & Langer 1977, S.76)*:
  - *D*(*R;P*+*Q*) = *A*x*D*(*R,P*) + *B*x*D*(*R,Q*) + *E*x*D*(*P,Q*) + *G*x|*D*(*R,P*)-*D*(*R,Q*)|
    - mit *D*(*R,P*): Distanz zwischen Gruppe *R* & *P*
    - mit *D*(*R,Q*): Distanz zwischen Gruppe *R* & *Q*
    - mit *D*(*P,Q*): Distanz zwischen Gruppe *P* & *Q*
    - Größen A, B, E & G sind **Konstanten**, die je nach verwendetem Algorithmus variieren
##### Verdeutlichung des Fusionierungsprozesses in einer Zuordnungsübersicht (Agglomeration Schedule)
- In Tabelle wird für jeden Fusionierungsschritt aufgezeigt, wekche beiden Objekte bzw. Cluster auf welchem **Heterogenitätsniveau** zusammengefasst werden
- Wir auch angegeben, auf welcher Stufe das gebildete Cluster als nächstes betrachtet wird
##### Verdeutlichung des Fusionierungsprozesses mithilfe eines Dendrogramms
- Verlauf eines Fusionierungsprozesses grafisch veranschaulicht
- Gibt an, mit welchem **Heterogenitätsmaß** eine bestimmte Anzahl an Cluster verbunden ist
  - Bei hierarchischen Clusterverfahren deshlab i.d.R. auf vertikalen Achse alle in einer Untersuchung betrachteten Objekte aufgelistet
  - Im Ausgangspunkt **Heterogenitätsmaß** von "0" verbunden, da ja jedes Objekt ein eigenes Cluster bildet
  - Mit fortschreitender Fusionierung steigt dann auch das **Heterogenitäsmaß**, wobei das Dendrogramm grafisch dijenigen Objekte verbindet, die auf einer bestimmten Fusionierungsstufe miteinander verbunden werden

##### Fusionierungseigenschaften/Charakteristiker 
- **Dilatierende Verfahren**: Neigt dazu, die Objekte verstärkt in einzelne etwa gleich große Gruppen zusammenzufassen. Außreiser werden nicht entdeckt und können zur Verzerrung des Gruppierungsprozesses führen, daher müssen Ausreißer vorher eliminiert werden
- **Kontrahierende Algorithmen**: Tendieren dazu, zunächst wenige große Gruppen zu bilden, denen viele kleine gegenüberstehen. Geeignet um "Ausreißer" in einem Objektraum zu identifizieren
- **Konservative Verfahren**: Wenn weder Tendenzen zur Dilatation noch zur Kontraktion aufweisen
- **Kettenbildung**: Im Fusionierungsprozess werden primär einzelne Objekte aneinandergereiht & so eher große Gruppen gebildet. "Schlecht" getrennte Gruppen können dadurch nur schwer aufgedeckt werden
  
##### Single Linkage, Complete Linkage und Ward-Verfahren 
- **Single Linkage** (In Abschnitt 8.2.3.2.1. genau erklärt)
  - (Nächstgelegener Nachbar)
  - Kleinste Distanz zwischen den Mitgliedern aus zwei Gruppen/Objekten
  - Charakteristiker: Kontrahierend, neigt zur Kettenbildung, alle Proximitätsmaße verwendbar
- **Complete Linkage** (In Abschnitt 8.2.3.2.2. genau erklärt)
  - (Entferntester Nachbar)
  - Größte Distanz zwischen den Mitgliedern aus zwei Gruppen/Objekten
  - Charakteristiker: Dilatierend, neigt zu kleinen Gruppen, alle Proximitätsmaße verwendbar
- **Ward-Methode** (In Abschnitt 8.2.3.2.3. genau erklärt)
  - (Minimum-Varianz)
  - Geringster Anstieg der Fehlerquadratsumme (Varianzkriterium)
  - In Praxis große Verbreitung gefunden
  - Unterscheidet von Linkage-Verfahren sich nicht nur durch Art der neuen Distanzbildung, sondern auch durch Vorgehensweise bei Fusion von Gruppen
  - Es werden nicht die Gruppen zusammengefasst, die die geringste Distanz aufweisen, sondern es werden die Objekte (Gruppen) vereinigt, die die Varianz (Fehlerquadratsumme) in einer Gruppe möglichst wenig erhöhen
  - Charakteristiker: Konservativ, Bildet etwa gleich große Gruppen, nur Distanzmaße als Proximitätsmaße verwendbar
  - Untersuchung von *Bergs (1981, S.96 f.)*: Im Vergleich in den meisten Fällen sehr gute Partitionen findet & Elemente den Gruppen meist auch korrekt zuordnet
  - Sehr guter Fusionierungsalgorithmus, wenn (*vgl. Milligan 1980, S 332 ff.; Punj & Stewart 1983, S. 141 ff.*):
    - Verwendung eines Distanzmaßes ein (inhaltlich) sinnvolles Kriterium zur Ähnlichkeitsbestimmung darstellt
    - Alle Variablen auf metrischem Skalenniveau gemessen wurden
    - Keine Ausreißer in einer Objektmenge enthalten sind bzw. vorher eliminiert wurden
    - Variablen unkorreliert sind
    - Zu erwarten ist, dass die Elementzahl in jeder Gruppe ungefährt gleich groß ist (Nicht in der Lage, langgestreckte Gruppen oder solche mit kleiner Elementzahl zu erkennen)
    - Die Gruppen in etwa die gleiche Ausdehnung besitzen

### 4. Bestimmung der Clusterzahl
- Bestimmung der Clusterzahl sollte sich an statistischen Kriterien orientieren und nicht sachlogisch (im Hinblick auf den Gruppen zugeordneten Fällen) begründet werden
- Bei Entscheidung besteht immer Zielkonflikt zwischen "Homogenitätsanforderung an die Cluster-Lösung" & der "Handhabbarkeit der Cluster-Lösung"
  - Zum Lösen können auch sachlogische Überlegungen herangezogen werden, die sich allerdings nu auf Anzahl der zu wählenden Cluster bezieht und nicht an den in den Clustern zusammengefassten Fällen ausgerichtet sein sollten

#### Verschiedene Möglichkeiten, um optimale Clusteranzahl zu bestimmen
- **Analyse von Scree-Plot & Elbow-Kriterium**
  - Bei Konstruktion des entsprechenden Diagramms sollte Ein-Cluster-Lösung nicht berücksichtigt werden
    - Beim Übergang von Zwei- zur Ein-Cluster-Lösung immer größter Heterogenitätssprung & sich daher bei dessen Berücksichtigung bei nahezu allen Anwenndungsfällen ein Elbow herausbildet
  - Hängt stark von subjektiven Einschätzung der/des Anwender:in ab
- **Regeln zur Bestimmung Clusterzahl**
  - statistisch und weitgehend objektive Anhaltspunkte zur Bestimmung optimaler Clusteranzahl bei Anwendung hierarchishen Clusteranalysen liefern
  - **Regel nach Calinski/Harabasz**
    - Metrische Merkmale
    - Analogie zur Varianzanalyse
  - **Test von Mojena**
    - Relativ einfach selbst durchführbar mit Tabellenkalkulation
  - **Optimierung einer Clusterlösung mit Hilfe von K-Means**
    - Ist finale Clusterzahl gefunden, kann weiterhin geprüft werden, ob sich durch die Verschiebung von Objekten innerhalb der gefundenen Cluster eine Verbesserung (Streuung innerhalb der Gruppe möglichst klein und zwischen Gruppen möglichst groß) der Lösung erreicht werden kann
      - Dafür wird häufig auf **K-Means-Clusteranalyse** zurückgegriffen

#### Beurteilung von Robustheit und Güte einer Cluster-Lösung
- Nachdem Clusteranzahl bestimmt stellt sich die Frage, wie robust die gefundene Clusterlösung ist
- Gibt selten objektive Vergleichskriterien, die bei der Clusteranalyse zur Robustheitsprüfung herangezogen werden können (**Bei mir eventuell schon, ich habe ja die subjektive Einschätzung**)
1. Ausreißer eliminieren
2. Wie sensibel sind Ergebnisse, wenn unterschiedliche Methoden zur Clusterung herangezogen werden
   - Nur Methoden der gleichen Kategorie von Clusterverfahren können vergleichend betrachtet werden
   - Untersceiden sich Ergebnisse nicht/nur wenig = robuste Lösung
- **Split Half-Verfahren**
  - Daten werden zufällig in 2 Stichproben aufgeteilt
    - Für jede Gruppe mit gleichem Verfahren eine Clusteranalyse durchführen
      - Gleiche/ähnliche Clusterstruktur? -> Robustheit
- Zur Beurteilung der Güte **Diskriminanzanalyse**
  - Ergibt meist relativ hohe Güte
    - Da die zur Clusterung herangezogenen Variablen & die unabhängigen Variablen der Diskriminanzanalyse identisch sind

### 5. Interpretation einer Cluster-Lösung
- Sollte sich an Auspregung der Clustervariablen in den ermittelten Clustern orientieren
- Dabei Sinnvoll Vergleich mit Erhebungsgesamtheit vorzunehmen
- Hilfreich dafür Berechnung von **t- und F-Werten**
  - **t-Werte**
    - Negative t-Werte = Variable in betrachteter Gruppe im Vergleich zur Erhebungsgesamtheit unterrepräsentiert
    - Positive t-Werte = Variablen überrepräsentiert
    - Dienen nicht zur Beurteilung der Güte, nur für Charakterisierung
- Zur Analyse der Unterschiede zwischen gefundenen Clustern ist zusätzlich Durchführung **Diskriminanzanalyse** sinnvoll
  - Gefundene Cluster = abhängige Variable (nominal skaliert)
  - Zur Clusterung herangezogene Größen = unabhängige Variablen (metrisch skaliert)
  - Welche Variablen in besonderer Weise für Trennung zwischen Clustern verantwortlich?

### Empfehlungen zum Ablauf einer hierarchischen agglomerativen Clusteranalyse
1. Single Linkage-Verfahren um Ausreißer zu identifizieren
2. Eliminierung der Ausreißer & anschließende Anwendung weiteren Verfahrens (z.B. Ward) auf reduzierten Datensatz
   - Auswahl des Verfahrens hat vor Hintergrund der jeweiligen Anwendungssituation & den Fusionierungseigenschaften der Clustermethodik zu erfolgen
3. Optimierung der in Schritt 2 gefundenen Clusterlösung mithilfe des K-Means-Verfahrens
4. Beurteilung der Robustheit einer Clusteranalyse & inhaltliche Interpretation der Ergebnisse

### Durchführung einer Cluster Analyse mit SPSS in Abschnitt 8.3.2. beschrieben
- Starke Anleitung

### 8.3.4. SPSS-Kommandos
- Falls ich die brauche
- Auch R-Befehle (*Seite 550*)

## Proximitätsmaße bei nicht metrischen Daten
### Nominales Skalenniveau
- 2 Möglichkeiten, Variablen in Clusteranalyse zu berücksichtigen:
  1. Transformation in binäre Variablen
     - Alle Ausprägungen werden als eigenständige Binär-Variable betrachtet & mit 0/1 kodiert. 1 = "Merkmalsausprägung vorhanden"; 0 = "Merkmalsausprägung nicht vorhanden"
  2. Analyse von Häufigkeitsdaten
     - Da keine Rechenoperationen möglich sind, werden häufig Häufigkeiten angegeben, mit der Variable in Erhebung auftritt
     - Lässt sich prüfen, ob zwischen nominalen Variablen eine statistische Abhängigkeit besteht
### Binäre Variablen
- Relativ gut hier beschrieben, mit vielen Möglichkeiten, aber für mich erstmal nicht interessant
### Gemischt skalierte Variablenstruktur
- Clusteranalytische Verfahren verlangen kein spezielles Skalenniveau der Merkmale
- Wie können Merkmale mit unterschiedlichem Skalenniveau gemeinsam Berücksichtigung finden? Vorgehensweise:
  1. Getrennte Berechnung für metrische und nicht-metrische Variablen
     - Getrennte Berechnung von **Ähnlichkeitskoeffizienten bzw. Distanzen**
     - Gesamtähnlichkeit ermittelt sich dann als ungewichteter o. gewichteter Mittelwert der im vorherigen Schritt berechneten Größe
  2. Transformation auf ein niedrigeres Skalenniveau
     - Eine Möglichkeit zur Umwandlung der vorliegenden Verhältnisskalen in binäre Skalen besteht in **Dichotomisierung**
       - Hierbei ist Schwelle festzulegen, die zu einer Trennug z.B. niedrig- und hochpreisig (kognitiv eingeschränkt vs. nicht) führt. Alle die unter liegen = 0, alle drüber = 1
       - Einfach und schnell anwendbar aber hoher Informationsverlust & Problem beim Schwellen festlegen
       - Kann aber auch Intervalle bilden und Intervall binär kodieren (besser, aber ja auch etwas willkührlich oder?)
  3. Einteilung in Klassen
     - Bildung von Klassen bzw. Intervallen

## Zentrale partitionierende Clusterverfahren
- Hierarchische, agglomerative Clusterverfahren große Bedeutung in Anwendungspraxis
  - Allerdings schnell an rechentechnische Grenzen, vorallem wenn große Fallzahl
    - Ausweg aus Problematik: **Partitionierende Clusterverfahren**
      - Gehen von einer vorgegebenen Gruppierung der Objekte aus (Startpartition) und nehmen dann im Hinblick auf bestimmtes Zielkriterium eine sukzessive Verbesserung dieser Startpartition vor
        - Dabei werden einzelnen Objekte mithilfe eines Austauschalgorithmus zwischen Gruppen so lange umsortiert, bis vorgegebene Zielkriterium erfüllt ist
      - Vorteil: Während Fusionierungsprozesses können Elemente zwischen den Gruppen getauscht werden (Bei hierarchisch nicht möglich, eine einmal gebildete Gruppe kann nicht mehr aufgelöst werden im Fusionierungsprozess)
        - Größere Flexibilität
      - **K-Means-Clusteranalyse (KM-CA)** & **Two-Step-Clusteranalyse (TS-CA)**
        - Gerade bei großen Datenmengen deutliche Vorteile gegenüber den hierarchischen Verfahren bieten
        - Effiziente Rechenmethoden, um Gruppen in großem Datensatz zu erkennen
### K-Means Clusteranalyse
- Geht im Ausgangspunkt von einer Partitionierung eines Datensatzes in *k* Cluster aus
  - Dabei wird jedes Cluster durch dessen "Schwerpunkt" (Gruppen-Zentroid) repräsentiert
    - Durch Mittelwertbildung der dem Cluster *i* zugeordneten Fälle errechnet
  - Formeln im *Abschnitt 8.4.2.1*
#### Vorgehensweise
1. Zufällige Festlegung von k initialen Clusterzentren
2. Zuordnung der Fälle (Datenpunkten) zu den initialen Clusterzentren in Abhängigkeit der Cluster-Varianzen
   - **Euklidische Distanz** wird zwischen allen Datenpunkten und den Clusterzentren berechnet
   - Dann: Datenpunkt wiwrd demjenigen Cluster zugeordnet, bei dem das sog. **Varianzkriterium** am wenigsten vergrößert wird
3. Neuberechnung der Clusterzentren
   - Neu berechnetes Clusterzentrum ergibt sich als Mittelwert der zu einem Cluster *i* gehörenden Datenpunkte
   - Anschließend werden mit neuen Clusterzentren erneut Cluster-Varianzen (wie in Schritt 2) berechnet & geprüft, ib eine Verringerung der Cluster-Varianzen erreicht werden konnte
     - So wird kontrolliert, ob Datenpunkte nicht doch besser einem anderen Cluster zugeordnet werden sollten
4. Prüfung eines Konvergenzkriteriums
   - Schritt 2 & 3 so lange wiederholen, bis die Varianzen in den Clustern durch anderen Zuordnungen der Fälle nicht mehr verringert werden können
   - Sobald sich Varianzkriterium nicht mehr iterativ verkleinern lässt, sind die finalen Clusterzentren in Form der Gruppenmittelwerte gefunden
     - Wert gilt als "Repräsentant" eines Clusters & dient zur Beschreibung des Clusters
    - Zur Interpretation der Cluster in SPSS sog. ANOVA-Tabelle angefordert werden, die für jede Variable den sog. F-Wert & zugehörige Signifikanzniveau ausweist
      - Allerdings werden die beobachteten Signifikanzniveaus nicht korrigiert und können daher auch nicht als statistische Tests für die Hypothese der Gleichzeit der Clustermittelwerte interpretiert werden
      - Daher nur beschreibende Funktion und geben nur Hinweis darauf, ob sich Ausprägungen der Variablen in den Clustern unterscheiden
      - Um Stabilität einer gefundenen Lösung zu prüfen, die KM-CA mehrmals durchführen, bei denen die Fälle in jeweils unterschiedlicher, zufällig ausgewählter Reihenfolge zugeordnet werden
      - Berechnung der Clusterzentren hängt von subjektiven initialen Einteilung der Cluster ab
        - Daher Gefahr, dass Lösung zwar ein lokales Optimun, jedoch kein globales Optimum darstellt
        - Daher versch. initiale Einteilungen ausprobieren & Ergebnisse miteinander zu vergleichen
- Durchführung in SPSS beschrieben in *Abschnitt 8.4.2.1.2.*
### Two-Step Clusteranalyse
- Große Datenmengen
- Kann Variablen mit unterschiedlichem Messviveau verarbeiten
- Ausreißer im Datensatz können erkannt werden
- Optimale Anzahl an Clustern kann bestimmt werden
- **Robustes Clusterverfahren**:
  - reagiert nicht sehr empfindlich auf Verletzungen von Annahmen
  - insgesamt leicht interpretierbare Ergebnisse erzeugt
#### Vorgehensweise
1. Stufe
   - Die Fälle im Ausgangsdatensatz werden den Knoten eines Entscheidungsbaums zugeordnet (sog. **Cluster Feature-Tree; CF-Tree**)
   - Anzahl der Knoten kann vorgegeben werden o. durch SPSS automatisch erzeugt werden
   - Im Ausgangspunkt sind zunächst alle Fälle in einem Knoten enthalten
     - Dieser wird dann nach Muster eines Entscheidungsbaums auf nächsten Ebene in weitere Knoten unterteilt, denen die ursprünglichen Fälle basierend auf ihren Ähnlichkeiten zugeordnet werden
     - Aufteilung in weitere Ebenene wird sukzessive fortgesetzt
     - Pro Ebene können maximal 8 Knoten gebildet werden
     - Auf 3. Ebene werden die in einem Schlussknoten enthaltenen Datenpunkte jeweils durch Mittelwertbildung zu einem Fall zusammengefasst
     - Ausreißer können identifiziert werden, die am Ende in einem eigenen Schlussknoten zusammengefasst werden
2. Stufe
   - Auf Schlussknoten der ersten Stufe (ohne Ausreißer) wird **hierarchische Clusteranalyse** angewandt
   - Alle Schlussknoten metrisch? Zur Distanzberechnung **Euklidische Distanz**
   - Unterschiedliches Skalenniveau? Wahrscheinlichkeitstheoretischen Modellansatz der **Log-Likelihood-Distanz** nutzen (Auch bei rein metrisch skalierten Variablen verwendbar
- Durchführung mit SPSS beschrieben in *Abschnitt 8.4.2.2.2.**
### Vergleich K-Means & Two-Step
- Skalenniveau der Variablen
  - K-Means: Nur metrisch
  - Two-Step: Metrisch & nominal
- Finale Clusterzahl
  - K-Means: Vom Anwender festzulegen; zufällig v. SPSS bestimmt
  - Two-Step: Vom Answender festzulegen; optimale Zahl durch SPSS bestimmbar
- Ausreißer
  - K-Means: Müssen vorab identifiziert werden
  - Two-Step: Automatisch identifiziert und verarbeitet
- Reihenfolge der Eingabedaten
  - K-Means: Kann Ergebnis beeinflussen
  - Two-Step: Keine Auswirkungen auf Ergebnis
- Verletzung der Annahmen
  - K-Means: Relativ robust gegenüber Annahmeverletzungen
  - Two-Step: Relativ robust gegenüber Annahmeverletzungen
### Problematiken der partitionierenden Verfahren
- Ergebnisse verstärkt durch die der "Umordnung" der Objekte zugrunde liegenden Zielfunktion beeinflusst
- Wahl der Startpartition ist häufig subjetiv begründet & kann ebenfalls Ergebnisse beeinflussen
- Nur lokale & keine globalen Optima
- Sind das nicht eher alles Probleme des K-Means & nicht der Two-Step-Analyse?

## Anwendungsempfehlung
- **Ward-Verfahren** hervorzuheben (Studie von *Bergs 1981, S.97*)
- Ablaufschritte & Entscheidungsprobleme der  Clusteranalyse grafisch dargestellt 

## Bei Darstellung der Ergebnisse folgende Fragen begründen
1. Welches Ähnlichkteismaß und welcher Algorithmus wurden gewählt?
2. Was waren die Gründe für die Wahl?
3. Wie stabil sind die Ergebnisse bei
   - Veränderung des Ähnlichkeitsmaßes
   - Wechsel des Algorithmus
   - Veränderung der Gruppenzahl?

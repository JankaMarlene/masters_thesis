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
  - **Vergleichbarkeit der Messdimensionen**
  - **Beeinflussbarkeit**
  - **Trennkraft**
  - **Repräsentativität**
  - **Clusterstabilität**

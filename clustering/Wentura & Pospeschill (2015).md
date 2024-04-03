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
  - 
  

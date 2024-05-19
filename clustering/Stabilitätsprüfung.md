# Stabilitätsprüfung

## Informationen aus Bacher &  Wenzing, 2010
- Mit Stabilitätsanalysen werden die im Rahmen der Lösung einer Klassifikation getroffenen "unsicheren" Entscheidungen untersucht
  - Entscheidungen, die nicht eindeutig inhaltlich &/o. empirisch begründet werden können (Ich kann sie ja aber inhaltlich interpretieren!?)
- Es wird untersucht, wie stark sich die Ergebnisse ändern, wenn geringfügige Modifikationen in Daten &/o. in getroffenen Spezifikation des gewählten Verfahrens vorgenommen werden
  - Führen geringfügige Änderungen zu stark abweichenden Ergebnissen, liegt Instabilität vor und eine Fehleranalyse (*Seite 27 ff.*) ist durchzuführen
- **RAND-Index** (siehe *Abschnitt 9.5*)
  - Maßzahl zur Messung der Ähnlichkeit der einzelnen Lösungen
  - Gibt Prozentsatz der übereinstimmenden Zuordnungen an
  - Werte größer 0.7 bzw. 70% können als ausreichende Übereinstimmung interpretiert werden (*Dreger 1986; Fraboni & Saltstone 1992*)
  - Bei **adjusted RAND-Index** von über 0.224 stabil
    - Ermöglicht feinere Differenzierung von sehr guten Lösungen
- Gründe für Instabilität:
  1. Variabilität der Daten
  2. Auswahl der Variablen
  3. Auswahl Abstandmaße & Methode
  4. Optimierung der Anzhal der Cluster
  5. Interpretation der Ergebnisse
- Formale Voraussetzung für Gültigkeit von stabiler Clusterlösung
  1. Mindestanzahl an Dimensionen entschieden
     - Ändert sich Ergebnisse, wenn anstelle von 5 Dimensionen mehr o. weniger verwendet werden?
  2. Welches Verfahren?
  3. Ist z.B. 14- o. 6-Clusterlösung stabiler?

## (In)stabilität in meiner Clusterlösung

<table>
  <thead>
    <tr>
      <th></th>
      <th>without PCS</th>
      <th>with PCS</th>
    </tr>
  </thead>
  <tbody>
    <tr>
      <td>Cluster 1</td>
      <td>11</td>
      <td>38</td>
    </tr>
    <tr>
      <td>Cluster 2</td>
      <td>16</td>
      <td>5</td>
    </tr>
  </tbody>
</table>

- Für Clusterlösung wurde 'ward' Verfahren verwendet

### Vergleich 2-Clusterlösung 'ward' mit anderen
- Verwendung von RAND-Index
- Wie viel Übereinstimmung haben die Verfahren mit 'ward' Methode? Angabe in %

<table>
  <thead>
    <tr>
      <th></th>
      <th>ward</th>
    </tr>
  </thead>
  <tbody>
    <tr>
      <td>average</td>
      <td>50%</td>
    </tr>
    <tr>
      <td>single</td>
      <td>55%</td>
    </tr>
    <tr>
      <td>complete</td>
      <td>49%</td>
    </tr>
    <tr>
      <td>centroid</td>
      <td>56%</td>
    </tr>
    <tr>
      <td>median</td>
      <td>55%</td>
    </tr>
  </tbody>
</table>

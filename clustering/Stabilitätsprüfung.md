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
- Wie viel Übereinstimmung haben die Verfahren mit 'ward' Methode?

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

- Leider also keine stabile Clusterlösung

### Was passiert, wenn ich statt Winsorizing die Ausreißer einfach entferne?
- Winsorizing:
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

- Ausreißer entfernt:
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
      <td>8</td>
      <td>30</td>
    </tr>
    <tr>
      <td>Cluster 2</td>
      <td>18</td>
      <td>6</td>
    </tr>
  </tbody>
</table>

Vergleich:

<table>
  <thead>
    <tr>
      <th></th>
      <th>2 Cluster</th>
      <th>3 Cluster</th>
      <th>4 Cluster</th>
      <th>6 Cluster</th>
    </tr>
  </thead>
  <tbody>
    <tr>
      <td>complete/ward</td>
      <td>63%</td>
      <td>71%</td>
      <td>89%</td>
      <td>88%</td>
    </tr>
    <tr>
      <td>average/ward</td>
      <td></td>
      <td></td>
      <td>73%</td>
      <td></td>
    </tr>
    <tr>
  </tbody>
</table>

- Bei mehr Clustern (bis zur bestimmten Anzahl) wird Lösung stabiler
- 2 Clusterlösung ist sowohl bei Verwendung von Winsorizing als auch Entfernung von Ausreißern instabil

### Bei welcher Clusteranzahl liegt Stabilität vor?
- Unter Verwendung von Winsorizing 
- Vergleich:

<table>
  <thead>
    <tr>
      <th></th>
      <th>2 Cluster</th>
      <th>3 Cluster</th>
      <th>4 Cluster</th>
      <th>5 Cluster</th>
      <th>6 Cluster</th>
    </tr>
  </thead>
  <tbody>
    <tr>
      <td>complete/ward</td>
      <td>49%</td>
      <td>72%</td>
      <td>69%</td>
      <td>70%</td>
      <td>69%</td>
    </tr>
    <tr>
      <td>average/ward</td>
      <td>51%</td>
      <td>48%</td>
      <td>77%</td>
      <td>80%</td>
      <td>78%</td>
    </tr>
    <tr>
      <td>median/ward</td>
      <td>51%</td>
      <td>53%</td>
      <td>55%</td>
      <td>57%</td>
      <td>54%</td>
    </tr>
    <tr>
      <td>centroid/ward</td>
      <td>56%</td>
      <td>50%</td>
      <td>42%</td>
      <td>44%</td>
      <td>45%</td>
    </tr>
    <tr>
      <td>single/ward</td>
      <td>55%</td>
      <td>39%</td>
      <td>33%</td>
      <td>33%</td>
      <td>30%</td>
    </tr>
    <tr>
  </tbody>
</table>

-> Nochmal die anderen Verfahren in die Tabelle mit aufnehmen

### Bei welcher Clusteranzahl liegt Stabilität vor?
- Ohne Winsorizing 
- Vergleich:

<table>
  <thead>
    <tr>
      <th></th>
      <th>2 Cluster</th>
      <th>3 Cluster</th>
      <th>4 Cluster</th>
      <th>5 Cluster</th>
      <th>6 Cluster</th>
    </tr>
  </thead>
  <tbody>
    <tr>
      <td>complete/ward</td>
      <td>63%</td>
      <td>71%</td>
      <td>89%</td>
      <td>82%</td>
      <td>88%</td>
    </tr>
    <tr>
      <td>average/ward</td>
      <td>51%</td>
      <td>74%</td>
      <td>74%</td>
      <td>73%</td>
      <td>73%</td>
    </tr>
    <tr>
      <td>median/ward</td>
      <td>53%</td>
      <td>35%</td>
      <td>35%</td>
      <td>39%</td>
      <td>34%</td>
    </tr>
    <tr>
      <td>centroid/ward</td>
      <td>49%</td>
      <td>43%</td>
      <td>43%</td>
      <td>35%</td>
      <td>30%</td>
    </tr>
    <tr>
      <td>single/ward</td>
      <td>51%</td>
      <td>37%</td>
      <td>38%</td>
      <td>31%</td>
      <td>29%</td>
    </tr>
    <tr>
  </tbody>
</table>

### Vergleich verschiedenen Distanzmaße
- Einfache & quadrierte Euklidische Distanz ('euclidean') -> Verwendet in Clusterlösung (einfache oder quadrierte?)
- City-Block-Metrik ('manhattan')
- Minkowski-Metrik ('minkowski')
- Pearson Korrelationskoeffizient ('pearson')
- RAND-Index zur Berechnung der prozentualen Übereinstimmung der Lösungen:

<table>
  <thead>
    <tr>
      <th></th>
      <th>manhattan</th>
      <th>minkowski</th>
      <th>pearson</th>
    </tr>
  </thead>
  <tbody>
    <tr>
      <td>euclidean</td>
      <td>51%</td>
      <td>100%</td>
      <td>100%</td>
    </tr>
    </tr>
  </tbody>
</table>

- Das sieht doch ganz gut aus!

### Vergleich der Variablen

#### Alle Variablen verwendet (PVT, nback_miss_1, nback_miss_2, tmt_a_time, tmt_b_time)
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

- 54/70 Personen "richtig" zugeordnet

#### Ohne PVT
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
      <td>22</td>
      <td>19</td>
    </tr>
    <tr>
      <td>Cluster 2</td>
      <td>5</td>
      <td>24</td>
    </tr>
  </tbody>
</table>

- 46/70 Personen "richtig" zugeordnet (unter vorbehalt, da ich mir die Cluster nicht im Hinblick auf ihre Ausprägungen in den kognitiven Variablen angeguckt habe)

#### Ohne TMT-A
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
      <td>24</td>
      <td>27</td>
    </tr>
    <tr>
      <td>Cluster 2</td>
      <td>3</td>
      <td>16</td>
    </tr>
  </tbody>
</table>

- 30/70 o. 40/70 Personen "richtig" zugeordnet, Keine Aussage möglich!!

#### Ohne TMT-B
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
      <td>37</td>
    </tr>
    <tr>
      <td>Cluster 2</td>
      <td>16</td>
      <td>6</td>
    </tr>
  </tbody>
</table>

- 53/70 Personen "richtig" zugeordnet (unter vorbehalt, da ich mir die Cluster nicht im Hinblick auf ihre Ausprägungen in den kognitiven Variablen angeguckt habe)

#### Ohne TMT-A & TMT-B
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
      <td>13</td>
      <td>36</td>
    </tr>
    <tr>
      <td>Cluster 2</td>
      <td>14</td>
      <td>7</td>
    </tr>
  </tbody>
</table>

- 50/70 Personen "richtig" zugeordnet (unter vorbehalt, da ich mir die Cluster nicht im Hinblick auf ihre Ausprägungen in den kognitiven Variablen angeguckt habe)

#### Ohne nback_miss1
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
      <td>8</td>
      <td>33</td>
    </tr>
    <tr>
      <td>Cluster 2</td>
      <td>19</td>
      <td>10</td>
    </tr>
  </tbody>
</table>

- 52/70 Personen "richtig" zugeordnet (unter vorbehalt, da ich mir die Cluster nicht im Hinblick auf ihre Ausprägungen in den kognitiven Variablen angeguckt habe)

#### Ohne nback_miss2
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
      <td>7</td>
      <td>32</td>
    </tr>
    <tr>
      <td>Cluster 2</td>
      <td>20</td>
      <td>11</td>
    </tr>
  </tbody>
</table>

- 52/70 Personen "richtig" zugeordnet (unter vorbehalt, da ich mir die Cluster nicht im Hinblick auf ihre Ausprägungen in den kognitiven Variablen angeguckt habe)

#### Ohne nback_miss1 & nback_miss2
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
      <td>8</td>
      <td>33</td>
    </tr>
    <tr>
      <td>Cluster 2</td>
      <td>19</td>
      <td>10</td>
    </tr>
  </tbody>
</table>

- 52/70 Personen "richtig" zugeordnet (unter vorbehalt, da ich mir die Cluster nicht im Hinblick auf ihre Ausprägungen in den kognitiven Variablen angeguckt habe)

#### Ohne TMT-B & nback_miss2
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
      <td>9</td>
      <td>32</td>
    </tr>
    <tr>
      <td>Cluster 2</td>
      <td>18</td>
      <td>11</td>
    </tr>
  </tbody>
</table>

- 50/70 Personen "richtig" zugeordnet (unter vorbehalt, da ich mir die Cluster nicht im Hinblick auf ihre Ausprägungen in den kognitiven Variablen angeguckt habe)

#### Ohne TMT-A & nback_miss1
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
      <td>13</td>
      <td>14</td>
    </tr>
    <tr>
      <td>Cluster 2</td>
      <td>31</td>
      <td>12</td>
    </tr>
  </tbody>
</table>

- 45/70 Personen "richtig" zugeordnet (unter vorbehalt, da ich mir die Cluster nicht im Hinblick auf ihre Ausprägungen in den kognitiven Variablen angeguckt habe)

#### Nur PVT
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
      <td>7</td>
      <td>27</td>
    </tr>
    <tr>
      <td>Cluster 2</td>
      <td>20</td>
      <td>16</td>
    </tr>
  </tbody>
</table>

- 47/70 Personen "richtig" zugeordnet (unter vorbehalt, da ich mir die Cluster nicht im Hinblick auf ihre Ausprägungen in den kognitiven Variablen angeguckt habe)

#### Nur nback_miss1 & nback_miss2
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
      <td>12</td>
      <td>15</td>
    </tr>
    <tr>
      <td>Cluster 2</td>
      <td>36</td>
      <td>7</td>
    </tr>
  </tbody>
</table>

- 51/70 Personen "richtig" zugeordnet (unter vorbehalt, da ich mir die Cluster nicht im Hinblick auf ihre Ausprägungen in den kognitiven Variablen angeguckt habe)

#### Nur nback_miss1
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
      <td>26</td>
      <td>1</td>
    </tr>
    <tr>
      <td>Cluster 2</td>
      <td>29</td>
      <td>14</td>
    </tr>
  </tbody>
</table>

- 30/70 o. 40/70 Personen "richtig" zugeordnet, Keine Aussage möglich!!

#### Nur nback_miss2
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
      <td>19</td>
      <td>18</td>
    </tr>
    <tr>
      <td>Cluster 2</td>
      <td>8</td>
      <td>25</td>
    </tr>
  </tbody>
</table>

- 44/70 Personen "richtig" zugeordnet (unter vorbehalt, da ich mir die Cluster nicht im Hinblick auf ihre Ausprägungen in den kognitiven Variablen angeguckt habe)
  
#### Nur TMT-A & TMT-B
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
      <td>4</td>
      <td>23</td>
    </tr>
    <tr>
      <td>Cluster 2</td>
      <td>17</td>
      <td>26</td>
    </tr>
  </tbody>
</table>

- 30/70 o. 40/70 Personen "richtig" zugeordnet, Keine Aussage möglich!!

#### Nur TMT-A
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
      <td>2</td>
      <td>13</td>
    </tr>
    <tr>
      <td>Cluster 2</td>
      <td>25</td>
      <td>30</td>
    </tr>
  </tbody>
</table>

- 32/70 o. 38/70 Personen "richtig" zugeordnet, Keine Aussage möglich!!

#### Nur TMT-B
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
      <td>22</td>
      <td>27</td>
    </tr>
    <tr>
      <td>Cluster 2</td>
      <td>5</td>
      <td>16</td>
    </tr>
  </tbody>
</table>

- 32/70 o. 38/70 Personen "richtig" zugeordnet, Keine Aussage möglich!!

-> Einmal MoCa mitreinnehmen, um zu begründen, warum ich MoCa nicht im Clustering berücksichtigt habe
-> Gruppen auch im Hinblick auf MoCa miteinander vergleichen (Wie aussagekräfigt ist er?)

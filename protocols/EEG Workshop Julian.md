# Protokoll EEG Workshop bei Julian
## Allgemein
- Bucht: Rhythms of the brain - György Buzaki
- Cheatsheet für Matlab: s01_Intro
- sc04_rest -> Für Restingstate
- Debugmode mit end beenden
- Anleitung zur Interpretation von Clustern im EEG bei FieldTrip: "How not to interpret results from a cluster based permutation test"
- Cluster nicht signifikant, sondern die Unterschiede zwischen Clustern sind signifikant 
## Inhaltlich
- 50Hz Netzbrummen
  - Strom macht uns alles über 50 Hz kaputt
- Störsignale sind ein Problem
  - z.B. Züge, Handys, Strom, etc.
- Alpha nicht nur, wenn Augen geschlossen, sondern auch bei Aufmerksamkeitsprozessen
- Stärke in alpha gibt an, ob ich Reiz wahrnehme oder nicht
  - Viel alpha, nehme eher nichts wahr -> Wie beim Schlaf
  - Wenig alpha, höhere Aufmerksamkeit
- Aufmerksamkeit
  - Lenkung von Verarbeitungsprozessen o.ä.
- Zwei Theorien zu Aufmerksamkeit in Alpha
  - Alpha = Visuell attention (ausschließlich)
  - Alpha = Insgesammt sensorische attentions (Aussage: Warum sollte alpha nur für visuelle Aufmerksamkeit gelten?)
  - Sprich: Visuelle vs. generelle Aufmerksamkeit
### Praktischer Teil EEG
- Filter erst später setzen
- Plot trial
- Besser: Artefacs drinn lassen, anstatt rausschmeißen, wenn man sich unsicher ist
  - Ob es "Scheiß" ist lernt man durch Angucken von EEG Daten
- Component analysis geht besser bei noisy data
- Segmentlänge eher zwischen 5-10 stellen
  - Falls genug überbleibt
- *_preproc -> Fasst alles zusammen, was auf _preproc endet
- Wichtig also: Gute Bezeichnungen für Variablen, etc.
- Niemals gleichen Filter 2x hintereinander benutzen
  - Filter kann nur kleiner werden, nicht größere Filter auf einmal verwenden
- firws -> Besonders geeigent für kurze Episoden von EEG Daten
- In Methodenteil angeben, welche Filter ich verwendet habe (Copy&Past aus Script)
  - Damit Script theoretisch genau nachgebaut werden kann
- Handgemacht nicht so elegant, daher bei 7.Stats in Matlab weiter machen (Wir haben bei 7.2. weitergemacht)
- Clusterkorrektur
  - ft_timelockstatistics
    - Braucht viel Input
- In etwa 5 Nachbarn pro Elektrode
  - Christian hatte 0,5 benutzt(?)
- numrandomization 1000 eine gute Zahl, 500 auch okay
## To-Dos
- Welche Frequenzbänder relevant für Kognitionen?

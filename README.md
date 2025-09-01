# Setlisten-Generator

Ein algorithmischer Setlisten-Generator, der eine musikalisch und spieltechnisch sinnvolle Konzertreihenfolge automatisch aus einer CSV-Datei mit Songdaten erstellt.

## Funktionsweise

Das Programm liest eine CSV-Datei mit Songs ein und sortiert sie unter Beachtung vordefinierter Regeln, die sich aus der Erfahrung mit meiner Band Fächerswing ergeben haben, zu einer schlüssigen Konzert-Setliste. Die finale Liste kann in der Konsole angezeigt und als neue CSV-Datei exportiert werden.

## Regeln für die Anordnung

- **Erster & letzter Song**: bleiben fest auf ihrer Position.
- **Zweiter Song**: Tempo zwischen 121–140, ≠ Tonart des ersten Songs, 2 Melodie-Instrumente.
- **Dritter Song**: Tempo zwischen 141–160, ≠ Tonart des zweiten Songs, 2 Melodie-Instrumente.
- **Vorletzter Song**: Tempo 10–30 langsamer als der letzte Song, ≠ Tonart des letzten Songs, 2 Melodie-Instrumente.
- **Mittlere Songs**:
  - Zirkulation durch sechs **Tempo-Cluster** (0–100, 101–130, 131-160, 161-190, 191-220, 221+)
  - Keine gleichen Tonarten hintereinander
  - Keine für den Ansatz ermüdenden Songs (`Ansatz = 1`) direkt nacheinander
  - Keine Solo-Stücke direkt hintereinander (außer Altsax-Stücke)
  - Altsax-Stücke (`Sax = 1`) werden gruppiert und mittig eingefügt

## Format der CSV-Datei

Die eingelesene Datei muss eine Kopfzeile und folgende Spalten enthalten:

```
Titel,Melodie-Instrumente,Tempo,Tonart,Ansatz,Sax,Solo,Position
```

Beispiel:
```csv
It Don't Mean A Thing,2,160,F,0,0,0,1
Blue Skies,1,140,Bb,1,0,1,0
```

## Installation

### Voraussetzungen

- [SBCL](http://www.sbcl.org/), [LispWorks](https://www.lispworks.com) oder eine andere Common Lisp-Umgebung

## Ausführung

### 1. Pfad zur CSV-Datei anpassen

Im Quellcode:
```lisp
(defparameter *songs* (read-songs-from-csv #P"/Pfad/zur/Datei.csv"))
```

### 2. Generiere die Setliste:

```lisp
(defparameter *final-songs*
  (arrange-songs-with-full-rules *songs*))
```

### 3. Setliste anzeigen:

```lisp
(print-song-list *final-songs*)
```

### 4. CSV-Export:

```lisp
(export-songs-to-csv *final-songs* #P"/Pfad/zum/Export.csv")
```

## Optionen

```lisp
(defparameter *show-tempo-warnings* t) ; Schaltet Tempo-Warnungen ein/aus
```

## Exportiertes Format

Die exportierte CSV-Datei enthält die folgenden Spalten:

```
Position,Titel,Besetzung,Tempo,Tonart,Kommentar
```

Beispiel:

```csv
1,It Don't Mean A Thing,Tenorsax & Posaune,160,F,
2,Blue Skies,Tenorsax,140,Bb,Ansatz
```

## Hinweise

- **Ansatz = 1** bedeutet: für das Spiel besonders anstrengend, daher wird auf ausreichenden Abstand geachtet.
- **Solo = 1 oder 2** gibt an, ob Saxophon oder Posaune solistisch ist.
- **Sax = 1** kennzeichnet Altsaxophon-Stücke, die gesondert behandelt werden.

## Lizenz

Dieses Projekt steht unter keiner offiziellen Lizenz – verwende es frei und gerne im Kontext von Bildung, Musik und Spaß
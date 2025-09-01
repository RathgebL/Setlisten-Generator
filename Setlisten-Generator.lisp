;;;; ----- SETLISTEN GENERATOR -----

;; Version: v1.1 (Stand: 2025-09-01)

#|

Beschreibung:
Mit dem Setlisten-Generator kann aus einer CSV-Liste von Songs automatisch eine musikalisch und spielerisch sinnvolle Konzertreihenfolge erstellt werden. Das Ergebnis wird uebersichtlich ausgegeben und als CSV-Datei exportiert. Die Songs werden dabei nach folgenden Regeln sortiert.

Regeln:
Erster und letzter Song: behalten ihrere vorgegeben Positionen
Zweiter Song: =/= Tonart wie erster Song, Tempo 121-140 und 2 Melodieinstrumente
Dritter Song: =/= Tonart wie zweiter Song, Tempo 141-160 und 2 Melodieinstrumente  
Mittlere Songs: Songs in Tempo-Clustern sortiert
                Zirkulation durch die verschiedenen Cluster
                Songs mit Sax-Wert = 1 zusammen (und mittig)
                Ansatzwert = 1 nicht hintereinander
                keine Soli am Stueck (ausser Altsax)
                optimaler Weise keine gleichen Tonarten hintereinander 
Vorletzter Song: =/= Tonart wie letzter Song, Tempo 10-30 weniger als letzter Song und 2 Melodieinstrumente

Nutzung:
Pfad zur CSV-Datei eingeben
Pfad fuer CSV-Export eingeben
Lade alle funktionen und Ausgabe der algorithmisch generiereten Setliste erhalten

|#

;;; Hilfsfunktionen
(defstruct song
  title
  melody-instruments 
  tempo
  key
  embouchure
  sax
  solo
  position)

(defun safe-parse-integer (s)
  (and s (plusp (length s)) (parse-integer s :junk-allowed t)))

(defun parse-csv-line (line &key (separator #\,))
  (let ((out '())
        (field (make-string-output-stream))
        (in-quote nil)
        (i 0)
        (n (length line)))
    (labels ((emit ()
               (push (get-output-stream-string field) out)
               (setf field (make-string-output-stream))))
      (loop while (< i n) do
            (let ((ch (char line i)))
              (cond
                ((char= ch #\")
                 (if in-quote
                     (if (and (< (1+ i) n)
                              (char= (char line (1+ i)) #\"))
                         (progn (incf i) (write-char #\" field)) ; \"\" -> "
                         (setf in-quote nil))
                     (setf in-quote t)))
                ((and (not in-quote) (char= ch separator))
                 (emit))
                (t
                 (write-char ch field))))
            (incf i))
      (emit)
      (nreverse out))))

(defun read-csv-file (pathname &key (separator #\,))
  (with-open-file (in pathname :direction :input)
    (loop for line = (read-line in nil nil)
          while line
          collect (parse-csv-line line :separator separator))))

(defun read-songs-from-csv (filename)
  (let* ((all-rows (read-csv-file filename)) ; Einlesen mit neuer Helferfunktion
         (rows (cdr all-rows))) ; entfernt die Ueberschrift
    (remove nil
            (mapcar (lambda (row)
                      (format t "Zeile gelesen: ~a~%" row)
                      (handler-case
                          (destructuring-bind (title melody-count tempo key embouchure sax solo position) row
                            (make-song
                             :title title
                             :melody-instruments (safe-parse-integer melody-count)
                             :tempo (safe-parse-integer tempo)
                             :key key
                             :embouchure (safe-parse-integer embouchure)
                             :sax (safe-parse-integer sax)
                             :solo (safe-parse-integer solo)
                             :position (safe-parse-integer position)))
                        (error (e)
                          (format t "Fehler beim Verarbeiten von Zeile: ~a~%Fehler: ~a~%" row e)
                          nil)))
                    rows))))

(defun shuffle-list (lst)
  (let ((vec (coerce lst 'vector)))
    (loop for i from (1- (length vec)) downto 1
          do (rotatef (aref vec i) (aref vec (random (1+ i)))))
    (coerce vec 'list)))

(defun insert-song-with-rules (song result)
;; Regelkonformes Einfuegen nach folgender Prioritaet:
;; 1. Kein Ansatz=1 nebeneinander UND keine gleiche Tonart UND Tempo-Regel eingehalten
;; 2. Kein Ansatz=1 nebeneinander UND Tempo-Regel eingehalten
;; 3. Notloesung: ans Ende
  (let ((ideal-spot nil)
        (good-spot nil))
    ;; Alle Einfuegestellen durchgehen
    (loop for i from 0 to (length result)
          for prev = (when (> i 0) (nth (1- i) result))
          for next = (nth i result)
          do
            (let* ((emb-song (song-embouchure song))
                   (key-song (song-key song))
                   (tempo-song (song-tempo song))

                   (emb-prev (and prev (song-embouchure prev)))
                   (emb-next (and next (song-embouchure next)))

                   (key-prev (and prev (song-key prev)))
                   (key-next (and next (song-key next)))

                   (tempo-prev (and prev (song-tempo prev)))
                   (tempo-next (and next (song-tempo next)))

                   ;; Regelverletzungen
                   (violates-emb
                    (or (and (eql emb-song 1) (eql emb-prev 1))
                        (and (eql emb-song 1) (eql emb-next 1))))

                   (violates-key
                    (or (and key-prev (string= key-prev key-song))
                        (and key-next (string= key-next key-song))))

                   (violates-tempo
                    (or (and tempo-prev (not (tempo-compatible-p tempo-prev tempo-song)))
                        (and tempo-next (not (tempo-compatible-p tempo-song tempo-next))))))

              ;; Bewertung der Position
              (unless (or violates-emb violates-tempo)
                (if (not violates-key)
                    (setf ideal-spot i)
                  (when (not good-spot)
                    (setf good-spot i))))))

    ;; Einfuegen an passender Stelle
    (cond
      (ideal-spot
       (append (subseq result 0 ideal-spot)
               (list song)
               (subseq result ideal-spot)))
      (good-spot
       (append (subseq result 0 good-spot)
               (list song)
               (subseq result good-spot)))
      (t
       (format t "~%WARNUNG: Kein idealer Platz fuer Song ~a gefunden - ans Ende gehaengt.~%" (song-title song))
       (append result (list song))))))

(defun tempo-compatible-p (t1 t2)
  "Prueft, ob t2 zum vorhergehenden Tempo t1 passt."
  (if (> t1 200)
      (< t2 120)
      (<= t2 (+ t1 30))))

(defun optimize-cluster (songs)
  ;; Optimiert die Cluster nach Ansatz und Tonart
  (let* ((shuffled (shuffle-list (copy-list songs)))
         (sax-songs (remove-if-not (lambda (s) (= (song-sax s) 1)) shuffled))
         (non-sax-songs (remove-if (lambda (s) (= (song-sax s) 1)) shuffled))
         (result '())
         (used '()))

    ;; Laesst Alt-Sax-Songs aussen vor
    (when non-sax-songs
      (push (first non-sax-songs) result)
      (push (first non-sax-songs) used))

    ;; Songs unter Regelbeachtung einfuegen
    (dolist (song (rest non-sax-songs))
      (let* ((last-song (car (last result)))
             (same-key (string= (song-key song) (song-key last-song)))
             (emb1? (= (song-embouchure song) 1))
             (emb-prev1? (= (song-embouchure last-song) 1)))
        (cond
         ((or same-key (and emb1? emb-prev1?))
          ;; Suche nach Alternative
          (let ((alt (find-if (lambda (alt)
                                (and (not (member alt used))
                                     (not (string= (song-key alt) (song-key last-song)))
                                     (not (and (= (song-embouchure alt) 1)
                                               (= (song-embouchure last-song) 1)))))
                              non-sax-songs)))
            (if alt
                (progn (push alt result) (push alt used))
              (progn (push song result) (push song used)))))
         (t
          (push song result)
          (push song used)))))

    ;; Anstrengende Songs (Ansatz = 1) verteilen
    (let* ((emb1-songs (remove-if-not (lambda (s) (= (song-embouchure s) 1)) result))
           (non-emb1 (remove-if (lambda (s) (= (song-embouchure s) 1)) result)))
      (setf result non-emb1)
      (when emb1-songs
        (let* ((n (length non-emb1))
               (k (length emb1-songs))
               (spacing (max 1 (floor n (1+ k))))
               (positions (loop for i from spacing by spacing
                                repeat k
                                collect (min i (length result)))))
          (loop for s in emb1-songs
                for i in positions
                do (setf result
                         (append (subseq result 0 i)
                                 (list s)
                                 (subseq result i)))))))

    ;; Sax-Songs einfuegen (sortiert nach Tempo)
    (let* ((sax-sorted (sort sax-songs #'< :key #'song-tempo))
           (insert-pos (truncate (/ (length result) 2))))
      (loop for s in sax-sorted
            for i from 0
            do (setf result
                     (append (subseq result 0 (+ insert-pos i))
                             (list s)
                             (subseq result (+ insert-pos i))))))

    ;; Doppelte vermeiden
    (remove-duplicates result :key #'song-title :test #'string=)))


;;; Hauptfunktion
(defun arrange-songs-with-full-rules (songs &key (embouchure-mode 1))
  "Ordnet Songs regelkonform mit rotierender Cluster-Strategie."
  (let* ((sorted-songs (sort (copy-list songs) #'< :key #'song-position))
         (first-song (first sorted-songs))
         (last-song (car (last sorted-songs)))
         (middle-songs (remove first-song (remove last-song sorted-songs)))

         ;; Zweiter Song
         (second-candidate
          (find-if (lambda (s)
                     (and (not (string= (song-key s) (song-key first-song)))
                          (= (song-melody-instruments s) 2)
                          (and (> (song-tempo s) 120) (<= (song-tempo s) 140))))
                   middle-songs))

         (remaining-after-second
          (if second-candidate
              (remove second-candidate middle-songs :count 1)
              middle-songs))

         ;; Dritter Song
         (third-candidate
          (find-if (lambda (s)
                     (and (not (string= (song-key s) (song-key first-song)))
                          (= (song-melody-instruments s) 2)
                          (and (> (song-tempo s) 140) (<= (song-tempo s) 160))))
                   middle-songs))

         (remaining-after-third
          (if third-candidate
              (remove third-candidate middle-songs :count 1)
            middle-songs))

         ;; Vorletzter Song
         (tempo-last (song-tempo last-song))
         (penultimate-candidate
          (find-if (lambda (s)
                     (let ((tempo (song-tempo s)))
                       (and (not (string= (song-key s) (song-key last-song)))
                            (>= tempo (- tempo-last 30))
                            (<= tempo (- tempo-last 10))
                            (= (song-melody-instruments s) 2))))
                   remaining-after-second))

         ;; Mittlere Songs
         (middle-candidates
          (if penultimate-candidate
              (remove penultimate-candidate remaining-after-second :count 1)
            remaining-after-second))

         ;; Alt-Sax-Songs extrahieren und mischen
         (sax-songs
          (shuffle-list
           (remove-if-not (lambda (s) (= (song-sax s) 1)) middle-candidates)))

         ;; Alt-Sax-Songs aus den mittleren Songs entfernen
         (middle-candidates
          (remove-if (lambda (s) (member s sax-songs)) middle-candidates))

         ;; Cluster definieren 
         ;; Cluster 1:    0-100     (langsam)
         ;; Cluster 2:  101-130     (moderat)
         ;; Cluster 3:  131-160     (mittel-schnell)
         ;; Cluster 4:  161-190     (schnell)
         ;; Cluster 5:  191-220     (sehr schnell)
         ;; Cluster 6:   221+       (extrem schnell)
         ;; + Songs in jedem Cluster fuer Varianz mischen
         (clusters (list
                    (shuffle-list ; Cluster 1 (Index 0)
                     (remove-if-not (lambda (s)
                                      (<= (song-tempo s) 100))
                                    middle-candidates))
                    (shuffle-list ; Cluster 2 (Index 1)
                     (remove-if-not (lambda (s)
                                      (and (> (song-tempo s) 100)
                                           (<= (song-tempo s) 130)))
                                    middle-candidates))
                    (shuffle-list ; Cluster 3 (Index 2)
                     (remove-if-not (lambda (s)
                                      (and (> (song-tempo s) 130)
                                           (<= (song-tempo s) 160)))
                                    middle-candidates))
                    (shuffle-list ; Cluster 4 (Index 3)
                     (remove-if-not (lambda (s)
                                      (and (> (song-tempo s) 160)
                                           (<= (song-tempo s) 190)))
                                    middle-candidates))
                    (shuffle-list ; Cluster 5 (Index 4)
                     (remove-if-not (lambda (s)
                                      (and (> (song-tempo s) 190)
                                           (<= (song-tempo s) 220)))
                                    middle-candidates))
                    (shuffle-list ; Cluster 6 (Index 5)
                     (remove-if-not (lambda (s)
                                      (> (song-tempo s) 220))
                                    middle-candidates))))
         (cluster-count (length clusters))
         (cluster-index 3) ; Start bei Cluster 4: 161-190
         (result (list))
         (last-inserted nil))

    ;; Erstes Element
    (push first-song result)
    (setf last-inserted first-song)

    ;; Zweites Element
    (when second-candidate
      (push second-candidate result)
      (setf last-inserted second-candidate))

    ;; Rotation durch Cluster
    (loop while (some #'identity clusters)
          do (let* ((cluster (nth cluster-index clusters))
                    (song (find-if
                           (lambda (s)
                             (let ((prev last-inserted))
                               (and
                                ;; Keine gleichen Tonart hintereinander
                                (not (string= (song-key s) (song-key prev)))

                                ;; Achte auf den Ansatz
                                (not (and (= (song-embouchure s) 1)
                                          (= (song-embouchure prev) 1)))

                                ;; Soli nicht am Stueck (ausser Altsax)
                                (not (and (not (= (song-sax s) 1))
                                          (not (= (song-sax prev) 1))
                                          (/= (song-solo s) 0)
                                          (/= (song-solo prev) 0))))))
                           cluster)))
               ;; Song zur Liste hinzufuegen
               (when song
                 (push song result)
                 (setf last-inserted song)
                 (setf (nth cluster-index clusters)
                       (remove song cluster :count 1))))
          (setf cluster-index (mod (1+ cluster-index) cluster-count)))

    ;; Alt-Sax-Songs einfuegen
    (when sax-songs
      (let* ((sax-sorted (optimize-cluster sax-songs))
             (mid (+ 2 (random 4))))
        (setf result
              (append (subseq result 0 (min mid (length result)))
                      sax-sorted
                      (subseq result (min mid (length result)))))))

    ;; Vorletzter Song
    (when penultimate-candidate
      (push penultimate-candidate result))

    ;; Letzter Song
    (push last-song result)

    ;; Reihenfolge umkehren (da push verwendet)
    (setf result (nreverse result))

    ;; Tempo-Regel bei bedarf pruefen
    (when *show-tempo-warnings*
      (loop for (s1 s2) on result while s2
            do (let ((t1 (song-tempo s1))
                     (t2 (song-tempo s2)))
                 (unless (tempo-compatible-p t1 t2)
                   (format t "~%WARNUNG: Tempo-Regel verletzt zwischen ~A (~A) und ~A (~A)~%"
                           (song-title s1) t1 (song-title s2) t2)))))

    ;; Fehlende Songs ergaenzen (zur Sicherheit)
    (let* ((result-titles (mapcar #'song-title result))
           (missing (remove-if (lambda (s)
                                 (member (song-title s) result-titles :test #'string=))
                               songs)))
      (when missing
        (format t "~%WARNUNG: Songs ergaenzt: ~a~%" (mapcar #'song-title missing))
        (dolist (s missing)
          (setf result (insert-song-with-rules s result)))))

    ;; Positionen setzen
    (loop for s in result
          for i from 1
          do (setf (song-position s) i))

    ;; Doppelte entfernen (Sicherheitsmassnahme)
    (remove-duplicates result :key #'song-title :test #'string=)))


;;; Ausgabefunktionen
(defun print-song-list (songs)
  (format t "~%~3@A  ~29A  ~10A  ~6A  ~5A  ~5A  ~5A ~5A~%" 
          "Pos" "Titel" "Melodie-Instrumente" "Tonart" "Tempo" "Ansatz" "Sax" "Solo")
  (format t "~A~%" (make-string 90 :initial-element #\-))
  (dolist (s songs)
    (format t "~3D  ~40A  ~8D  ~6A  ~5D  ~6D  ~3D ~6D ~%"
            (song-position s)
            (song-title s)
            (song-melody-instruments s)
            (song-key s)
            (song-tempo s)
            (song-embouchure s)
            (song-sax s)
            (song-solo s))))

(defun export-songs-to-csv (songs filename)
  ;; Nachbearbeitete Ausgabe
  (with-open-file (stream filename
                          :direction :output
                          :if-exists :supersede
                          :if-does-not-exist :create)
    ;; Kopfzeile
    (format stream "Position,Titel,Besetzung,Tempo,Tonart,Kommentar~%")

    ;; Songs schreiben
    (dolist (s songs)
      (let* ((melody (song-melody-instruments s))
             (sax (song-sax s))
             (solo (song-solo s))
             (emb (song-embouchure s))
             (besetzung
              (cond
                ((= melody 2)
                 (if (= sax 1) "Altsax & Posaune" "Tenorsax & Posaune"))
                ((and (= melody 1) (= solo 1))
                 (if (= sax 1) "Altsax" "Tenorsax"))
                ((and (= melody 1) (= solo 2)) "Posaune")
                (t "Unbekannt")))
             (kommentar (if (= emb 1) "Ansatz" "")))

        ;; Zeile schreiben
        (format stream "~D,~A,~A,~D,~A,~A~%"
                (song-position s)
                (song-title s)
                besetzung
                (song-tempo s)
                (song-key s)
                kommentar)))))


;;; Ausfuehrung
;; Debug-Schalter
(defparameter *show-tempo-warnings* nil) ;; nil / t  =  off / on

;; Auszulesende Datei bereitstellen
(defparameter *songs* (read-songs-from-csv #P"/Users/rale/Desktop/Musikhochschule/Unterrichtsmaterialien/SPCL/Projektarbeit/Faecherswing-Bsp+Solo.csv")) ; <-- HIER PFAD EINFUEGEN

;; Setliste generieren
(defparameter *final-songs*
  (arrange-songs-with-full-rules *songs*)) ; <--- HIER EVALUIEREN FUER NEUE REIHENFOLGE

;; Tests
(length *songs*) ; Anzahl geladener Songs
(song-title (first *songs*)) ; Titel des ersten Songs
(mapcar #'song-title middle-candidates)
(mapcar #'song-sax middle-candidates)
(mapcar #'song-key *songs*) ; Alle Tonarten 
(mapcar #'song-title *songs*) ; Alle Titel
(mapcar #'song-sax *songs*) ; Alle Sax-Nummern

;; Simple Ausgabe der Setlist
(print-song-list *final-songs*) ; <--- HIER EVALUIREN UM GESAMTE LISTE AUSZUGEBEN

;; CSV Export
(export-songs-to-csv *final-songs* #P"/Users/rale/Desktop/Musikhochschule/Unterrichtsmaterialien/SPCL/Projektarbeit/Faecherswing-Bsp-Setliste.csv") ; <--- HIER PFAD EINFUEGEN UND DATEINAME ANGEBEN 
;; !!! ACHTUNG: GLEICHE DATEINAMEN WERDEN UEBERSCHRIEBEN!!!
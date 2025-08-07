;;;; ----- SETLISTEN GENERATOR -----

;;; Beschreibung

;;; Regeln
#|  
- Erster und letzter Song bleiben an ihrer vorgegeben Positionen
- Song an Position 2: =/= Tonart wie erster Song, Tempo < 150 und 2 Melodieinstrumente
- Vorletzter Song: Tempo 10-30 weniger als letzter Song und =/= Tonart wie letzter Song
- Mittlere Songs: Songs mit Sax-Wert = 1 zusammen, keine gleichen Tonarten und Ansatzwert = 1 hintereinander
|#

;;; Packete laden
(load (merge-pathnames "quicklisp/setup.lisp" (user-homedir-pathname)))
(ql:quickload :cl-csv)
(ql:quickload :split-sequence)


;;; Hilfsfunktionen
(defstruct song
  title
  melody-instruments 
  tempo
  key
  embouchure
  sax
  position)

(defun parse-instruments (field)
  "Gegen fehlerhafte Eingaben"
  (parse-integer field :junk-allowed t))

(defun read-songs-from-csv (filename)
  (let* ((all-rows (cl-csv:read-csv filename))
         (rows (cdr all-rows))) ; entfernt Überschrift
    (remove nil
            (mapcar (lambda (row)
                      (format t "Zeile gelesen: ~a~%" row)
                      (handler-case
                          (destructuring-bind (title melody-count tempo key embouchure sax position) row
                            (make-song
                             :title title
                             :melody-instruments (parse-integer melody-count)
                             :tempo (parse-integer tempo)
                             :key key
                             :embouchure (parse-integer embouchure)
                             :sax (parse-integer sax)
                             :position (parse-integer position)))
                        (error (e)
                          (format t "Fehler beim Verarbeiten von Zeile: ~a~%Fehler: ~a~%" row e)
                          nil)))
                    rows))))

(defun hash-table-keys (table)
  (let (keys)
    (maphash (lambda (k v) (push k keys)) table)
    keys))

(defun shuffle-list (lst)
  "Gibt eine zufällig permutierte Kopie der Liste LST zurück."
  (let ((vec (coerce lst 'vector)))
    (loop for i from (1- (length vec)) downto 1
          do (rotatef (aref vec i) (aref vec (random (1+ i)))))
    (coerce vec 'list)))

(defun insert-song-with-rules (song result)
  "Fügt SONG möglichst regelkonform in RESULT mit folgenden Prioritäten ein:
   1. Kein Ansatz=1 nebeneinander UND keine gleiche Tonart UND Tempo-Regel eingehalten.
   2. Kein Ansatz=1 nebeneinander UND Tempo-Regel eingehalten.
   3. Notlösung: ans Ende."
  (let ((ideal-spot nil)
        (good-spot nil))

    ;; Alle Einfügestellen durchgehen
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

    ;; Einfügen an passender Stelle
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
       (format t "~%WARNUNG: Kein idealer Platz für Song ~a gefunden - ans Ende gehängt.~%" (song-title song))
       (append result (list song))))))

(defun tempo-compatible-p (t1 t2)
  "Prüft, ob t2 zum vorhergehenden Tempo t1 passt."
  (if (> t1 200)
      (< t2 150)
      (and (>= t2 (- t1 5))
           (<= t2 (+ t1 30)))))

(defun optimize-cluster (songs)
  "Optimiert eine Gruppe von Songs bzgl. Embouchure & Tonart."
  (let* ((shuffled (shuffle-list (copy-list songs)))
         (sax-songs (remove-if-not (lambda (s) (= (song-sax s) 1)) shuffled))
         (non-sax-songs (remove-if (lambda (s) (= (song-sax s) 1)) shuffled))
         (result '())
         (used '()))

    (when non-sax-songs
      (push (first non-sax-songs) result)
      (push (first non-sax-songs) used))

    (dolist (song (rest non-sax-songs))
      (let* ((last-song (car (last result)))
             (same-key (string= (song-key song) (song-key last-song)))
             (emb1? (= (song-embouchure song) 1))
             (emb-prev1? (= (song-embouchure last-song) 1)))

        (cond
         ((or same-key (and emb1? emb-prev1?))
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

    ;; Embouchure-1-Songs verteilen
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

    ;; Sax-Songs einfügen (sortiert nach Tempo)
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
  "Sortiere Songs unter Beachtung aller Regeln: Tonart, Embouchure, Sax. Anfang & Ende fix."
  (let* ((sorted-songs (sort (copy-list songs) #'< :key #'song-position))
         (first-song (first sorted-songs))
         (last-song (car (last sorted-songs)))
         (middle-songs (remove first-song (remove last-song sorted-songs))))

    ;; Cluster nach Tempo definieren
    (let* ((clusters (list
                      (remove-if-not (lambda (s) (< (song-tempo s) 100)) middle-songs)
                      (remove-if-not (lambda (s) (and (>= (song-tempo s) 100) (< (song-tempo s) 140))) middle-songs)
                      (remove-if-not (lambda (s) (and (>= (song-tempo s) 140) (< (song-tempo s) 180))) middle-songs)
                      (remove-if-not (lambda (s) (and (>= (song-tempo s) 180) (< (song-tempo s) 220))) middle-songs)
                      (remove-if-not (lambda (s) (>= (song-tempo s) 220)) middle-songs)))
           (flattened-clusters '()))

      ;; Lokale Optimierung pro Cluster
      (dolist (cluster clusters)
        (let ((opt-cluster (remove nil (optimize-cluster cluster))))
          (setf flattened-clusters (append flattened-clusters opt-cluster))))

      ;; Songliste zusammenbauen
      (let ((final-order
             (remove nil
                     (append (list first-song)
                             flattened-clusters
                             (list last-song)))))

        ;; Positionen setzen
        (loop for s in final-order
              for i from 1
              do (setf (song-position s) i))

        ;; Tempo-Check
        (loop for (s1 s2) on final-order while s2
              do (let ((t1 (song-tempo s1))
                       (t2 (song-tempo s2)))
                   (unless (tempo-compatible-p t1 t2)
                     (format t "~%WARNUNG: Tempo-Regel verletzt zwischen ~A (~A) und ~A (~A)~%"
                             (song-title s1) t1
                             (song-title s2) t2))))

        final-order))))


;;; Ausgabefunktionen
(defun print-song-list (songs)
  (format t "~%~3@A  ~30A  ~10A  ~7A  ~7A  ~7A  ~5A~%" 
          "Pos" "Titel" "Melodie-Instrumente" "Tonart" "Tempo" "Ansatz" "Sax")
  (format t "~A~%" (make-string 100 :initial-element #\-))
  (dolist (s songs)
    (format t "~3D  ~40A  ~8D  ~7A  ~7D  ~7D  ~5D~%"
            (song-position s)
            (song-title s)
            (song-melody-instruments s)
            (song-key s)
            (song-tempo s)
            (song-embouchure s)
            (song-sax s))))


(defun export-songs-to-csv (songs filename)
  (with-open-file (stream filename
                          :direction :output
                          :if-exists :supersede
                          :if-does-not-exist :create)
    ;; Header schreiben
    (format stream "Position,Titel,Melodie-Instrumente,Tonart,Tempo,Ansatz,Sax~%")
    ;; Songs schreiben
    (dolist (s songs)
      (format stream "~D,~A,~D,~A,~D,~D,~D~%"
              (song-position s)
              (song-title s)
              (song-melody-instruments s)
              (song-key s)
              (song-tempo s)
              (song-embouchure s)
              (song-sax s)))))

;;; Ausführung
(defparameter *songs* (read-songs-from-csv #P"/Users/rale/Desktop/Musikhochschule/Unterrichtsmaterialien/SPCL/Projektarbeit/Fächerswing-Bsp.csv")) ; hier Pfad enfügen
(defparameter *final-songs*
  (arrange-songs-with-full-rules *songs*))

;; Tests
(length *songs*) ; Anzahl geladener Songs
(song-title (first *songs*)) ; Titel des ersten Songs
(mapcar #'song-title middle-candidates)
(mapcar #'song-sax middle-candidates)
(mapcar #'song-key *songs*) ; Alle Tonarten 
(mapcar #'song-title *songs*) ; Alle Titel
(mapcar #'song-sax *songs*) ; Alle Sax-Nummern

;; Print
(print-song-list *final-songs*) ; Gesamte Liste

;; CSV Export
(export-songs-to-csv *final-songs* #P"/Users/rale/Desktop/Musikhochschule/Unterrichtsmaterialien/SPCL/Projektarbeit/Fächerswing-Bsp-Setliste.csv")


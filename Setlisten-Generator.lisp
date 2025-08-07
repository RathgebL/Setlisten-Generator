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
  (parse-integer field :junk-allowed t)) ; gegen fehlerhafte Eingaben

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


;;; Hauptfunktionen
(defun arrange-songs-with-full-rules (songs &key (embouchure-mode 1))
  "Sortiere Songs unter Beachtung aller Regeln: Tonart, Embouchure und Sax zusammen."
  (let* ((sorted-songs (sort (copy-list songs) #'< :key #'song-position))
         (first-song (first sorted-songs))
         (last-song (car (last sorted-songs)))
         (middle-songs (remove first-song (remove last-song sorted-songs)))
         
         ;; Suche nach zweitem Song
         (second-candidate
           (find-if (lambda (s)
                      (and (not (string= (song-key s) (song-key first-song)))
                           (= (song-melody-instruments s) 2)
                           (< (song-tempo s) 150)))
                    middle-songs))
         
         (remaining-after-second
           (if second-candidate
               (remove second-candidate middle-songs :count 1)
               middle-songs))
         
         ;; Suche nach vorletztem Song
         (tempo-last (song-tempo last-song))
         (penultimate-candidate
           (find-if (lambda (s)
                      (let ((tempo (song-tempo s)))
                        (and (not (string= (song-key s) (song-key last-song)))
                             (>= tempo (- tempo-last 30))
                             (<= tempo (- tempo-last 10))
                             (= (song-melody-instruments s) 2))))
                    remaining-after-second))
         
         (middle-candidates
           (if penultimate-candidate
               (remove penultimate-candidate remaining-after-second :count 1)
               remaining-after-second)))

    ;; Bearbeitung mittlerer Songs
    (labels ((middle-rules (songs)
           (let* ((sax-1 (remove-if-not (lambda (s) (= (song-sax s) 1)) songs))
                  (sax-0 (remove-if     (lambda (s) (= (song-sax s) 1)) songs))
                  (sax-grouped (append sax-0 sax-1))  ; sax=1 zusammenhängend später
                  (used '())
                  (result '()))

             ;; Helferfunktion: Regelverletzung prüfen
             (flet ((violates-rules? (last-song candidate)
                      (or
                       ;; gleiche Tonart wie vorher
                       (string= (song-key last-song) (song-key candidate))
                       ;; zweimal Embouchure = 1 direkt hintereinander
                       (and (= (song-embouchure last-song) 1)
                            (= (song-embouchure candidate) 1)))))

               ;; Hauptschleife: gehe durch, finde jeweils bestmöglichen Song
               (loop
                 for i from 1
                 until (= (length result) (length sax-grouped))
                 do
                   (let* ((last-song (car (last result)))
                          (remaining (remove-if (lambda (s) (member s used)) sax-grouped))
                          (next-candidate
                            (if last-song
                                (or
                                 ;; bevorzugt: Song, der keine Regel verletzt
                                 (find-if (lambda (s) (not (violates-rules? last-song s))) remaining)
                                 ;; oder irgendein übrig gebliebener
                                 (first remaining))
                                ;; allererster Song
                                (first remaining))))
                     (when next-candidate
                       (push next-candidate result)
                       (push next-candidate used)))))

             (nreverse result))))

      ;; Finale Songliste zusammenstellen
      (let ((final-order (remove nil
                                 (append (list first-song)
                                         (when second-candidate (list second-candidate))
                                         (middle-rules middle-candidates)
                                         (when penultimate-candidate (list penultimate-candidate))
                                         (list last-song)))))

        ;; Setze Positionen neu
        (loop for s in final-order
              for i from 1
              do (setf (song-position s) i))
        final-order))))


;;; Ausgabefunktionen
(defun print-song-list (songs)
  (format t "~%~4@A  ~-30A  ~20A  ~8A  ~7A  ~8A  ~5A~%" 
          "Pos" "Titel" "Melodie-Instrumente" "Tonart" "Tempo" "Ansatz" "Sax")
  (format t "~A~%" (make-string 100 :initial-element #\-))
  (dolist (s songs)
    (format t "~4D  ~-30A  ~20D  ~8A  ~7D  ~8D  ~5D~%"
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
(export-songs-to-csv *final-songs* #P"/Users/rale/Desktop/Musikhochschule/Unterrichtsmaterialien/SPCL/Projektarbeit/sortierte-songs.csv")


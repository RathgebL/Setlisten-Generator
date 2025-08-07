;;;; ----- SETLISTEN GENERATOR -----

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
  position)

(defun parse-instruments (field)
  (parse-integer field :junk-allowed t))

(defun read-songs-from-csv (filename)
  (let* ((all-rows (cl-csv:read-csv filename))
         (rows (cdr all-rows))) ; Header weg
    (remove nil
            (mapcar (lambda (row)
                      (format t "Zeile gelesen: ~a~%" row)
                      (handler-case
                          (destructuring-bind (title melody-count tempo key embouchure position) row
                            (make-song
                             :title title
                             :melody-instruments (parse-integer melody-count :junk-allowed t)
                             :tempo (parse-integer tempo :junk-allowed t)
                             :key key
                             :embouchure (parse-integer embouchure :junk-allowed t)
                             :position (parse-integer position :junk-allowed t)))
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
  "Ordnet die Songs nach spezifischen Regeln: 
   - Erster und letzter Song bleiben an Positionen.
   - Song an Position 2: =/= Tonart wie erster Song, Tempo < 150, 2 Melodieinstrumente.
   - Vorletzter Song: Tempo 10-30 weniger als letzter, =/= Tonart wie letzter Song.
   - Mittlere Songs: keine gleichen Tonarten hintereinander und Ansatz beachten
   - Ansatz 0 = keine Einschränkung, Ansatz 1 = keine Aufeinanderfolgende, Ansatz 2 = sollen aufeinander folgen"
  (let* ((sorted-songs (sort (copy-list songs) #'< :key #'song-position))
         (first-song (first sorted-songs))
         (last-song (car (last sorted-songs)))
         (remaining-songs (remove first-song (remove last-song sorted-songs)))

         ;; Finde Song für Position 2
         (second-candidate
          (find-if (lambda (s)
                     (and (not (string= (song-key s) (song-key first-song)))
                          (= (song-melody-instruments s) 2)
                          (< (song-tempo s) 150)))
                   remaining-songs))

         (remaining-after-second (if second-candidate
                                     (remove second-candidate remaining-songs :count 1)
                                     remaining-songs))

         ;; Finde Song für vorletzte Position
         (tempo-last (song-tempo last-song))
         (penultimate-candidate
          (find-if (lambda (s)
            (let ((tempo (song-tempo s)))
              (and (not (string= (song-key s) (song-key last-song)))
                   (>= tempo (- tempo-last 30))
                   (<= tempo (- tempo-last 10))
                   (= (song-melody-instruments s) 2))))
          remaining-after-second))

         (middle-candidates (if penultimate-candidate
                                (remove penultimate-candidate remaining-after-second :count 1)
                                remaining-after-second)))

    ;; Sortiere mittlere Songs so, dass keine gleichen Tonarten und Ansatzwert = 1 aufeinander folgen
    (labels ((no-repeating-keys-and-embouchure (songs embouchure-mode)
           (when songs
             (let ((result (list (first songs))))
               (dolist (song (rest songs))
                 (let* ((last-song (car (last result)))
                        (same-key (string= (song-key song) (song-key last-song)))
                        (same-emb (and (= embouchure-mode 1)
                                       (= (song-embouchure song) (song-embouchure last-song)))))
                   (cond
                     ((or same-key same-emb)
                      ;; versuche Alternative zu finden
                      (let ((alt (find-if (lambda (s)
                                            (and (not (string= (song-key s) (song-key last-song)))
                                                 (or (/= embouchure-mode 1)
                                                     (/= (song-embouchure s) (song-embouchure last-song)))))
                                          songs)))
                        (if alt
                            (progn
                              (push alt result)
                              (setf songs (remove alt songs :count 1)))
                            (push song result))))
                     (t
                      (push song result)))))
               (nreverse result)))))

      ;; Finaler Aufbau der Songliste
      (let* ((middle-sorted (no-repeating-keys-and-embouchure middle-candidates embouchure-mode))
       (final-order (append (list first-song)
                            (when second-candidate (list second-candidate))
                            middle-sorted
                            (when penultimate-candidate (list penultimate-candidate))
                            (list last-song))))

        ;; Setze Positionen neu
        (loop for s in final-order
              for i from 1
              do (setf (song-position s) i))
        final-order))))

(defun print-song-list (songs)
  (format t "~&~4A  ~-30A ~12A ~-10A  ~6A  ~7A ~%"
          "Pos" "Titel" "Instrumente" "Tonart" "Tempo" "Ansatz")
  (format t "~A~%" (make-string 80 :initial-element #\-))
  (dolist (s songs)
    (format t "~4D  ~-30A ~12A ~-10A  ~6A  ~7A ~%"
            (song-position s)
            (song-title s)
            (song-melody-instruments s)
            (song-key s)
            (song-tempo s)
            (song-embouchure s))))

(defun export-songs-to-csv (songs filename)
  "Exportiert die übergebene SONG-Liste als CSV-Datei nach FILENAME."
  (with-open-file (out filename
                       :direction :output
                       :if-exists :supersede
                       :external-format :utf-8)
    ;; Schreibe Header
    (format out "Position,Titel,Instrumente,Tonart,Tempo,Ansatz~%")
    ;; Schreibe jede Song-Zeile
    (dolist (s songs)
      (format out "~D,~A,~D,~A,~D,~D~%"
              (song-position s)
              (song-title s)
              (song-melody-instruments s)
              (song-key s)
              (song-tempo s)
              (song-embouchure s)))))


(defparameter *songs* (read-songs-from-csv #P"/Users/rale/Desktop/Musikhochschule/Unterrichtsmaterialien/SPCL/Projektarbeit/testV2.csv"))
(defparameter *final-songs*
  (arrange-songs-with-full-rules *songs*))


;; Tests
(mapcar #'song-title *songs*)
(length *songs*) ; Anzahl geladener Songs
(song-title (first *songs*)) ; Titel des ersten Songs
(mapcar #'song-key *songs*) ; Alle Tonarten 

;; Print
(print-song-list *final-songs*) ; Gesamte Liste

;; CSV Export
(export-songs-to-csv *final-songs* #P"/Users/rale/Desktop/Musikhochschule/Unterrichtsmaterialien/SPCL/Projektarbeit/sortierte-songs.csv")


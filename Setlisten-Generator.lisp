;; run in REPL first: (load (merge-pathnames "quicklisp/setup.lisp" (user-homedir-pathname)))


(ql:quickload :cl-csv)
(ql:quickload :split-sequence)


(defstruct song
  title
  melody-instruments
  tempo
  key
  position)

(defun parse-instruments (field)
  (parse-integer field :junk-allowed t))

(defun read-songs-from-csv (filename)
  (let* ((all-rows (cl-csv:read-csv filename))
         (rows (cdr all-rows))) ; Header weg
    (remove nil
            (mapcar (lambda (row)
                      (handler-case
                          (destructuring-bind (title melody-instruments tempo key position) row
                            (make-song
                             :title title
                             :melody-instruments (parse-integer melody-instruments)
                             :tempo (parse-integer tempo)
                             :key key
                             :position (parse-integer position)))
                        (error (e)
                          (format t "Fehler beim Verarbeiten von Zeile: ~a~%Fehler: ~a~%" row e)
                          nil)))
                    rows))))

(defun hash-table-keys (table)
  (let (keys)
    (maphash (lambda (k v) (push k keys)) table)
    keys))

(defun arrange-songs-with-full-rules (songs)
  "Ordnet die Songs nach spezifischen Regeln: 
   - Erster und letzter Song bleiben an Positionen.
   - Song an Position 2: =/= Key wie erster Song, Tempo < 150, 2 Melodieinstrumente.
   - Vorletzter Song: Tempo 10-30 weniger als letzter, =/= Key wie letzter Song.
   - Mittlere Songs: keine gleichen Tonarten hintereinander."
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

    ;; Sortiere mittlere Songs so, dass keine gleichen Tonarten aufeinander folgen
    (labels ((no-repeating-keys (songs)
               (if (null songs)
                   nil
                   (let ((result (list (first songs))))
                     (dolist (song (rest songs))
                       (let ((last-key (song-key (car (last result)))))
                         (if (string= (song-key song) last-key)
                             ;; versuche alternativen zu finden
                             (let ((alt (find-if (lambda (s)
                                                   (not (string= (song-key s) last-key)))
                                                 songs)))
                               (if alt
                                   (progn
                                     (push alt result)
                                     (push song result))
                                   (push song result)))
                             (push song result))))
                     (nreverse result)))))

      ;; Finaler Aufbau der Songliste
      (let* ((middle-sorted (no-repeating-keys middle-candidates))
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
  "Gibt eine Liste von SONG-Objekten formatiert aus."
  (format t "~%~5A  ~27A  ~10A  ~9A  ~7A~%" "Pos" "Titel" "Instr." "Key" "Tempo")
  (format t "~A~%" (make-string 80 :initial-element #\-))
  (dolist (s songs)
    (format t "~5A  ~30A ~10A ~10A ~10A~%"
            (song-position s)
            (song-title s)
            (song-melody-instruments s)
            (song-key s)
            (song-tempo s))))

(defparameter *songs* (read-songs-from-csv #P"/Users/rale/Desktop/Musikhochschule/Unterrichtsmaterialien/SPCL/Projektarbeit/testV1.csv"))
(defparameter *final-songs*
  (arrange-songs-with-full-rules *songs*))


;; Tests
(mapcar #'song-title *songs*)
(length *songs*) ; Anzahl geladener Songs
(song-title (first *songs*)) ; Titel des ersten Songs
(mapcar #'song-key *songs*) ; Alle Tonarten 

(print-song-list *final-songs*) ; Gesamte Liste

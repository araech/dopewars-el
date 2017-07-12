;;; dopewars-format.el --- Formatting functions for dopewars.el
;; (c) 2017 Aubrey Raech

;; This file is part of the dopewars.el package. Please refer to the header
;; of the file dopewars.el for information on this package and the licensing
;; of all program files, including this one.

;;;
;;;; TEXT FORMATTING FUNCTIONS
;;;

(defun dope--fmt-header ()
  "FIXME. This is fucking disgusting and you should be ASHAMED."
  (let ((loc-name (dw-place dope-current-location name)))
    (format "%s%s%s%s%s\n%s\u2514%s\u2518\n\n"
            "\u2014\u2550\u258f DOPEWARS \u2595\u2550\u2014"
            (make-string 12 ? )
            (let ((padsize (/ (- 18 (length loc-name)) (float 2))))
              (format "\u2502%s%s%s\u2502"
                      (make-string (floor padsize) ? )
                      loc-name
                      (make-string (ceiling padsize) ? )))
            (make-string 16 ? )
            (format "DAY %2d/%d" dope-day *dopewars-days*)
            (make-string 28 ? )
            (make-string 18 #x2500))))

(defun dope--fmt-stat-block ()
  (format "%s  %s  %s\n%s%s  %s\n"
          (dope--fmt-stat "health" (format "%3d" dope-health))
          (dope--fmt-stat "cash"   (dope--fmt-money dope-cash))
          (dope--fmt-stat "stash"  (format "%3d/%3d"
                                           (dw-stash size)
                                           *dopewars-stash-max*))
          (make-string 25 ? ) ; paddin', yo
          (dope--fmt-stat "debt" (dope--fmt-money dope-loan))
          (dope--fmt-stat "bank" (dope--fmt-money dope-bank))))
(defun dope--fmt-stat (name datum)
  (let ((span (- 19 (length name) (length datum))))
    (format "[ %s%s%s ]" (upcase name) (make-string span ? ) datum)))

(defun dope--fmt-stash ()
  (mapconcat 'dope--fmt-stash-entry (dope-sort-by-car dope-stash) "\n"))
(defun dope--fmt-stash-entry (ent)
  (let ((name (elt (elt *dopewars-drugs* (car ent)) 0))
        (quantity (number-to-string (cadr ent)))
        (cost (dope--fmt-money (cl-caddr ent))))
    (format " %s%s%s @%s%s"
            name (make-string (- 15 (length name) (length quantity)) ? )
            quantity (make-string (- 11 (length cost)) ? ) cost)))

(defun dope--fmt-drop-drugs ()
  (mapconcat (lambda (x) (apply 'concat x))
             (mapcar (lambda (x) (mapcar 'dope--fmt-drop-drug x))
                     (seq-partition dope-stash 3)) "\n"))
(defun dope--fmt-drop-drug (stash-drug)
  (let ((letter (+ ?a (seq-position dope-stash stash-drug)))
        (name (elt (elt *dopewars-drugs* (car stash-drug)) 0)))
    (format " %c) %s%s" letter name
            (make-string (- 12 (length name)) ? ))))

(defun dope--fmt-locs ()
  (mapconcat (lambda (x) (apply 'concat x))
             (seq-partition (seq-map 'dope--fmt-loc *dopewars-locations*) 4)
             "\n"))
(defun dope--fmt-loc (loc)
  (format "%2d) %s%s" (1+ (seq-position *dopewars-locations* loc))
          (elt loc 0) (make-string (- 15 (length (elt loc 0))) ? )))

(defun dope--fmt-drugs ()
  (mapconcat (lambda (x) (apply 'concat x))
             (seq-partition (seq-map 'dope--fmt-drug here-drugs) 3)
             "\n"))
(defun dope--fmt-drug (drug)
  (let* ((id (seq-position here-drugs drug))
         (name (elt (elt *dopewars-drugs* (elt drug 0)) 0))
         (price (dope--drugprice id)))
    (format " %c) %s%s%s    " (+ id ?a) name
            (make-string (- 16 (+ (length name) (length price))) ? ) price)))

(defun dope--canafford (index)
  (floor (/ dope-cash (elt (elt here-drugs index) 1))))
(defun dope--drugprice (index)
  (dope--fmt-money (elt (elt here-drugs index) 1)))
(defun dope--fmt-money (value)
  (concat "$" (group-number value)))
(defun dope--fmt-letter-request-str (limit)
  (format "Which drug [a-%c]? " (1- (+ limit ?a))))
(defun dope--fmt-number-request-str (limit)
  (format "Where to [1-%d]? " limit))
(defun dope--fmt-buying-str (here-index)
  (format "You can afford %d, and you can carry %d."
          (dope--canafford here-index) (dw-stash space)))
(defun dope--fmt-selling-str (pos)
  (let ((num (dw-stash entry pos quantity)))
    (format "You have %d unit%s." num (if (> num 1) "s" ""))))

;;; dopewars-format.el ends here

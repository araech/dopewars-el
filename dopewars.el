;;; dopewars.el --- Classic dopewars game for emacs
;; (c) 2017 Aubrey Raech

;; Author: Aubrey Raech <aubrey@raech.net>
;; Version: dev
;; License: GPLv3+
;; URL: https://github.com/araech/dopewars-el/

;; This file is not part of GNU Emacs

;; This program is free software: you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by the
;; Free Software Foundation, either version 3 of the License, or (at your
;; option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General
;; Public License for more details.

;; You should have received a copy of the GNU General Public License along
;; with this program. If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; TODO LIST:
;; - Bronx and Ghetto activities
;; --- LOAN SHARK
;; --- BANK
;; - Endgame procedure
;; - High scores??
;; - Cops
;; - Bitches?
;; - Guns?
;; - BUST/CHEAP!!!?!?!?!?
;; - Figure out proper scoping for variables. :(
;; --- Buffer/local?

;;; Code:

(defgroup dopewars nil
  "Dopewars variables."
  :prefix "dopewars-")

(defcustom *dopewars-days*
  31
  "Number of days of gameplay."
  :group 'dopewars
  :type 'integer)

(defcustom *dopewars-stash-max*
  100
  "Maximum stash size / drug slots."
  :group 'dopewars
  :type 'integer)

(defcustom *dopewars-starting-cash*
  2000
  "Starting cash in dollars."
  :group 'dopewars
  :type 'integer)

(defcustom *dopewars-starting-loan*
  5500
  "Starting loan in dollars."
  :group 'dopewars
  :type 'integer)

(defcustom *dopewars-expensive-strings*
  ["Cops made a big %s bust! Prices are outrageous!"
   "Addicts are buying %s at ridiculous prices!"]
  "Strings used to notify player of expensive drugs."
  :group 'dopewars
  :type 'sexp)

(defcustom *dopewars-rates*
  [1.1 1.05]
  "Rates for loan and bank interests, respectively."
  :group 'dopewars
  :type 'sexp)

(defcustom *dopewars-drugs*
  ;; Name     Min    Max    Exp? Cheap?  &optional cheap-string
  [["Acid"    1000   4400   nil  t
    "The market is flooded with cheap home-made %s!"]
   ["Cocaine" 15000  29000  t    nil]
   ["Hashish" 480    1280   nil  t
    "The Marrakesh Express has arrived!"]
   ["Heroin"  5500   13000  t    nil]
   ["Ludes"   11     60     nil  t
    "Rival drug drealers raided a pharmacy and are selling cheap %s!"]
   ["MDA"     1500   4400   nil  nil]
   ["Opium"   540    1250   t    nil]
   ["PCP"     1000   2500   nil  nil]
   ["Peyote"  220    700    nil  nil]
   ["Shrooms" 630    1300   nil  nil]
   ["Speed"   90     250    t    nil]
   ["Weed"    315    890    nil  t
    "Columbian freighter dusted the Coast Guard! %s prices have bottomed out!"]]
  "Complete list of drugs. Each vector contains the following:
   - Drug name
   - Normal minimum price
   - Normal maximum price
   - Can be expensive?
   - Can be cheap?
   - (optional) if drug can be cheap, cheap string"
  :group 'dopewars
  :type 'sexp)

(defcustom *dopewars-locations*
  ;; Name        Min-drugs    Max    Police Presence
  [["Bronx"              7     12     10]
   ["Ghetto"             8     12      5]
   ["Central Park"       6     12     15]
   ["Manhattan"          4     10     90]
   ["Coney Island"       6     12     20]
   ["Brooklyn"           4     11     70]
   ["Queens"             6     12     50]
   ["Staten Island"      6     12     20]]
  "Complete set of locations, including:
   - Location name
   - Minmum number of drugs
   - Maximum number of drugs
   - Police presence, out of 100"
  :group 'dopewars
  :type 'sexp)

;;; Defvars

(defvar dope-health 100
  "Health of player. Uses not yet fully implemented (cops).")
(defvar dope-stash nil
  "List of lists of drug data currently held by player.
FURTHER DESCRIPTION.")
(defvar dope-cash 2000
  "Money held by player.")
(defvar dope-loan 5500
  "Amount of money owed to loan shark.")
(defvar dope-bank 0
  "Amount of money in the bank.")
(defvar dope-day 1
  "Current day number.")
(defvar dope-current-location 0
  "Current location of player, as index of *dopewars-locations*.")
(defvar here-drugs nil
  "Vector of vectors of available drug data.
FURTHER DESCRIPTION DOCSTRING.")


;;;
;;;; MODE AND META FUNCTIONS
;;;

(define-derived-mode dope-mode special-mode "dope-mode"
  (define-key dope-mode-map (kbd "j") 'dw-do-jet)
  (define-key dope-mode-map (kbd "b") 'dw-do-buy)
  (define-key dope-mode-map (kbd "s") 'dw-do-sell)
  (define-key dope-mode-map (kbd "d") 'dw-do-drop)
  (define-key dope-mode-map (kbd "SPC") 'dw-do-nothing))

(defun dopewars ()
  "Initiate dopewars game."
  (interactive)
  (switch-to-buffer "dopewars")
  (buffer-disable-undo "dopewars")
  (dope-mode)
  (dope-init))

(defun dope-init ()
  "Start a new dopewars game, setting appropriate starter values."
  (setq dope-health 100
        dope-stash nil
        dope-cash *dopewars-starting-cash*
        dope-loan *dopewars-starting-loan*
        dope-bank 0
        dope-day 1
        dope-current-location 0
        here-drugs (dope-get-here-drugs
                    (dope-get-loc-drug-idxs
                     dope-current-location)))
  ;; Bust/cheap? Location activities? FIXME
  (dope-print-game))

;;;
;;;; FUNCTIONS
;;;

(defun dope-apply-interest ()
  "Alter loan and bank amounts to reflect daily interest rates."
  (let ((loan-rate (elt *dopewars-rates* 0))
        (bank-rate (elt *dopewars-rates* 1)))
    (setq dope-loan (floor (* dope-loan loan-rate))
          dope-bank (floor (* dope-bank bank-rate)))))

(defun dope-gen-drug-price (drug)
  "Take vector DRUG and create drug price within range.
FIXME: This does not take busts and floods into account."
  (let ((dmin (elt drug 1))
        (dmax (elt drug 2)))
    (+ dmin (random (- dmax dmin)))))

(defun dope-get-loc-drug-idxs (index)
  "Return a vector for location INDEX, with N elements,
wnere N is a random number between the selected location's
min-drugs and max-drugs numbers. The resulting vector should
contain, in order, a random selection of drug indices by use of
dope-filter. FIXME could use clearer documentation.

Also, this should only be run once per turn, at arrival to jetted
location."
  (let ((i 0)
        (vec (vector))
        (location (elt *dopewars-locations* index)))
    (dotimes (i (length *dopewars-drugs*))
      (setq vec (vconcat vec (vector i))))
    (let ((nmin (elt location 1))
          (nmax (elt location 2)))
      (dope-filter vec (+ nmin (random (- nmax nmin)))))))

(defun dope-get-here-drugs (drug-indices)
  "FIXME: This. Is. Fucking. Gross. DOCSTRING"
  (let ((vec (vector))
        modified) ; for cheap/expensive
    (dotimes (i (length drug-indices))
      ;; Call probabilistic cheap/expensive for each, MAX 2
      ;; Notate cheap/exp strings in 'modified
      ;;  -- Ensure unique expensive strings
      (setq vec
            (vconcat
             vec
             (vector (vector
                      (elt drug-indices i)
                      (dope-gen-drug-price
                       (elt *dopewars-drugs* (elt drug-indices i))))))))
    ;; (dope-alert-user (modified))
    vec))

(defun dope-filter (vec goal)
  "Remove random elements from a vector until it reaches GOAL length."
  (if (= goal (length vec)) vec
    (dope-filter (remove (elt vec (random (length vec))) vec) goal)))

(defun dope-stash-position (here-id)
  "Find position of a drug from here-drugs in the player's stash.
Return the index of the drug from here-drugs at index HERE-ID, if
it is present in the player's stash. If it isn't, return nil."
  (seq-position dope-stash (elt (elt here-drugs here-id) 0)
                (lambda (a b) (eq (car a) b))))

(defun dope-add-drug (here-index n)
  "Add N units of drug at HERE-INDEX to stash.
If the drug's already in the stash, add it to the existing entry.
Otherwise, push the drug onto the stash stack. Lessen player cash
by an appropriate amount."
  (let ((drug (elt here-drugs here-index)))
    ;; FIXME accomplish with (unless (or A B)) ?
    (cond
     ((= n 0) nil)
     ((or (< n 0) (not (integerp n))) ; negatives and floats
      (message "What the hell kinda number is that?") nil)
     ((< dope-cash (* n (elt drug 1)))
      (message "You can't afford it!"))
     ((> n (- *dopewars-stash-max* (dope-calc-stash-size)))
      (message "You don't have the space!"))
     (t
      (let ((pos (dope-stash-position here-index)))
        (if pos ; EXISTS IN STASH ALREADY
            (let* ((mydrug (nth pos dope-stash))
                   (new-total (+ n (cadr mydrug))))
              (setf (nth pos dope-stash)
                    (list (car mydrug) new-total
                          (/ (+ (* n (elt drug 1))
                                (* (nth 1 mydrug)
                                   (nth 2 mydrug)))
                             new-total))))
          ;; ELSE: DRUG IS NEW, ADD NEW DRUG
          (push (list (elt drug 0) n (elt drug 1)) dope-stash))
        (setq dope-cash (- dope-cash (* n (elt drug 1)))))))))

(defun dope-sell-drug (here-index n)
  "Remove N units of drug at HERE-INDEX from stash.
If N is all of the drug, remove that drug from the stash stack.
Increase player cash by an appropriate amount."
  (let ((drug (elt here-drugs here-index))
        (pos (dope-stash-position here-index))) ; position
    (cond
     ((= n 0) nil)
     ((or (< n 0) (not (integerp n))) ; negatives and floats
      (message "What the hell kinda number is that?") nil)
     ((> n (cadr (nth pos dope-stash)))
      (message "You don't have that many!")) ; FIXME just sell all?
     (t (let ((my-drug (nth pos dope-stash)))
          (setq dope-cash (+ dope-cash (* n (elt drug 1))))
          (if (= n (cadr my-drug))
              (setf dope-stash (remove my-drug dope-stash))
            (setf (cadr my-drug) (- (cadr my-drug) n))))))))

(defun dope-lose-drug (index n)
  "Remove N units of drug at INDEX of stash. If N is the entire
  stash quantity (or more than), remove the drug from the stash.
  Drugs removed in this way are lost without compensation. This
  is used by the Drop functionality, as well as (as yet
  unwritten) general drug loss from subway travel."
  (let ((drug (nth index dope-stash)))
    (if (< n (cadr drug))
        (setf (cadr drug) (- (cadr drug) n))
      (setf dope-stash (remove drug dope-stash)))))

(defun dope-calc-stash-size ()
  "Return total number of drug units in dope-stash."
  (let ((n 0))
    (dotimes (item (length dope-stash))
      (setq n (+ n (nth 1 (nth item dope-stash)))))
    n))

(defun dope-select-destination ()
  "Prompt user for jet destination. If it's a new location,
return the index of new location. Otherwise, return nil."
  (let* ((key (read-key "Where to?"))
         (keyint (string-to-number (char-to-string key))))
    (cond ((or (< key ?1)
               (> key ?8))
           (message "Invalid destination.") nil)
          ((= keyint (1+ dope-current-location))
           (message "You're already there!") nil)
          (t (1- keyint)))))
(defun dope-select-drug (limit)
  "Prompt user for drug selection up to LIMIT. Return index where
drug resides."
  (if (= limit 0) nil
    (let ((key (read-key "Which drug?")))
      (if (or (< key ?a) (>= key (+ ?a limit)))
          (prog1 nil (message "Dude, that isn't a drug."))
        (- key ?a)))))
(defun dope-sort-by-car (sequence)
  "Sort sequences in SEQUENCE by their element at index 0."
  (sort sequence (lambda (a b) (< (elt a 0) (elt b 0)))))

;;;
;;;; TEXT FORMATTING FUNCTIONS
;;;

(defun dope--fmt-header ()
  "FIXME. This is fucking disgusting and you should be ASHAMED."
  (let ((program-str  "\u2014\u2550\u258f DOPEWARS \u2595\u2550\u2014")
        (location-str (dope--currentlocation))
        (day-str      (format "DAY %2d/%d" dope-day *dopewars-days*)))
    (format "%s%s%s%s%s\n%s\u2514%s\u2518\n\n"
            program-str
            (make-string 12 ? )
            (let ((padsize (/ (- 18 (length location-str)) (float 2))))
              (format "\u2502%s%s%s\u2502"
                      (make-string (floor padsize) ? )
                      location-str
                      (make-string (ceiling padsize) ? )))
            (make-string 16 ? )
            day-str
            (make-string 28 ? )
            (make-string 18 #x2500))))
;; ((lambda (pad) (list (floor pad) (ceiling pad)))
;;  (/ (- 18 (length location-str)) (float 2)))

(defun dope--fmt-stat-block ()
  (format "%s  %s  %s\n%s%s  %s\n"
          (dope--fmt-stat "health" (format "%3d" dope-health))
          (dope--fmt-stat "cash"   (dope--fmt-money dope-cash))
          (dope--fmt-stat "stash"  (format "%3d/%3d"
                                             (dope-calc-stash-size)
                                             *dopewars-stash-max*))
          (make-string 25 ? ) ; paddin', yo
          (dope--fmt-stat "debt" (dope--fmt-money dope-loan))
          (dope--fmt-stat "bank" (dope--fmt-money dope-bank))))
(defun dope--fmt-stat (name datum)
  (let ((span (- 19 (length name) (length datum))))
    (format "[ %s%s%s ]" (upcase name) (make-string span ? ) datum)))

(defun dope--fmt-stash ()               ;FIXME FUCK YOU
  (setf dope-stash (dope-sort-by-car dope-stash))
  (let ((limit (length dope-stash))
        (str ""))
    (dotimes (index limit)
      (setq str (concat str (dope--fmt-stash-entry
                             (nth index dope-stash)) "\n")))
    str))
(defun dope--fmt-stash-entry (entry)
  (let ((name     (elt (elt *dopewars-drugs* (car entry)) 0))
        (quantity (number-to-string (nth 1 entry)))
        (cost     (dope--fmt-money   (nth 2 entry))))
    (format " %s%s%s @%s%s"
            name
            (make-string (- 15 (length name) (length quantity)) ? )
            quantity
            (make-string (- 11 (length cost)) ? )
            cost)))
(defun dope--fmt-drop-drugs ()
  (let ((limit (length dope-stash))
        (str ""))
    (dotimes (index limit)
      (setq str (concat str (dope--fmt-drop-drug (nth index dope-stash))
                        (when (= 3 (mod index 4)) "\n"))))
    (concat str "\n")))
(defun dope--fmt-drop-drug (stash-drug)
  (let ((letter (string (+ ?a index)))
        (name (elt (elt *dopewars-drugs* (car stash-drug)) 0)))
    (format " %s) %s%s" letter name
            (make-string (- 12 (length name)) ? ))))

(defun dope--fmt-loc (index)
  (let ((name (elt (elt *dopewars-locations* index) 0)))
    (format "%2d) %s%s" (1+ index) name
            (make-string (- 15 (length name)) ? )))) ; FIXME?
(defun dope--fmt-locs ()
  (let ((limit (length *dopewars-locations*))
        (str ""))
    (dotimes (index limit)              ;FIXME?
      (setq str
            (concat
             str
             (dope--fmt-loc index)
             (when (zerop (mod (1+ index) 4)) "\n"))))
    str))

(defun dope--fmt-drugs ()
  (let ((str ""))
    (dotimes (i (length here-drugs))
      (setq str
            (concat str
                    (dope--fmt-drug i)
                    (when (zerop (mod (1+ i) 3)) "\n"))))
    str))
(defun dope--fmt-drug (index)
  (let ((drugname (dope--drugname index))
        (drugprice (dope--drugprice index)))
    (concat " " (string (+ index ?a)) ") " drugname
            (make-string (- 16 (+ (length drugname) (length drugprice))) ? )
            drugprice "    ")))         ; FIXME (end of line?)

(defun dope--canafford (index)
  (floor (/ dope-cash (elt (elt here-drugs index) 1))))
(defun dope--cancarry ()
  (- *dopewars-stash-max* (dope-calc-stash-size)))
(defun dope--currentlocation ()
  (elt (elt *dopewars-locations* dope-current-location) 0))
(defun dope--drugname (index)
  (elt (elt *dopewars-drugs* (elt (elt here-drugs index) 0)) 0))
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
          (dope--canafford here-index) (dope--cancarry)))
(defun dope--fmt-selling-str (stash-pos)
  (let ((num (nth 1 (nth stash-pos dope-stash))))
    (format "You have %d unit%s." num (if (> num 1) "s" ""))))

(defun dope-alert (lines)
  "Alert user by displaying LINES on a blank screen."
  (let ((inhibit-read-only t))
    (erase-buffer)
    (insert (format "\n\n\n"))
    (cond ((stringp lines)
           (insert (format "    %s\n" lines)))
          ((listp lines)
           (while lines
             (insert (format "    %s\n" (pop lines)))))
          ((stringp lines))
          (t (insert "Function dope-alert received no valid data.\n")))
    (insert "\n\nPress any key to continue...")
    (read-key "Press any key to continue..."))
  (dope-print-game))

(defun dope-print-game ()
  "Wipe buffer and print game state."
  (let ((inhibit-read-only t))
    (erase-buffer)
    (insert (dope--fmt-header))
    (insert (dope--fmt-stat-block))
    (insert "LOCATIONS\n" (dope--fmt-locs))
    (insert "\nYOUR STASH\n")
    (insert (dope--fmt-stash))
    (insert "\n\nHey dude, the drugs for sale here are:\n")
    (insert (dope--fmt-drugs))
    (insert "\n\n[b]uy [s]ell [j]et  > ")))

;;;
;;;; Supplementary
;;;
;; http://www.emacswiki.org/emacs/ElispCookbook#toc23
(defun group-number (num &optional size char)
    "Format NUM as string grouped to SIZE with CHAR."
    ;; Based on code for `math-group-float' in calc-ext.el
    (let* ((size (or size 3))
           (char (or char ","))
           (str (if (stringp num)
                    num
                  (number-to-string num)))
           (pt (or (string-match "[^0-9a-zA-Z]" str) (length str))))
      (while (> pt size)
        (setq str (concat (substring str 0 (- pt size))
                          char
                          (substring str (- pt size)))
              pt (- pt size)))
      str))

;;;
;;;; ACTIONS
;;;

(defun dope-new-day-at (dest)
  "New day code FIXME DOCSTRING"
  (if (= dope-day *dopewars-days*)
      ;; start end-game procedure FIXME
      (dope-alert "I haven't programmed the endgame sequence yet.")
    (progn
      (setq dope-day (1+ dope-day))
      (setq dope-current-location dest)
      (setq here-drugs ; -- Bust? Cheap?
            (dope-get-here-drugs (dope-get-loc-drug-idxs dope-current-location)))
      ;; Bronx / Ghetto activities?

      ;; apply bank and loan interest
      (dope-apply-interest)
      (dope-print-game))))

(defun dw-do-jet ()
  "Move to a new location. If everything checks out, start a new day."
  (interactive)
  (goto-char (point-max))
  (let ((inhibit-read-only t))
    (insert (format "JET\nWhere you wanna go? [1-%d] > "
                    (length *dopewars-locations*)))
    (let ((dest (dope-select-destination)))
      (if dest
          (progn
            (insert (elt (elt *dopewars-locations* dest) 0) "\n")
            (sleep-for 0 300)
            (dope-new-day-at dest))
        (dope-print-game)))))

(defun dw-do-buy ()
  "Buy a drug, prompting for relevant information."
  (interactive)
  (goto-char (point-max)) ; FIXME How can I prohibit point adjustment?
  (let ((inhibit-read-only t))
    (insert "BUY\n  -> " (dope--fmt-letter-request-str (length here-drugs)))
    (let ((here-index (dope-select-drug (length here-drugs))))
      (when here-index
        (insert (dope--drugname here-index)
                "\n     " (dope--fmt-buying-str here-index)
                "\n  -> How many do you buy? ")
        (let ((num (read-number "How many do you buy? "
                                (min (dope--cancarry)
                                     (dope--canafford here-index)))))
          (when num (dope-add-drug here-index num))))))
  (dope-print-game))

(defun dw-do-sell ()
  "Sell a drug, prompting for relevant information."
  (interactive)
  (let ((inhibit-read-only t))
    (goto-char (point-max))
    (insert "SELL\n  -> " (dope--fmt-letter-request-str (length here-drugs)))
    (let ((here-index (dope-select-drug (length here-drugs))))
      (when here-index
        (insert (dope--drugname here-index))
        (let ((pos (dope-stash-position here-index)))
          (if (not pos)
              (message "You don't have that drug!")
            (insert "\n     " (dope--fmt-selling-str pos)
                    "\n  -> How many do you sell? ")
            (let ((num (read-number "How many do you sell? "
                                    (cadr (nth pos dope-stash)))))
              (when num (dope-sell-drug here-index num))))))))
  (dope-print-game))

(defun dw-do-drop ()
  "Drop some amount of a drug from your stash."
  (interactive)
  (goto-char (point-max))
  (let ((inhibit-read-only t))
    (insert "DROP\n\n" (dope--fmt-drop-drugs) "\n  -> "
            (dope--fmt-letter-request-str (length dope-stash)))
    (let ((stash-index (dope-select-drug (length dope-stash))))
      (when stash-index
        (insert "\n  -> How many do you drop? ")
        (let ((num (read-number "How many do you drop? " 0)))
          (when num (dope-lose-drug stash-index num))))))
  (dope-print-game))

(defun dw-do-nothing ()
  "Do nothing, then refresh the game buffer."
  (interactive)
  (dope-print-game))

(provide 'dopewars)

;;; dope.el ends here

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
;;   ✓ LOAN SHARK
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

(defcustom *dopewars-multipliers*
  [4 4]
  "Dividend and multiplicand for calculating cheap and expensive prices, respectively.
For example, (2 . 6) would mean, when calculating cheap price,
divide price by 2, and when calculating expensive price, multiply
by 6. The defaults are the ones used in the original dopewars."
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

(defmacro dw-stash (query &optional id elem)
  ;; should macro contain index out-of-bounds error-checking?
  (cond ((equal (symbol-name query) "entry") ; ID is dope-stash index
         (cond ((equal (symbol-name elem) "name")
                `(elt (elt *dopewars-drugs* (car (nth ,id dope-stash))) 0))
               ((equal (symbol-name elem) "index") ; *dopewars-drugs* index
                `(car (nth ,id dope-stash)))
               ((equal (symbol-name elem) "quantity")
                `(nth 1 (nth ,id dope-stash)))
               ((equal (symbol-name elem) "cost")
                `(nth 2 (nth ,id dope-stash)))
               (t `(nth ,id dope-stash))))
        ((equal (symbol-name query) "drug") ; ID is here-drugs index
         (let ((pos (seq-position dope-stash (elt (elt here-drugs id) 0)
                                  (lambda (a b) (= (car a) b)))))
           (if pos `(dw-stash entry ,pos ,elem) nil)))
        ((equal (symbol-name query) "size")
         (apply '+ (mapcar 'cadr dope-stash)))
        ((equal (symbol-name query) "space")
         (- *dopewars-stash-max*
            (apply '+ (mapcar 'cadr dope-stash))))))

;;; FIXMEFIXMEFIXME
(defmacro dw-drug (id &optional part)
  ;; Macro for interacting with here-drugs. ID is here-index
  (cond ((equal (symbol-name part) "name")
         `(elt (elt *dopewars-drugs* (elt (elt here-drugs ,id) 0)) 0))
        ((equal (symbol-name part) "drugid")
         `(elt (elt here-drugs ,id) 0))
        ((equal (symbol-name part) "cost")
         `(elt (elt here-drugs ,id) 1))
        (t `(elt here-drugs ,id))))

(defmacro dw-place (id &optional elem)
  "Location macro"
  (cond ((equal (symbol-name elem) "name")
         `(elt (elt *dopewars-locations* ,id) 0))
        ((equal (symbol-name elem) "min")
         `(elt (elt *dopewars-locations* ,id) 1))
        ((equal (symbol-name elem) "max")
         `(elt (elt *dopewars-days* ,id) 2))
        ((equal (symbol-name elem) "police")
         `(elt (elt *dopewars-days* ,id) 3))
        (t `(elt *dopewars-locations* ,id))))

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

;;; FORMATTING FUNCTIONS
;; FIXME: Does this need to be down here? Can it be at the top?
(load-file "dopewars-format.el")

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
  (switch-to-buffer "*dopewars*")
  (buffer-disable-undo "*dopewars*")
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
  (setq dope-loan (floor (* dope-loan (elt *dopewars-rates* 0)))
        dope-bank (floor (* dope-bank (elt *dopewars-rates* 1)))))

(defun dope-gen-drug-price (drug)
  "Take vector DRUG and create drug price within range.
FIXME: This does not take busts and floods into account."
  ((lambda (min max) (+ min (random (- max min))))
   (elt drug 1) (elt drug 2)))

(defun dope-get-loc-drug-idxs (id)
  "Return a vector for location INDEX, with N elements,
wnere N is a random number between the selected location's
min-drugs and max-drugs numbers. The resulting vector should
contain, in order, a random selection of drug indices by use of
dope-filter. FIXME could use clearer documentation.

Also, this should only be run once per turn, at arrival to jetted
location."
  (dope-filter
   (seq-into (number-sequence 0 (1- (length *dopewars-drugs*))) 'vector)
   ((lambda (min max) (+ min (random (- max min))))
    (elt (elt *dopewars-locations* id) 1)
    (elt (elt *dopewars-locations* id) 2))))

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
  (seq-position dope-stash (dw-drug here-id drugid)
                (lambda (a b) (eq (car a) b))))

(defun dope-add-drug (id n)
  "Add N units of drug at ID, a here-index, to stash.
If the drug's already in the stash, add it to the existing entry.
Otherwise, push the drug onto the stash stack. Lessen player cash
by an appropriate amount."
  (let ((drug (dw-drug id)))
    ;; FIXME accomplish with (unless (or A B)) ?
    (cond
     ((= n 0) nil)
     ((or (< n 0) (not (integerp n))) ; negatives and floats
      (message "What the hell kinda number is that?") nil)
     ((< dope-cash (* n (dw-drug id cost)))
      (message "You can't afford it!"))
     ((> n (- *dopewars-stash-max* (dw-stash size)))
      (message "You don't have the space!"))
     (t (let ((pos (dope-stash-position id)))
          (if pos ; EXISTS IN STASH ALREADY
              (let* ((mydrug (dw-stash entry pos))
                     (new-total (+ n (cadr mydrug))))
                (setf (nth pos dope-stash)
                      (list (car mydrug) new-total
                            (/ (+ (* n (dw-drug id cost))
                                  (* (cadr mydrug) (cl-caddr mydrug)))
                               new-total))))
            ;; ELSE: DRUG IS NEW, ADD NEW DRUG
            (push (list (dw-drug id drugid) n (dw-drug id cost)) dope-stash))
          (setq dope-cash (- dope-cash (* n (dw-drug id cost)))))))))

(defun dope-sell-drug (id n)
  "Remove N units of drug at ID, a here-index, from stash.
If N is all of the drug, remove that drug from the stash stack.
Increase player cash by an appropriate amount."
  (let ((pos (dope-stash-position id))) ; position
    (cond ((= n 0) nil)
          ((or (< n 0) (not (integerp n))) ; negs and floats
           (message "What the hell kinda number is that?") nil)
          ((> n (cadr (nth pos dope-stash)))
           (message "You don't have that many!")) ; FIXME sell all?
          (t (setq dope-cash (+ dope-cash (* n (dw-drug id cost))))
             (dope-lose-drug pos n)))))

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
  (sort (seq-copy sequence) (lambda (a b) (< (elt a 0) (elt b 0)))))

(defun dope-number-of-events ()
  "Get a number of events to occur at this new location, between 0 and 3."
  (cond ((>= (random 100) 70) 0)
        ((>= (random 100) 40) 1)
        ((>= (random 100)  5) 2)
        (t 3)))

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

(defun dope-print-game (&optional abbreviate)
  "Wipe buffer and print game state."
  (let ((inhibit-read-only t))
    (erase-buffer)
    (insert (dope--fmt-header)
            (dope--fmt-stat-block))
    (unless abbreviate
      (insert
       "LOCATIONS\n" (dope--fmt-locs)
       "\n\nYOUR STASH\n" (dope--fmt-stash)
       "\n\nHey dude, the drugs for sale here are:\n" (dope--fmt-drugs)
       "\n\n[b]uy [s]ell [d]rop [j]et  > "))))

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

(defun say-and-wait (str) ; Hmm... useful?
  (insert "\n\n    " str)
  (read-key "Press any key..."))

;;;
;;;; ACTIONS
;;;

(defun dope-visit-loanshark ()
  (when (> dope-loan 0)
    (let ((inhibit-read-only t)
          (default-amount (min dope-loan dope-cash)))
      (erase-buffer)
      (dope-print-game t) ; abbrev'd
      (insert "\n\n\n  Do you want to visit the Loan Shark?  [y/n] > ")
      (when (and  (y-or-n-p "Visit Loan Shark?")
                  (> dope-loan 0))
        (insert (format "\n\n    You owe %s, and have %s."
                        (dope--fmt-money dope-loan)
                        (dope--fmt-money dope-cash))
                "\n    How much do you want to pay back? "
                (format "(default %d) " default-amount))
        (let ((amount (read-number "Pay back how much?"
                                   default-amount)))
          (cond ((> amount dope-cash)   ; FIXME if!
                 (say-and-wait "You don't have that much money!"))
                (t
                 (when (> amount dope-loan) (setq amount dope-loan))
                 (setq dope-cash (- dope-cash amount)
                       dope-loan (- dope-loan amount)))))))))

(defun dope-visit-bank ()
  (let ((inhibit-read-only t))
    (erase-buffer)
    (dope-print-game t) ; abbrev'd
    (insert "\n\n\n  Do you want to visit the Bank?  [y/n] > ")
    (when (y-or-n-p "Visit Bank?")
      ;; WORK ON THIS
      )))

(defun dope-new-day-at (dest)
  "New day code FIXME DOCSTRING"
  (if (= dope-day *dopewars-days*)
      ;; start end-game procedure FIXME
      (dope-alert "I haven't programmed the endgame sequence yet.")
    (progn
      (sleep-for 0 300)
      (setq dope-day (1+ dope-day)
            dope-current-location dest
            here-drugs (dope-get-here-drugs
                        (dope-get-loc-drug-idxs dope-current-location)))
      ;; apply bank and loan interest
      (dope-apply-interest)

      ;; Bronx / Ghetto activities?
      (cond ((= dope-current-location 0)
             (dope-visit-loanshark))
            ((= dope-current-location 1)
             (dope-visit-bank)
             ;; dope-visit-guns/pub?
             )))))

(defun dw-do-jet ()
  "Move to a new location. If everything checks out, start a new day."
  (interactive)
  (goto-char (point-max))
  (let ((inhibit-read-only t))
    (insert (format "JET\nWhere you wanna go? [1-%d] > "
                    (length *dopewars-locations*)))
    (let ((dest (dope-select-destination)))
      (when dest
        (insert (dw-place dest name) "\n")
        (dope-new-day-at dest))))
  (dope-print-game))

(defun dw-do-buy ()
  "Buy a drug, prompting for relevant information."
  (interactive)
  (goto-char (point-max)) ; FIXME How can I prohibit point adjustment?
  (let ((inhibit-read-only t))
    (insert "BUY\n  -> " (dope--fmt-letter-request-str (length here-drugs)))
    (let ((here-index (dope-select-drug (length here-drugs))))
      (when here-index
        (insert (dw-drug here-index name)
                "\n     " (dope--fmt-buying-str here-index)
                "\n  -> How many do you buy? ")
        (let ((num (read-number "How many do you buy? "
                                (min (dw-stash space)
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
        (insert (dw-drug here-index name))
        (let ((pos (dope-stash-position here-index)))
          (if (not pos)
              (message "You don't have that drug!")
            (insert "\n     " (dope--fmt-selling-str pos)
                    "\n  -> How many do you sell? ")
            (let ((num (read-number "How many do you sell? "
                                    (dw-stash entry pos quantity))))
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

;;; dopewars.el ends here

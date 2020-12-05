;;; 04.el --- Advent of code 2020 - 04 -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2020 Niko Sten
;;
;;; Commentary:
;;; Blaa blaa
;;
;;; Code:

(provide '04)
(setq lexical-binding t)

(load-file "./helpers.el")

;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; byr (Birth Year)      ;;
;; iyr (Issue Year)      ;;
;; eyr (Expiration Year) ;;
;; hgt (Height)          ;;
;; hcl (Hair Color)      ;;
;; ecl (Eye Color)       ;;
;; pid (Passport ID)     ;;
;; cid (Country ID)      ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;

(cl-defstruct passport byr iyr eyr hgt hcl ecl pid cid)

;; Parsing

(defun set-passport-field (p field)
  (pcase field
    (`(,"byr" ,byr) (setf (passport-byr p) (string-to-number byr)))
    (`(,"iyr" ,iyr) (setf (passport-iyr p) (string-to-number iyr)))
    (`(,"eyr" ,eyr) (setf (passport-eyr p) (string-to-number eyr)))
    (`(,"hgt" ,hgt) (setf (passport-hgt p) hgt))
    (`(,"hcl" ,hcl) (setf (passport-hcl p) hcl))
    (`(,"ecl" ,ecl) (setf (passport-ecl p) ecl))
    (`(,"pid" ,pid) (setf (passport-pid p) pid))
    (`(,"cid" ,cid) (setf (passport-cid p) (string-to-number cid)))))

(defun fields-as-record (fields)
  "Take a FIELDS and return a passport struct. FIELDS is like (('byr' 1940) ('cid' 294)..)."
  (let ((p (make-passport)))
    (progn
      (while fields
        (let ((field (pop fields)))
          (set-passport-field p field)))
      p)))


(defun merge-record-lines (rec-lines)
  "Merge REC-LINES in passport into list of lists like (('byr' 1940) ('cid' 294)..)."
  (seq-map (lambda (x) (split-string x ":"))
           (seq-mapcat (lambda (x) (split-string x " ")) rec-lines)))

(defun parse-records (file)
  "Parse records from FILE and create list of passport records from it."
  (seq-map 'fields-as-record
           (seq-map 'merge-record-lines
                    (seq-filter 'identity
                                (split-list-by (read-lines file) "")))))


;; Logic

(defun is-valid-passport-part-1 (passport)
  "Check if PASSPORT record is valid."
  (=
   (length
    (seq-filter 'identity
                (list (passport-byr passport)
                      (passport-iyr passport)
                      (passport-eyr passport)
                      (passport-hgt passport)
                      (passport-hcl passport)
                      (passport-ecl passport)
                      (passport-pid passport))))
   7))

(defun valid-hcl (hcl)
  "Validate hair color (HCL)."
  (s-matches-p (rx bol "#" (repeat 6 (in "a-f" "0-9")) eol) hcl))

(defun valid-ecl (ecl)
  "Validate eye color (ECL)."
  (s-matches-p (rx bol (or "amb" "blu" "brn" "gry" "grn" "hzl" "oth") eol) ecl))

(defun valid-pid (pid)
  "Validate pid (PID)."
  (string-match-p (rx bol (repeat 9 digit) eol) pid))

(defun valid-hgt (hgt)
  "Validate height (HGT)."
  (and
   (string-match-p (rx bol (or (seq (repeat 3 digit) "cm")
                               (seq (repeat 2 digit) "in"))
                       eol)
                   hgt)
   (if (equal (substring hgt -1) "n")
       (let ((height (string-to-number (substring hgt 0 2))))
         (<= 59 height 76))
     (let ((height (string-to-number (substring hgt 0 3))))
       (<= 150 height 193)))))

(defun is-valid-passport-part-2 (passport)
  "Check if PASSPORT record is valid."
  (and (is-valid-passport-part-1 passport)
       (<= 1920 (passport-byr passport) 2002)
       (<= 2010 (passport-iyr passport) 2020)
       (<= 2020 (passport-eyr passport) 2030)
       (valid-hgt (passport-hgt passport))
       (valid-hcl (passport-hcl passport))
       (valid-ecl (passport-ecl passport))
       (valid-pid (passport-pid passport))))

(defun solve-part-1 ()
  "Solves the advent of code 2020 04 - part 1."
  (seq-count 'is-valid-passport-part-1 (parse-records "04.input")))

(defun solve-part-2 ()
  "Solves the advent of code 2020 04 - part 2."
  (seq-count 'is-valid-passport-part-2 (parse-records "04.input")))

(solve-part-1)
(solve-part-2)

;;; 04.el ends here

;;; 02.el --- Advent of code 2020 - 02 -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2020 Niko Sten
;;
;;
;;; Commentary:
;;
;;; Code:


(provide '02)

(load-file "./helpers.el")

(setq data (read-lines "02.input"))

(defun as-typed-record (record)
  "Read string values of a RECORD and change them into proper types."
  (list (string-to-number (pop record))
        (string-to-number (pop record))
        (string-to-char (pop record))
        (pop record)))

(defun destruct-a-record (line-of-text)
  "Break a LINE-OF-TEXT into a record of (min max ch password)."
  (split-string line-of-text "[-: ]" t))

(setq records (mapcar 'as-typed-record
                      (mapcar 'destruct-a-record data)))

;; Solution for part 01

(defun good-password-by-policy-1 (low high ch pw)
  "Check for password validity. PW must has CH occurrences between LOW and HIGH."
  (let ((occurrences (ch-occurrences ch pw)))
    (<= low occurrences high)))

(cl-loop for (l h c s) in records
         count (good-password-by-policy-1 l h c s))

;; Solution for part 02

(defun good-password-by-policy-2 (low high ch pw)
  "Check for pasword validity. PW mus has CH in LOW and HIGH positions of string."
  (let ((low-ch (seq-elt pw (- low 1)))
        (high-ch (seq-elt pw (- high 1))))
    (= (seq-count (lambda (x) (= ch x)) (list low-ch high-ch)) 1)))

(cl-loop for (l h c s) in records
         count (good-password-by-policy-2 l h c s))

;;; 02.el ends here

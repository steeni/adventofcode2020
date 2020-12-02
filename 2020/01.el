;;; 01.el -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2020 Niko Sten
;;
;;; Commentary:
;;
;;; Code:

(provide '01)
(require 'generator)

(setq lexical-binding t)

;; Santas little helper functions
(defun read-lines (filePath)
  "Return a list of lines of a FILEPATH."
  (with-temp-buffer
    (insert-file-contents filePath)
    (split-string (buffer-string) "\n" t)))

(iter-defun combinations-2 (coll)
  "Genarates combinations for COLL."
  (dolist (x coll)
    (dolist (y coll)
      (iter-yield (list x y)))))

(iter-defun combinations-3 (coll)
  (dolist (x coll)
    (dolist (y coll)
      (dolist (z coll)
        (iter-yield (list x y z))))))

;; Solution

(defun solve1 (numbers)
  "Solves advent of code 2020 task 1 part 1 with 'cl-loop' and combinations generator for NUMBERS."
  (cl-loop for (x y) iter-by (combinations-2 numbers)
           until (= (+ x y) 2020)
           finally return (* x y)))

(defun solve2 (numbers)
  "Solves advent of code 2020 task 1 part 2 with 'cl-loop' and combinations generator for NUMBERS."
  (cl-loop for (x y z) iter-by (combinations-3 numbers)
           until (= (+ x y z) 2020)
           finally return (* x y z)))

(defun solve-01 ()
  "Solves advent of code 01. Assumes that input is in 01.input file."
  (let* ((lines (read-lines "01.input"))
         (numbers (seq-map 'string-to-number lines))
         (sorted (sort numbers '<)))
    (list (solve1 sorted)
          (solve2 sorted))))

(solve-01)


;;; 01.el ends here

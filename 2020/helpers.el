;;; helpers.el --- Santas little helper functions -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2020 Niko Sten

;;; Commentary:
;;;
;;; Helper functions for I/O and other simple things.

(provide 'helpers)

;;; Code:
(defun read-lines (filePath)
  "Return a list of lines of a FILEPATH."
  (with-temp-buffer
    (insert-file-contents filePath)
    (split-string (buffer-string) "\n" t)))

(iter-defun combinations-2 (coll)
  "Genarates 2-way combinations for COLL."
  (dolist (x coll)
    (dolist (y coll)
      (iter-yield (list x y)))))

(iter-defun combinations-3 (coll)
  "Genarates 2-way combinations for COLL."
  (dolist (x coll)
    (dolist (y coll)
      (dolist (z coll)
        (iter-yield (list x y z))))))

(defun ch-occurrences (ch str)
  "Find how many CH found in STR."
  (seq-count (lambda (c) (equal c ch)) str))

;;; helpers.el ends here

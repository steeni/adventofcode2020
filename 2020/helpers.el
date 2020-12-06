;;; helpers.el --- Santas little helper functions -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2020 Niko Sten

;;; Commentary:
;;;
;;; Helper functions for I/O and other simple things.

(provide 'helpers)
(setq lexical-binding t)

;;; Code:
(defun read-lines (filePath &optional omit-nulls)
  "Return a list of lines of a FILEPATH optionally OMIT-NULLS."
  (with-temp-buffer
    (insert-file-contents filePath)
    (split-string (buffer-string) "\n" omit-nulls)))

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

(defun split-list-by (lst by)
  "Split list (LST) by (BY) element creating groups from data in between."
  (seq-reduce (lambda (a b)
                (if (equal b by)
                    (cons (list) a)
                  (cons (cons b (car a)) (cdr a))))
              lst
              (list)))

;;; helpers.el ends here

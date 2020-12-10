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

(defun comp (a b)
  "Combine 2 functions (A (B ...)). Use fset to bind function."
  (lambda (&rest args)
    (funcall a (apply b args))))

(defun comp-n (&rest functions)
  "Combine n FUNCTIONS (A (B .. (N..))). Use fset to bind function."
  (lambda (&rest args)
    (seq-first
     (seq-reduce (lambda (a f)
                   (list (apply f a)))
                 functions
                 args))))

(defun intersect-2 (a b)
  "Intersects 2 sequences A and B."
  (seq-reduce (lambda (acc item)
                (if (member item b)
                    (cons item acc)
                  acc))
              a nil))

(defun intersect-n (&rest sets)
  (seq-reduce (lambda (acc seq)
                (intersect-2 acc seq))
              (cdr sets) (car sets)))

(defun numeric-string (str)
  (string-match-p "\\`[1-9][0-9]*\\'" str))

(defun iterate-while (pred-fun iterator-fun init)
  "Call ITERATOR-FUN for INIT while PRED-FUN produces true for INIT. Result of previous iteration will INIT next."
    (let ((data init))
      (progn
        (while (funcall pred-fun data)
          (setq data (funcall iterator-fun data)))
        data)))

(defun iterate-until (pred-fun iterator-fun init)
  "Call ITERATOR-FUN for INIT until PRED-FUN produces true for INIT. Result of previous iteration will INIT next."
  (let ((data init))
    (progn
      (while (not (funcall pred-fun data))
        (setq data (funcall iterator-fun data)))
      data)))

(defun string-to-list (str)
  "Convert string (STR) sequence to list sequence."
  (seq-into str 'list))

;;; helpers.el ends here

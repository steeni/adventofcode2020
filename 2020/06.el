;;; 06.el --- Advent of code 2020 - 06 -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2020 Niko Sten
;;
;;; Commentary:
;;
;;; Code:

(provide '06)
(load-file "./helpers.el")

(fset 'list-concat (apply-partially 'seq-concatenate 'list))

(defun part-1 (file)
  (length
   (apply 'list-concat
          (seq-map (comp-n 'string-join 'string-to-list 'delete-dups)
                   (seq-filter 'identity
                               (split-list-by (read-lines file) ""))))))

(part-1 "06.input")

(defun part-2 (file)
  (length
   (apply 'list-concat
          (seq-map (lambda (x) (apply 'intersect-n (seq-map 'string-to-list x)))
                   (seq-filter 'identity
                               (split-list-by (read-lines "06.input") ""))))))

(part-2 "06.input")
;;; 06.el ends here


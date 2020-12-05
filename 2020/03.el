;;; 03.el --- Advent of code 2020 - 03 -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2020 Niko Sten
;;
;;; Commentary:
;;; The code reads 03.input file into buffer and runs elisp buffer operations
;;; to read the number of trees along the path.
;;;
;;; If you want to see how it works, you can
;;;  1. Open elisp file an evaluate it
;;;  2. Open *scartch* buffer
;;;  3. Evaluate (insert-file-contents "03.input") to read file contents into scratch
;;;  4. Evaluate (buffer-tree-search 3 1) for part 1
;;;     - you should see buffer reducing in a way that leaves the path into left most column
;;
;;; Code:


(provide '03)
;;; 03.el ends here

(defun lines (size width)
  "Calculate how many lines based on SIZE and WIDTH. Works only for files that has constant WIDTH."
  ;; Algorithm is to calculate how factor of \n in the file and then multiply size with that
  (* (/ 1.0 (+ width 1)) size))

(defun x-positions (width size x-move y-move)
  "Get sequence of column locations for infinite width map of pattern WIDTH, SIZE, X-MOVE, Y-MOVE movement."
  (mapcar (lambda (x)
            (mod (* x x-move) width))
          (number-sequence 0 (- (fceiling (/ (lines size width) y-move)) 1))))

(defun buffer-tree-search (x-move y-move)
  "Scan through the buffer and search for trees. Give movement with X-MOVE and Y-MOVE."
  (goto-char (point-min))
  (let* ((width (- (line-end-position)
                   (line-beginning-position))))
    (seq-map (lambda (x)
               (let* ((_ (delete-char x))
                      (ch (char-after))
                      (__ (forward-line y-move)))
                 ch))
             (x-positions width (buffer-size) x-move y-move))))

(defun trees (x-move y-move)
  "Find the trees along X-MOVE, Y-MOVE path from the input file."
  (with-temp-buffer
    (insert-file-contents "03.input")
    (buffer-tree-search x-move y-move)))

(defun how-many-trees (x-move y-move)
  "Find out how many trees in the input file along the path X-MOVE and Y-MOVE."
  (seq-count (lambda (x) (eq x ?#)) (trees x-move y-move)))

;; Part 1
(message (format "Part 1: %s" (how-many-trees 3 1)))

;; Part 2
(message (format "Part 2: %s" (*
                               (how-many-trees 1 1)
                               (how-many-trees 3 1)
                               (how-many-trees 5 1)
                               (how-many-trees 7 1)
                               (how-many-trees 1 2))))

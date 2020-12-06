;;; 05.el --- Advent of code 2020 - 05 -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2020 Niko Sten
;;
;;; Commentary:
;;
;;; Code:



(provide '05)

(load-file "./helpers.el")

(setq row-init (number-sequence 0 127))
(setq col-init (number-sequence 0 7))

(defun binary-decoder (code coll low-ch)
  "Binary decodes CODE into a number using COLL as a source of data and LOW-CH as separator."
  (if (= (length coll) 1)
      (seq-first coll)
    (let ((reduction (expt 2 (- (length code) 1))))
      (if (equal (elt code 0) low-ch)
          (binary-decoder (substring code 1) (seq-take coll reduction) low-ch)
        (binary-decoder (substring code 1) (seq-drop coll reduction) low-ch)))))

(defun decode-row (code)
  "Binary decode a seat row number from CODE."
  (binary-decoder (substring code 0 7) row-init ?F))

(defun decode-col (code)
  "Binary decode a seat col number from CODE."
  (binary-decoder (substring code 7 10) col-init ?L))

(defun seat-id (row col)
  "Calculate seat-id from ROW and COL numbers."
  (+ (* row 8) col))

(defun decode-seat (code)
  "Decode 10char length seat CODE into a seat-id."
  (let ((row (decode-row code))
        (col (decode-col code)))
    (seat-id row col)))

;; Search Part 1 solution
(defun solve-part-1 ()
  "Solve first part of the task 05."
  (seq-first (seq-sort '> (seq-map 'encode-seat (read-lines "05.input" t)))))

;; ------------------

(defun generate-plane-seats ()
  "Generate an empty grid of seats."
  (let ((rows (make-vector 128 nil)))
    (progn
      (dotimes (i 128) (aset rows i (seq-into (number-sequence 0 7) 'vector)))
      rows)))

(defun fill-the-seats (row-data)
  "Fill the seats based on ROW-DATA."
  (seq-reduce (lambda (seats code)
                (progn
                  (let ((row (decode-row code))
                        (col (decode-col code)))
                    (setf (elt (elt seats row) col) nil)
                    seats)))
              row-data
              (generate-plane-seats)))

(defun find-my-seat (seated-seats)
  "Find my seat from the plane."
  (seq-find 'identity
            (seq-map-indexed (lambda (el id)
                               (if (equal (seq-count 'identity el) 1)
                                   (list id (seq-find 'identity el))
                                 nil))
                             seated-seats)))

(defun solve-part-2 ()
  "Solve second part of the task 05."
  (let ((seat (find-my-seat (fill-the-seats (read-lines "05.input" t)))))
    (seat-id (car seat) (cadr seat))))

(solve-part-1)
(solve-part-2)


;;; 05.el ends here

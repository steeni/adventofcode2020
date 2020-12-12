;;; 09.el --- AOC - 2020 - 09 -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2020 Niko Sten
;
;;; Commentary:
;;
;;; Code:
(setq lexical-binding t)


(provide '09)
(load-file "./helpers.el")

(defun read-xmas ()
  "Read XMAS input from file."
  (seq-map 'string-to-number (read-lines "09.input")))

(defun summable (numbers needle)
  "Check if there is pair in NUMBERS that sums up to NEEDLE."
  (cl-loop for (a b) iter-by (combgen numbers)
           when (= (+ a b) needle) return t))

(defun next-xmas (s)
  (seq-let (r l) s
    (progn
      (ring-insert r (car l))
      (pop l)
      (list r l))))

(defun valid-xmas (s)
  (seq-let (r l) s
    (or (< (ring-length r) 25)
        (summable (ring-elements r) (seq-first l)))))

(defun xmas-part-1 ()
  "Solve xmas part 1."
    (seq-first
     (nth 1
          (iterate-while 'valid-xmas
                         'next-xmas
                         (list (make-ring 25)
                               (read-xmas))))))

(defun seq-sums-up-to (sum-goal coll)
  "Return non-nil if COLL start a sequence which sums up to SUM-GOAL."
  (=
   sum-goal
   (cl-loop for v in coll
            sum v into s
            while (< s sum-goal)
            finally return s)))

(defun find-seq-that-sums-up-to (sum-goal coll)
  "Return first sequenece from COLL that sums up to SUM-GOAL."
  (cl-loop for v on coll
           until (seq-sums-up-to sum-goal v)
           finally return (cl-loop for x in v
                                   collect x into a
                                   until (= (apply '+ a) sum-goal)
                                   finally return a)))

(defun xmas-part-2 ()
  (let ((seq (sort (find-seq-that-sums-up-to (xmas-part-1) (read-xmas)) '<)))
    (+ (seq-first seq) (car (last seq)))))

;;; 09.el ends here

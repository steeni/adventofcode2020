;;; 07.el --- AOC 2020 - 07 -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2020 Niko Sten
;;
;;; Commentary:
;;
;;; Code:


(provide '07)
(load-file "./helpers.el")

;; Parsing

(rx-define maybe-bag
  (optional
   (group digit)
   " "
   (group (* word) " " (* word))
   " bag"
   (* (or "," space "s"))))

(rx-define bags-re
       (seq bol
            (group (* word) " " (* word))
            " bags contain "
            maybe-bag maybe-bag maybe-bag maybe-bag maybe-bag))

(defun parse-bags (file)
  (seq-map (lambda (x)
             (seq-drop (seq-first (s-match-strings-all (rx bags-re) x)) 1))
           (read-lines file t)))

;; Building a bag mapping (contained-by & contains)

(defun directly-contained-by-mapping (bag-data)
  "Build a map of bag -> (bags that directly contain a bag) from BAG-DATA."
  (seq-reduce (lambda (bag-map bags)
                (let ((container (pop bags)))
                  (dolist (key (seq-remove 'numeric-string bags))
                    (let ((val (gethash key bag-map)))
                      (if val
                          (puthash key (cons container val) bag-map)
                        (puthash key (list container) bag-map))))
                  bag-map))
              bag-data
              (make-hash-table :test 'equal)))

(defun directly-contains-mapping (bag-data)
  "Create a mapping bag -> list of bag-names repeated as many times they exists in the bag from BAG-DATA."
  (seq-reduce (lambda (bag-map bags)
                (let ((container (pop bags)))
                  (progn
                    (puthash container (seq-mapcat (lambda (x)
                                                     (make-list (string-to-number (car x)) (cadr x)))
                                                   (seq-partition bags 2)) bag-map)
                    bag-map)))
              bag-data
              (make-hash-table :test 'equal)))

;; Graph traversal

(defun expand-all (mapping key)
  "Expand and collect all values followed in a MAPPING starting from KEY."
  (seq-concatenate 'list
                    (gethash key mapping)
                    (seq-mapcat (lambda (b)
                                  (expand-all mapping b))
                                (gethash key mapping))))

;; Part 1 solution
(length (delete-dups
         (expand-all (directly-contained-by-mapping (parse-bags "07.input")) "shiny gold")))

;; Part 2 solution
(length (expand-all (directly-contains-mapping (parse-bags "07.input")) "shiny gold"))

;;; 07.el ends here

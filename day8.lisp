;;; Advent of Code 2022 Day 8
;;; Adapted from a REDIT post, author unknown

(defpackage :aoc2022
  (:use :cl)
  (:export day8 day8a day8b))

(in-package :aoc2022)

(defun val (c)
  (declare (standard-char c))
  (- (char-int c) (char-int #\0)))

(defun parse-line (line)
  ;; Convert the string into a list of heights
  (map 'list #'val line))

(defun read-input (filename)
  (let* ((lines (uiop:read-file-lines filename))
         (dim0 (length lines))
         (dim1 (length (first lines))))
    (make-array (list dim0 dim1)
                :initial-contents (mapcar #'parse-line lines))))

(defun explore (grid rr cc rinc cinc)
  (declare (fixnum rr cc rinc cinc))
  (loop with height = (aref grid rr cc)
        for _r = (+ rr rinc) then (+ _r rinc)
        for _c = (+ cc cinc) then (+ _c cinc)
        while (array-in-bounds-p grid _r _c)
        count (< (aref grid _r _c) height)))

(defun visible (grid rr cc)
  (declare (fixnum rr cc))
  (or (= (explore grid rr cc -1 0) rr)
      (= (explore grid rr cc 0 -1) cc)
      (= (explore grid rr cc 1 0) (- (array-dimension grid 0) rr 1))
      (= (explore grid rr cc 0 1) (- (array-dimension grid 1) cc 1))))

(defun day8a (grid)
  (destructuring-bind (r c) (array-dimensions grid)
    (loop for rr from 0 below r
          summing (loop for cc from 0 below c
                        counting (visible grid rr cc)))))


(defun explore2 (grid rr cc rinc cinc)
  (declare (fixnum rr cc rinc cinc))
  (loop with count = 0
        for _r = (+ rr rinc) then (+ _r rinc)
        for _c = (+ cc cinc) then (+ _c cinc)
        while (array-in-bounds-p grid _r _c)
        if (< (aref grid _r _c) (aref grid rr cc))
         do (incf count)
        else
         do (return (1+ count)) ;; we need to include the tall tree
        finally (return count)))

(defun scenic-score (grid rr cc)
  (declare (fixnum rr cc))
  (* (explore2 grid rr cc -1 0)
     (explore2 grid rr cc 0 -1)
     (explore2 grid rr cc 1 0)
     (explore2 grid rr cc 0 1)))

(defun day8b (grid)
  (loop for rr from 0 below (array-dimension grid 0)
        maximizing (loop for cc from 0 below (array-dimension grid 1)
                         maximizing (scenic-score grid rr cc))))


(defun day8 (&optional (filename "day_8.txt"))
  (let ((input (read-input filename)))
    (values (day8a input)
            (day8b input))))


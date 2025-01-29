;; -*- lexical-binding: t -*-

(defun read-grid (file)
  (with-temp-buffer
    (insert-file-contents file)
    (let* ((width (1- (line-end-position)))
	   (height (line-number-at-pos (1- (point-max))))
	   (grid (make-grid height width)))
      (dotimes (i height)
	(let ((row (buffer-substring (point) (line-end-position))))
	  (dotimes (j width)
	    (gset grid (cons i j) (aref row j)))
	  (forward-line)))
      grid)))

(defun make-grid (height width &optional blank)
  (let ((grid (make-vector height nil)))
    (dotimes (i height)
      (aset grid i (make-vector width (or blank ?.))))
    grid))

(defun grid-height (grid)
  (length grid))

(defun grid-width (grid)
  (length (aref grid 0)))

(defun gset (grid pos value)
  "Ignores out-of-bounds references"
  (pcase-let* ((`(,i . ,j) pos))
    (when (and (>= i 0) (< i (length grid)))
      (let ((row (aref grid i)))
	(when (and (>= j 0) (< j (length row)))
	  (aset row j value))))))

(defun gref (grid pos)
  "Returns nil for out-of-bounds references"
  (pcase-let* ((`(,i . ,j) pos))
    (when (and (>= i 0) (< i (length grid)))
      (let ((row (aref grid i)))
	(when (and (>= j 0) (< j (length row)))
	  (aref row j))))))

(defun grid-like (grid &optional blank)
  (make-grid (grid-height grid) (grid-width grid) blank))

(defun insert-grid (grid)
  (seq-do (lambda (row)
	    (seq-do #'insert row)
	    (insert ?\n))
	  grid))

(defun map-grid (function grid)
  (dotimes (i (grid-height grid))
    (dotimes (j (grid-width grid))
      (let ((pos (cons i j)))
	(funcall function pos (gref grid pos))))))

(defun find-reindeer (grid)
  (catch :found
    (map-grid (lambda (pos value)
		(when (eq value ?S)
		  (throw :found (list :east 0 pos))))
	      grid)))

(defun find-end (grid)
  (catch :found
    (map-grid (lambda (pos value)
		(when (eq value ?E)
		  (throw :found pos)))
	      grid)))

(defun score (from to)
  (if (eq from to)
      1
    (let* ((dirs '(:east :north :west :south))
	   (fn (seq-position dirs from))
	   (tn (seq-position dirs to)))
      (pcase (abs (- fn tn))
	(1 1001)
	(2 2001)
	(3 1001)))))

(defun around (deer)
  (pcase-let ((`(_ _ (,i . ,j)) deer))
    (list (list :east (cons i (1+ j)))
	  (list :north (cons (1- i) j))
	  (list :west (cons i (1- j)))
	  (list :south (cons (1+ i) j)))))

(defun passable (tile)
  (or (eq tile ?.) (eq tile ?E)))

(defun moves (grid deer)
  (let ((moves nil))
    (dolist (a (around deer))
      (pcase-let ((`(,to ,pos) a))
	(when (passable (gref grid pos))
	  (pcase-let* ((`(,from ,score) deer))
	    (let ((score (+ score (score from to))))
	      (push (list to score pos) moves))))))
    moves))

(defun step (grid scores deer)
  (pcase-let* ((`(_ ,score ,pos) deer))
    (when (< score (gref scores pos))
      (gset scores pos score)
      (unless (eq (gref grid pos) ?E)
	(dolist (deer (moves grid deer))
	  (step grid scores deer))))))

(defun puzzle-16a ()
  ;; Needed to set (setq max-lisp-eval-depth 5000) to avoid a stack
  ;; overflow with the larger puzzle input
  (let* ((grid (read-grid "data/example-16.txt"))
	 (scores (grid-like grid (expt 2 32)))
	 (deer (find-reindeer grid))
	 (end (find-end grid)))
    (step grid scores deer)
    (gref scores end)))

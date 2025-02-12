;; -*- lexical-binding: t -*-

(defun read-line ()
  (buffer-substring (point) (line-end-position)))

(defun read-positions (file)
  (with-temp-buffer
    (insert-file-contents file)
    (let ((positions nil))
      (while (not (eobp))
	(let* ((line (read-line))
	       (xy (split-string line ","))
	       (pos (cons (read (cadr xy)) (read (car xy)))))
	  (push pos positions))
	(forward-line))
      (nreverse positions))))

(defun make-grid (height width &optional blank)
  (let ((grid (make-vector height nil)))
    (dotimes (i height)
      (aset grid i (make-vector width blank)))
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

(defun copy-grid (grid)
  (let ((new (grid-like grid)))
    (dotimes (i (grid-height new))
      (aset new i (copy-sequence (aref grid i))))
    new))

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

(defun dimensions (positions)
  (let ((width 0)
	(height 0))
    (dolist (pos positions)
      (setq height (max height (car pos)))
      (setq width (max width (cdr pos))))
    (cons (1+ height) (1+ width))))

(defun drop-bytes (memory positions)
  (dolist (pos positions)
    (gset memory pos ?#)))

(defun around (pos)
  (pcase-let ((`(,i . ,j) pos))
    ;; try down and right first`
    (list (cons i (1+ j))
	  (cons (1+ i) j)
	  (cons (1- i) j)
	  (cons i (1- j)))))

(defun passable (tile)
  (eq tile ?.))

(defun moves (memory pos)
  (seq-filter (lambda (pos)
		(passable (gref memory pos)))
	      (around pos)))

(defun step (memory scores score pos end)
  (unless (>= score (gref scores pos))
    (gset scores pos score)
    (unless (equal pos end)
      (dolist (pos (moves memory pos))
	(step memory scores (1+ score) pos end)))))

(defun puzzle-18a ()
  (let ((positions (read-positions "data/input-18.txt")))
    (pcase-let ((`(,height . ,width) (dimensions positions)))
      (let* ((max-lisp-eval-depth 5000)
	     (memory (make-grid height width ?.))
	     (scores (grid-like memory (expt 2 16)))
	     (start (cons 0 0))
	     (end (cons (1- height) (1- width))))
	(drop-bytes memory (seq-take positions 1024))
	(step memory scores 0 start end)
	(gref scores end)))))

;; For part 2, manually binary-search for the blocking byte

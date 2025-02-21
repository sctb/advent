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
      (aset grid i (make-vector width blank)))
    grid))

(defun insert-grid (grid)
  (seq-do (lambda (row)
	    (seq-do #'insert row)
	    (insert ?\n))
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

(defun map-grid (function grid)
  (dotimes (i (grid-height grid))
    (dotimes (j (grid-width grid))
      (let ((pos (cons i j)))
	(funcall function pos (gref grid pos))))))

(defun find (grid c)
  (catch :found
    (map-grid (lambda (pos value)
		(when (eq value c)
		  (throw :found pos)))
	      grid)))

(defun cheatable (a b picos)
  (let ((n (+ (abs (- (car a) (car b)))
	      (abs (- (cdr a) (cdr b))))))
    (when (and (>= n 2)
	       (<= n picos))
      n)))

(defun around (pos)
  (pcase-let ((`(,i . ,j) pos))
    (list (cons i (1+ j))
	  (cons (1+ i) j)
	  (cons (1- i) j)
	  (cons i (1- j)))))

(defun next (grid pos trail)
  (catch :next
    (dolist (pos (around pos))
      (unless (or (eq (gref grid pos) ?#)
		  (equal pos (cadr trail)))
	(throw :next pos)))))

(defun trace (grid start end)
  (let ((pos start)
	(trail nil))
    (while (not (equal pos end))
      (push pos trail)
      (setq pos (next grid pos trail)))
    (push end trail)
    (nreverse trail)))

(defun cheats (trail picos)
  (let ((count 0)
	(here trail))
    (while here
      (let ((delta 2)
	    (there (cddr here)))
	(while there
	  (when-let* ((from (car here))
		      (to (car there))
		      (spent (cheatable from to picos)))
	    (let ((saved (- delta spent)))
	      (when (>= saved 100)
		(setq count (1+ count)))))
	  (setq delta (1+ delta))
	  (setq there (cdr there))))
      (setq here (cdr here)))
    count))

(defun puzzle-20a ()
  (let* ((grid (read-grid "data/input-20.txt"))
	 (start (find grid ?S))
	 (end (find grid ?E))
	 (trail (trace grid start end)))
    (cheats trail 2)))

(defun puzzle-20b ()
  (let* ((grid (read-grid "data/input-20.txt"))
	 (start (find grid ?S))
	 (end (find grid ?E))
	 (trail (trace grid start end)))
    (cheats trail 20)))

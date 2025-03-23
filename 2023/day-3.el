;; -*- lexical-binding: t -*-

(defun read-grid (file)
  (with-temp-buffer
    (insert-file-contents file)
    (let* ((height (line-number-at-pos (1- (point-max))))
	   (grid (make-vector height nil)))
      (dotimes (i height)
	(let ((row (buffer-substring (point) (line-end-position))))
	  (aset grid i row)
	  (forward-line)))
      grid)))

(defun insert-grid (grid)
  (seq-do (lambda (row)
	    (insert row)
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

(defun puzzle-3a ()
  (let ((grid (read-grid "data/example-3.txt")))
    (insert-grid grid)))

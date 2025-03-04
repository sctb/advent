;; -*- lexical-binding: t -*-

(defun next-paragraph ()
  (save-excursion
    (forward-paragraph)
    (1- (point))))

(defun read-line ()
  (buffer-substring (point) (line-end-position)))

(defun read-grid ()
  (let ((width (- (line-end-position) (point))))
    (when (> width 0)
      (let* ((line (line-number-at-pos))
	     (height (- (line-number-at-pos (next-paragraph)) line))
	     (grid (make-grid height width)))
	(dotimes (i height)
	  (let ((row (read-line)))
	    (dotimes (j width)
	      (gset grid (cons i j) (aref row j)))
	    (forward-line)))
	(forward-line)
	(forward-line)
	grid))))

(defun read-grids (file)
  (with-temp-buffer
    (insert-file-contents file)
    (let ((grids nil))
      (while-let ((grid (read-grid)))
	(push grid grids))
      (nreverse grids))))

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

(defun lock-p (grid)
  (eq (gref grid (cons 0 0)) ?#))

(defun puzzle-25a ()
  (let* ((file "data/example-25.txt")
	 (grid (read-grids file)))
    (mapcar #'lock-p locks)))

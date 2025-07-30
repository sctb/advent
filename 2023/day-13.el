;; -*- lexical-binding: t -*-

(defun next-paragraph ()
  (save-excursion
    (forward-paragraph)
    (1- (point))))

(defun read-line ()
  (buffer-substring (point) (line-end-position)))

(defun read-grid ()
  (when (> (- (line-end-position) (point)) 0)
    (let* ((line (line-number-at-pos))
	   (height (- (line-number-at-pos (next-paragraph)) line))
	   (grid (make-vector height nil)))
      (dotimes (i height)
	(let ((row (read-line)))
	  (aset grid i row)
	  (forward-line)))
      (forward-line)
      (forward-line)
      grid)))

(defun read-grids (file)
  (with-temp-buffer
    (insert-file-contents file)
    (let ((grids nil))
      (while-let ((grid (read-grid)))
	(push grid grids))
      (nreverse grids))))

(defun grid-height (grid)
  (length grid))

(defun grid-width (grid)
  (length (aref grid 0)))

(defun gset (grid pos value)
  (pcase-let ((`(,i . ,j) pos))
    (when (and (>= i 0) (< i (length grid)))
      (let ((row (aref grid i)))
	(when (and (>= j 0) (< j (length row)))
	  (aset row j value))))))

(defun gref (grid pos)
  (pcase-let ((`(,i . ,j) pos))
    (when (and (>= i 0) (< i (length grid)))
      (let ((row (aref grid i)))
	(when (and (>= j 0) (< j (length row)))
	  (aref row j))))))

(defun puzzle-13a ()
  (let* ((file "data/example-13.txt")
	 (grids (read-grids file)))
    (length grids)))

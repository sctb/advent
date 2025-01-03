;; -*- lexical-binding: t -*-

(defun read-grid (file)
  (with-temp-buffer
    (insert-file-contents file)
    (let* ((w (1- (line-end-position)))
	   (h (line-number-at-pos (1- (point-max))))
	   (g (make-grid h w)))
      (dotimes (i h)
	(let ((s (buffer-substring (point) (line-end-position))))
	  (dotimes (j w)
	    (gset g i j (aref s j)))
	  (forward-line)))
      g)))

(defun make-grid (h w)
  (let ((g (make-vector h nil)))
    (dotimes (i h)
      (aset g i (make-vector w ?.)))
    g))

(defun copy-grid (g)
  (let* ((h (length g))
	 (c (make-vector h nil)))
    (dotimes (i h)
      (aset c i (copy-sequence (aref g i))))
    c))

(defun reset-grid (a b)
  (dotimes (i (grid-width a))
    (dotimes (j (grid-width a))
      (gset b i j (gref a i j)))))

(defun grid-height (g)
  (length g))

(defun grid-width (g)
  (length (aref g 0)))

(defun insert-grid (g)
  (seq-do (lambda (r) (seq-do #'insert r) (insert ?\n)) g))

(defun gset (g i j v)
  "Ignores out-of-bounds references"
  (when (and (>= i 0) (< i (length g)))
    (let ((r (aref g i)))
      (when (and (>= j 0) (< j (length r)))
	(aset r j v)))))

(defun gref (g i j)
  "Returns nil for out-of-bounds references"
  (when (and (>= i 0) (< i (length g)))
    (let ((r (aref g i)))
      (when (and (>= j 0) (< j (length r)))
	(aref r j)))))

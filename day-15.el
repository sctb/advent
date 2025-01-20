;; -*- lexical-binding: t -*-

(defun read-warehouse (file)
  (with-temp-buffer
    (insert-file-contents file)
    (let ((w (1- (line-end-position))))
      (re-search-forward "^$")
      (let* ((h (line-number-at-pos (1- (point))))
	     (g (make-grid h w))
	     (moves nil))
	(goto-char (point-min))
	(dotimes (i h)
	  (let ((s (buffer-substring (point) (line-end-position))))
	    (dotimes (j w)
	      (gset g i j (aref s j)))
	    (forward-line)))
	(forward-line)
	(let ((start (point)))
	  (condition-case nil
	      (while t
		(goto-char (line-end-position))
		(delete-char 1))
	    (end-of-buffer nil))
	  (setq moves (buffer-substring start (point-max))))
	(cons g moves)))))

(defun make-grid (h w &optional blank)
  (let ((g (make-vector h nil)))
    (dotimes (i h)
      (aset g i (make-vector w (unless blank ?.))))
    g))

(defun grid-height (g)
  (length g))

(defun grid-width (g)
  (length (aref g 0)))

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

(defun puzzle-15a ()
  (let ((file "data/example-15.txt"))
    (read-warehouse file)))

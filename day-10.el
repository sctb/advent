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

(defun grid-like (g)
  (make-grid (grid-height g) (grid-width g)))

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

(defun count-grid (g c)
  (let ((count 0))
    (dotimes (i (grid-height g))
      (dotimes (j (grid-width g))
	(when (eq (gref g i j) c)
	  (setq count (+ count 1)))))
    count))

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

(defun numchar (n)
  (+ n 48))

(defun trailheads (g)
  (let ((heads nil))
    (dotimes (i (grid-height g))
      (dotimes (j (grid-width g))
	(when (eq (gref g i j) (numchar 0))
	  (push (cons i j) heads))))
    heads))

(defun step (c i j g x)
  (when (eq (gref g i j) (+ c 1))
    (walk-trail i j g x)))

(defun walk-trail (i j g x)
  (when-let* ((c (gref g i j)))
    (if (eq c (numchar 9))
	(gset x i j ?X)
      (step c (1+ i) j g x)
      (step c i (1+ j) g x)
      (step c (1- i) j g x)
      (step c i (1- j) g x))))

(defun puzzle-10a ()
  (let* ((g (read-grid "data/input-10.txt"))
	 (heads (trailheads g))
	 (sum 0))
    (dolist (h heads)
      (let ((i (car h))
	    (j (cdr h))
	    (x (grid-like g)))
	(walk-trail i j g x)
	(setq sum (+ sum (count-grid x ?X)))))
    sum))

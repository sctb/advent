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

(defun antennas (g)
  (let ((a (make-hash-table)))
    (dotimes (i (grid-height g))
      (dotimes (j (grid-width g))
	(let ((c (gref g i j)))
	  (unless (eq c ?.)
	    (let ((l (gethash c a)))
	      (push (cons i j) l)
	      (puthash c l a))))))
    a))

(defun combine (l)
  (let ((pairs nil))
    (when (cdr l)
      (let ((a (car l)))
	(dolist (b (cdr l))
	  (push (cons a b) pairs)))
      (when (cddr l)
	(setq pairs (append pairs (combine (cdr l))))))
    pairs))

(defun antenna-pairs (a)
  (let ((pairs nil))
    (maphash (lambda (_ l)
	       (setq pairs (append pairs (combine l))))
	     a)
    pairs))

(defun antipoints (n m)
  (let ((d (- n m)))
    (cons (+ n d) (- m d))))

(defun mark-antinodes (p x)
  (let* ((a (car p))
	 (b (cdr p))
	 (c (antipoints (car a) (car b)))
	 (d (antipoints (cdr a) (cdr b))))
    (gset x (car c) (car d) ?#)
    (gset x (cdr c) (cdr d) ?#)))

(defun count-antinodes (x)
  (let ((count 0))
    (dotimes (i (grid-height x))
      (dotimes (j (grid-width x))
	(when (eq (gref x i j) ?#)
	  (setq count (+ count 1)))))
    count))

(defun puzzle-8a ()
  (let* ((g (read-grid "data/input-8.txt"))
	 (x (make-grid (grid-height g) (grid-width g)))
	 (a (antennas g)))
    (dolist (p (antenna-pairs a))
      (mark-antinodes p x))
    (count-antinodes x)))

(defun antipoints-2 (n m factor)
  (let ((d (- n m)))
    (cons (+ n (* d factor)) (- m (* d factor)))))

(defun mark-antinodes-2 (p x)
  (let* ((a (car p))
	 (b (cdr p)))
    ;; rather than thinking too much about bounds, just keep
    ;; increasing the number of factors until the result converges
    (dotimes (f 60)
      (let ((c (antipoints-2 (car a) (car b) f))
	    (d (antipoints-2 (cdr a) (cdr b) f)))
	(gset x (car c) (car d) ?#)
	(gset x (cdr c) (cdr d) ?#)))))

(defun puzzle-8b ()
  (let* ((g (read-grid "data/input-8.txt"))
	 (x (make-grid (grid-height g) (grid-width g)))
	 (a (antennas g)))
    (dolist (p (antenna-pairs a))
      (mark-antinodes-2 p x))
    (count-antinodes x)))
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

(defun make-grid (h w &optional blank)
  (let ((g (make-vector h nil)))
    (dotimes (i h)
      (aset g i (make-vector w (or blank ?.))))
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

(defun grid-like (g &optional blank)
  (make-grid (grid-height g) (grid-width g) blank))

(defun insert-grid (g)
  (seq-do (lambda (r) (seq-do #'insert r) (insert ?\n)) g))

(defun find-reindeer (g)
  (catch :found
    (dotimes (i (grid-height g))
      (dotimes (j (grid-width g))
	(when (eq (gref g i j) ?S)
	  (throw :found (list :east 0 (cons i j))))))))

(defun find-end (g)
  (catch :found
    (dotimes (i (grid-height g))
      (dotimes (j (grid-width g))
	(when (eq (gref g i j) ?E)
	  (throw :found (cons i j)))))))

(defun score (a b)
  (if (eq a b)
      1
    (let* ((dirs '(:east :north :west :south))
	   (ai (seq-position dirs a))
	   (bi (seq-position dirs b)))
      (pcase (abs (- ai bi))
	(1 1001)
	(2 2001)
	(3 1001)))))

(defun around (deer)
  (pcase-let ((`(_ _ (,i . ,j)) deer))
    (list (cons :east (cons i (1+ j)))
	  (cons :north (cons (1- i) j))
	  (cons :west (cons i (1- j)))
	  (cons :south (cons (1+ i) j)))))

(defun passable (c)
  (and (or (eq c ?.) (eq c ?E)) c))

(defun moves (g deer)
  (let ((moves nil))
    (dolist (a (around deer))
      (pcase-let ((`(,dir ,i . ,j) a))
	(when (passable (gref g i j))
	  (pcase-let* ((`(,dd ,ds) deer))
	    (let ((score (+ ds (score dir dd))))
	      (push (list dir score (cons i j)) moves))))))
    moves))

(defun step (g x move)
  (pcase-let* ((`(_ ,score (,i . ,j)) move))
    (when (< score (gref x i j))
      (gset x i j score)
      (unless (eq (gref g i j) ?E)
	(dolist (move (moves g move))
	  (step g x move))))))

(defun puzzle-16a ()
  ;; Needed to set (setq max-lisp-eval-depth 5000) to avoid a stack
  ;; overflow with the larger puzzle input
  (let* ((g (read-grid "data/input-16.txt"))
	 (x (grid-like g (expt 2 32)))
	 (deer (find-reindeer g))
	 (end (find-end g)))
    (dolist (move (moves g deer))
      (step g x move))
    (gref x (car end) (cdr end))))

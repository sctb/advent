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

(defun insert-grid (g)
  (seq-do (lambda (r) (seq-do #'insert r) (insert ?\n)) g))

(defun find-robot (g)
  (catch :found
    (dotimes (i (grid-height g))
      (dotimes (j (grid-width g))
	(when (eq (gref g i j) ?@)
	  (throw :found (cons i j)))))))

(defun target (i j d)
  (pcase d
    (?^ (cons (1- i) j))
    (?> (cons i (1+ j)))
    (?v (cons (1+ i) j))
    (?< (cons i (1- j)))))

(defun scoot (g i j ti tj)
  (let ((c (gref g i j)))
    (unless (eq (gref g ti tj) ?.)
      (error "Bad scoot"))
    (gset g i j ?.)
    (gset g ti tj c)))

(defun move (g i j d)
  "If it is possible to move in direction `d', do it and return non-NIL,
otherwise do nothing and return NIL"
  (pcase-let* ((`(,ti . ,tj) (target i j d)))
    (when (pcase (gref g ti tj)
	    (?. t)
	    (?O (move g ti tj d)))
      (scoot g i j ti tj))))

(defun gps-sum (g)
  (let ((sum 0))
    (dotimes (i (grid-height g))
      (dotimes (j (grid-width g))
	(when (eq (gref g i j) ?O)
	  ;; "The GPS coordinate of a box is equal to 100 times its
	  ;; distance from the top edge of the map plus its distance
	  ;; from the left edge of the map"
	  (setq sum (+ sum (+ (* 100 i) j))))))
    sum))

(defun puzzle-15a ()
  (let ((file "data/input-15.txt"))
    (pcase-let* ((`(,g . ,moves) (read-warehouse file)))
      (seq-do (lambda (d)
		;; FIXME: silly to lose and find the robot each time
		(pcase-let* ((`(,i . ,j) (find-robot g)))
		  (move g i j d)))
	      moves)
      (gps-sum g))))

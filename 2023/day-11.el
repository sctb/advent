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

(defun empty-rows (grid)
  (let ((rows ()))
    (dotimes (i (grid-height grid))
      (let ((row (aref grid i)))
	(unless (seq-contains-p row ?#)
	  (push i rows))))
    (nreverse rows)))

(defun empty-cols (grid)
  (let ((cols ()))
    (dotimes (j (grid-width grid))
      (catch :full
	(dotimes (i (grid-height grid))
	  (when (eq (gref grid (cons i j)) ?#)
	    (throw :full nil)))
	(push j cols)))
    (nreverse cols)))

(defun expand (grid)
  (let ((galaxies nil)
	(erows (empty-rows grid))
	(ecols (empty-cols grid))
	(i+ 0))
    (dotimes (i (grid-height grid))
      (if (eq i (car erows))
	  (progn
	    (incf i+)
	    (setq erows (cdr erows)))
	(let ((ecols ecols)
	      (j+ 0))
	  (dotimes (j (grid-width grid))
	    (if (eq j (car ecols))
		(progn
		  (incf j+)
		  (setq ecols (cdr ecols)))
	      (when (eq (gref grid (cons i j)) ?#)
		(let ((pos (cons (+ i i+) (+ j j+))))
		  (push pos galaxies))))))))
    (nreverse galaxies)))

(defun pairwise (list)
  (let ((pairs nil))
    (while list
      (let ((x (car list)))
	(dolist (y (cdr list))
	  (push (cons x y) pairs)))
      (setq list (cdr list)))
    (nreverse pairs)))

(defun distance (a b)
  (+ (abs (- (car a) (car b)))
     (abs (- (cdr a) (cdr b)))))

(defun puzzle-11a ()
  (let* ((grid (read-grid "data/input-11.txt"))
	 (galaxies (expand grid))
	 (sum 0))
    (dolist (p (pairwise galaxies))
      (pcase-let ((`(,a . ,b) p))
	(incf sum (distance a b))))
    sum))

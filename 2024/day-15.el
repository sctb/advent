;; -*- lexical-binding: t -*-

(defun make-grid (height width)
  (let ((grid (make-vector height nil)))
    (dotimes (i height)
      (aset grid i (make-vector width ?.)))
    grid))

(defun read-warehouse (file)
  (with-temp-buffer
    (insert-file-contents file)
    (let ((width (1- (line-end-position))))
      (re-search-forward "^$")
      (let* ((height (line-number-at-pos (1- (point))))
	     (grid (make-grid height width))
	     (moves nil))
	(goto-char (point-min))
	(dotimes (i height)
	  (let ((s (buffer-substring (point) (line-end-position))))
	    (dotimes (j width)
	      (gset grid (cons i j) (aref s j)))
	    (forward-line)))
	(forward-line)
	(let ((start (point)))
	  (condition-case nil
	      (while t
		(goto-char (line-end-position))
		(delete-char 1))
	    (end-of-buffer nil))
	  (setq moves (buffer-substring start (point-max))))
	(cons grid moves)))))

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

(defun find-robot (grid)
  (catch :found
    (dotimes (i (grid-height grid))
      (dotimes (j (grid-width grid))
	(let ((pos (cons i j)))
	  (when (eq (gref grid pos) ?@)
	    (throw :found pos)))))))

(defun target (pos d)
  (pcase-let ((`(,i . ,j) pos))
    (pcase d
      (?^ (cons (1- i) j))
      (?> (cons i (1+ j)))
      (?v (cons (1+ i) j))
      (?< (cons i (1- j))))))

(defun scoot (grid a b)
  (let ((c (gref grid a)))
    (unless (eq (gref grid b) ?.)
      (error "Bad scoot"))
    (gset grid a ?.)
    (gset grid b c)
    b))

(defun move (grid pos d)
  "If it is possible to move in direction `d', do it and return non-NIL,
otherwise do nothing and return NIL"
  (let ((target (target pos d)))
    (when (pcase (gref grid target)
	    (?. t)
	    (?O (move grid target d)))
      (scoot grid pos target))))

(defun gps-sum (grid)
  (let ((sum 0))
    (dotimes (i (grid-height grid))
      (dotimes (j (grid-width grid))
	(when (memq (gref grid (cons i j)) '(?O ?\[))
	  ;; "The GPS coordinate of a box is equal to 100 times its
	  ;; distance from the top edge of the map plus its distance
	  ;; from the left edge of the map"
	  (incf sum (+ (* 100 i) j)))))
    sum))

(defun puzzle-15a ()
  (let* ((file "data/input-15.txt")
	 (warehouse (read-warehouse file))
	 (grid (car warehouse))
	 (moves (cdr warehouse))
	 (pos (find-robot grid)))
    (seq-do (lambda (d)
	      (when-let* ((next (move grid pos d)))
		(setq pos next)))
	    moves)
    (gps-sum grid)))

(defun double-tile (c)
  (pcase c
    (?# '(?# ?#))
    (?O '(?\[ ?\]))
    (?. '(?. ?.))
    (?@ '(?@ ?.))))

(defun scale-warehouse (grid)
  (let* ((height (grid-height grid))
	 (width (grid-width grid))
	 (scaled (make-grid height (* width 2))))
    (dotimes (i height)
      (dotimes (j width)
	(let* ((k (- width j 1))
	       (c (gref grid (cons i k)))
	       (m (* k 2))
	       (n (+ m 1)))
	  (pcase-let ((`(,a ,b) (double-tile c)))
	    (gset scaled (cons i m) a)
	    (gset scaled (cons i n) b)))))
    scaled))

(defun other-half (grid pos)
  (pcase-let ((`(,i . ,j) pos))
    (pcase (gref grid pos)
      (?\[ (cons i (1+ j)))
      (?\] (cons i (1- j))))))

(defun movable-p (grid pos d)
  (let ((target (target pos d)))
    (pcase (gref grid target)
      ((or ?\[ ?\])
       (let ((other (other-half grid target)))
	 (pcase d
	   ((or ?< ?>)
	    ;; when moving laterally we only need to check the leading
	    ;; character of the pair
	    (movable-p grid other d))
	   (_ (and (movable-p grid target d)
		   (movable-p grid other d))))))
      (?. t))))

(defun execute-move (grid pos d)
  (let ((target (target pos d)))
    (pcase (gref grid target)
      ((or ?\[ ?\])
       (let ((other (other-half grid target)))
	 ;; move the other pair first so that ‘scoot’ doesn't
	 ;; overwrite it with this one
	 (execute-move grid other d)
	 (execute-move grid target d))))
    (scoot grid pos target)))

(defun puzzle-15b ()
  (let* ((file "data/input-15.txt")
	 (warehouse (read-warehouse file))
	 (grid (car warehouse))
	 (moves (cdr warehouse))
	 (grid (scale-warehouse grid))
	 (pos (find-robot grid)))
    (seq-do (lambda (d)
	      (when (movable-p grid pos d)
		(execute-move grid pos d)
		(setq pos (target pos d))))
	    moves)
    (gps-sum grid)))

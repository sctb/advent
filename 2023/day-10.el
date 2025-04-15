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

(defun find-animal (grid)
  (catch :found
    (dotimes (i (grid-height grid))
      (dotimes (j (grid-width grid))
	(let ((pos (cons i j)))
	  (when (eq (gref grid pos) ?S)
	    (throw :found pos)))))))

(defun pipe-ends (c)
  (pcase c
    (?| '((-1 . 0) . (+1 . 0)))
    (?- '((0 . -1) . (0 . +1)))
    (?L '((-1 . 0) . (0 . +1)))
    (?J '((-1 . 0) . (0 . -1)))
    (?7 '((+1 . 0) . (0 . -1)))
    (?F '((+1 . 0) . (0 . +1)))))

(defun offset-pos (pos offset)
  (pcase-let ((`(,i . ,j) pos)
	      (`(,n . ,m) offset))
    (cons (+ i n) (+ j m))))

(defun around (pos)
  (pcase-let ((`(,i . ,j) pos))
    (list (cons i (1+ j))
	  (cons (1+ i) j)
	  (cons (1- i) j)
	  (cons i (1- j)))))

(defun step (pos grid steps)
  (let* ((c (gref grid pos))
	 (n (gethash pos steps 0))
	 (offsets (pipe-ends c))
	 (a (offset-pos pos (car offsets)))
	 (b (offset-pos pos (cdr offsets)))
	 (next nil))
    (unless (gethash a steps)
      (setq next a))
    (unless (gethash b steps)
      (setq next b))
    (when next
      (puthash next (1+ n) steps)
      next)))

(defun puzzle-10a ()
  (let* ((grid (read-grid "data/example-10.txt"))
	 (start (find-animal grid))
	 (pipes nil)
	 (steps (make-hash-table :test 'equal)))
    (dolist (pos (around start))
      (let ((c (gref grid pos)))
	(when-let* ((offsets (pipe-ends c))
		    (a (offset-pos pos (car offsets)))
		    (b (offset-pos pos (cdr offsets))))
	  ;; look for pipes with openings to the starting position
	  (cond ((equal a start)
		 (push pos pipes))
		((equal b start)
		 (push pos pipes))))))
    (let ((pos (car pipes)))
      (while pos
	(let ((next (step pos grid steps)))
	  (message "%s: %s" pos (gethash pos steps))
	  (setq pos next))))))


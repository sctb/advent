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

(defun around (pos)
  (pcase-let ((`(,i . ,j) pos))
    (list (cons i (1+ j))
	  (cons (1+ i) j)
	  (cons (1- i) j)
	  (cons i (1- j)))))

(defun pipe-ends (pos grid)
  (let ((c (gref grid pos)))
    (pcase-let ((`(,i . ,j) pos))
      (pcase c
	(?| `((,(1- i) . ,j) . (,(1+ i) . ,j)))
	(?- `((,i . ,(1- j)) . (,i . ,(1+ j))))
	(?L `((,(1- i) . ,j) . (,i . ,(1+ j))))
	(?J `((,(1- i) . ,j) . (,i . ,(1- j))))
	(?7 `((,(1+ i) . ,j) . (,i . ,(1- j))))
	(?F `((,(1+ i) . ,j) . (,i . ,(1+ j))))))))

(defun egress (pos grid steps)
  "The position of the unvisited end of the pipe at POS"
  (pcase-let ((`(,a . ,b) (pipe-ends pos grid)))
    (cond ((null (gethash a steps)) a)
	  ((null (gethash b steps)) b))))

(defun step (pos grid steps)
  (when-let* ((count (1+ (gethash pos steps)))
	      (next (egress pos grid steps)))
    (puthash next count steps)
    next))

(defun connectedp (pos to grid)
  (pcase-let ((`(,a . ,b) (pipe-ends pos grid)))
    (or (equal to a) (equal to b))))

(defun puzzle-10a ()
  (let* ((grid (read-grid "data/input-10.txt"))
	 (start (find-animal grid))
	 (steps (make-hash-table :test 'equal))
	 (fronts nil)
	 (max 0))
    ;; need to bootstrap the stepping mechanism as the pipe under the
    ;; starting point is not known
    (puthash start 0 steps)
    (dolist (pos (around start))
      (when (connectedp pos start grid)
	(puthash pos 1 steps)
	(push pos fronts)))
    ;; now proceed normally...
    (while fronts
      (let ((next nil))
	(dolist (pos fronts)
	  (when-let* ((pos (step pos grid steps))
		      (count (gethash pos steps)))
	    (push pos next)
	    (setq max (max max count))))
	(setq fronts next)))
    max))

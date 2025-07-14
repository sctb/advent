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

(defun make-grid (height width)
  (let ((grid (make-vector height nil)))
    (dotimes (i height)
      (aset grid i (make-vector width ?.)))
    grid))

(defun grid-like (grid)
  (make-grid (grid-height grid) (grid-width grid)))

(defun copy-grid (grid)
  (let* ((height (grid-height grid))
	 (new (make-vector height nil)))
    (dotimes (i height)
      (aset new i (copy-sequence (aref grid i))))
    new))

(defun deltas (dir)
  (pcase dir
    (:up    '(-1 .  0))
    (:right '( 0 .  1))
    (:down  '( 1 .  0))
    (:left  '( 0 . -1))))

(defun turn-right (dir)
  (pcase dir
    (:up    :right)
    (:right :down)
    (:down  :left)
    (:left  :up)))

(defun guard-glyph (dir)
  (pcase dir
    (:up    ?∧)
    (:right ?>)
    (:down  ?∨)
    (:left  ?<)))

(defun find-guard (grid)
  (catch :found
    (dotimes (i (grid-height grid))
      (dotimes (j (grid-width grid))
	(let ((pos (cons i j)))
	  (when (eq (gref grid pos) ?^)
	    (throw :found (cons :up pos))))))))

(defun mark-guard (grid guard &optional marker)
  (pcase-let ((`(,dir . ,pos) guard))
    (let ((c (or marker (guard-glyph dir))))
      (gset grid pos c))))

(defun look-ahead (grid guard)
  (pcase-let ((`(,dir . (,i . ,j)) guard))
    (let ((d (deltas dir)))
      (setq i (+ i (car d)))
      (setq j (+ j (cdr d))))
    (gref grid (cons i j))))

(defun step-forward (guard)
  (pcase-let ((`(,dir . (,i . ,j)) guard))
    (let ((d (deltas dir)))
      (cons dir (cons (+ i (car d)) (+ j (cdr d)))))))

(defun next-step (grid guard)
  (pcase-let ((`(,dir . ,pos) guard))
    (let ((c (look-ahead grid guard)))
      (cond ((eq c ?#)
	     (cons (turn-right dir) pos))
	    ((eq c (guard-glyph dir)) :stuck)
	    (c (step-forward guard))))))

(defun watch-guard (grid guard)
  "Animation for debugging purposes"
  (pop-to-buffer "*grid*")
  (while guard
    (erase-buffer)
    (insert-grid grid)
    (mark-guard grid guard ?X)
    (setq guard (next-step grid guard))
    (when guard
      (mark-guard grid guard))
    (sit-for 0.5)))

(defun trace-guard (grid guard)
  (while guard
    (mark-guard grid guard ?X)
    (setq guard (next-step grid guard))
    (when guard
      (mark-guard grid guard ?X))))

(defun count-positions (grid)
  (let ((count 0))
    (dotimes (i (grid-height grid))
      (dotimes (j (grid-width grid))
	(when (eq (gref grid (cons i j)) ?X)
	  (incf count))))
    count))

(defun puzzle-6a ()
  ;; example-6.txt: 41
  ;; input-6.txt: 4826
  (let* ((grid (read-grid "data/input-6.txt"))
	 (guard (find-guard grid)))
    (trace-guard grid guard)
    (count-positions grid)))

(defun reset-grid (a b)
  (dotimes (i (grid-height a))
    (aset b i (copy-sequence (aref a i)))))

(defun guard-stuck (g guard)
  (let ((stuck nil)
	;; When the guard is stuck moving back-and-forth along the
	;; same row or column, ‘next-step’ will not recognize previous
	;; positions because they will always be in the opposite
	;; direction.  In these few cases, bail out after a large
	;; number of iterations
	(limit 10000))
    (while (and guard (> limit 0))
      (mark-guard g guard)
      (setq guard (next-step g guard))
      (cond ((consp guard)
	     (mark-guard g guard))
	    ((eq guard :stuck)
	     (setq stuck t)
	     (setq guard nil)))
      (setq limit (- limit 1)))
    (or stuck (<= limit 0))))

(defun obstructions (grid x guard)
  (let* ((w (copy-grid grid))
	 (count 0)
	 (n 0)
	 (total (count-positions x))
	 (p (make-progress-reporter "Finding obstructions" 0 total)))
    (dotimes (i (grid-height w))
      (dotimes (j (grid-width w))
	(let ((pos (cons i j)))
	  (when (eq (gref x pos) ?X)
	    ;; reset our working grid for a new trace
	    (reset-grid grid w)
	    ;; install the new obstacle
	    (gset w pos ?#)
	    ;; see if the guard becomes stuck
	    (when (guard-stuck w guard)
	      (incf count))
	    (progress-reporter-update p n)
	    (incf n)))))
    (progress-reporter-done p)
    count))

(defun puzzle-6b ()
  ;; example-6.txt: 6
  ;; input-6.txt: 1721
  (let* ((g (read-grid "data/input-6.txt"))
	 (x (copy-grid g))
	 (guard (find-guard g)))
    ;; trace the guard's path to determine possible positions to
    ;; obstruct (although obstructions cause new pathways, any
    ;; potential obstruction must be along the original path
    (trace-guard x guard)
    ;; restore the original guard ^ because "The new obstruction can't
    ;; be placed at the guard's starting position..."
    (mark-guard x guard)
    (obstructions g x guard)))

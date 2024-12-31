;; -*- lexical-binding: t -*-

(defun read-grid (filename)
  (with-temp-buffer
    (insert-file-contents filename)
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

(defun grid-height (g)
  (length g))

(defun grid-width (g)
  (length (aref g 0)))

(defun insert-grid (g)
  (seq-do (lambda (r) (seq-do #'insert r) (insert ?\n)) g))

(defun gset (g i j v)
  (let ((r (aref g i)))
    (aset r j v)))

(defun gref (g i j)
  "Returns nil for out-of-bounds references"
  (when (and (>= i 0) (< i (length g)))
    (let ((r (aref g i)))
      (when (and (>= j 0) (< j (length r)))
	(aref r j)))))

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

(defun find-guard (g)
  (catch :found
    (dotimes (i (grid-height g))
      (dotimes (j (grid-width g))
	(when (eq (gref g i j) ?^)
	  (throw :found (cons :up (cons i j))))))))

(defun mark-guard (g guard &optional marker)
  (pcase-let ((`(,dir . (,i . ,j)) guard))
    (let ((c (or marker (guard-glyph dir))))
      (gset g i j c))))

(defun look-ahead (g guard)
  (pcase-let ((`(,dir . (,i . ,j)) guard))
    (let ((d (deltas dir)))
      (setq i (+ i (car d)))
      (setq j (+ j (cdr d))))
    (gref g i j)))

(defun step-forward (guard)
  (pcase-let ((`(,dir . (,i . ,j)) guard))
    (let ((d (deltas dir)))
      (cons dir (cons (+ i (car d)) (+ j (cdr d)))))))

(defun next-step (g guard)
  (pcase-let ((`(,dir . (,i . ,j)) guard))
    (let ((c (look-ahead g guard)))
      (cond ((eq c ?#)
	     (cons (turn-right dir) (cons i j)))
	    (c (step-forward guard))))))

(defun watch-guard (g guard)
  "Animation for debugging purposes"
  (pop-to-buffer "*grid*")
  (while guard
    (erase-buffer)
    (insert-grid g)
    (mark-guard g guard ?X)
    (setq guard (next-step g guard))
    (when guard
      (mark-guard g guard))
    (sit-for 0.5)))

(defun trace-guard (g guard)
  (while guard
    (mark-guard g guard ?X)
    (setq guard (next-step g guard))
    (when guard
      (mark-guard g guard ?X))))

(defun count-positions (g)
  (let ((count 0))
    (dotimes (i (grid-height g))
      (dotimes (j (grid-width g))
	(when (eq (gref g i j) ?X)
	  (setq count (+ count 1)))))
    count))

(defun puzzle-6a ()
  (let* ((g (read-grid "data/input-6.txt"))
	 (w (grid-width g))
	 (h (grid-height g))
	 (guard (find-guard g)))
    (trace-guard g guard)
    (count-positions g)))

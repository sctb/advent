;; -*- lexical-binding: t -*-

(defun read-line ()
  (buffer-substring (point) (line-end-position)))

(defun read-list (string)
  (read (concat "(" string ")")))

(defun read-races (file)
  (let ((races nil))
    (with-temp-buffer
      (insert-file-contents file)
      (let ((times (cdr (read-list (read-line)))))
	(forward-line)
	(let ((distances (cdr (read-list (read-line)))))
	  (while-let ((time (car times))
		      (distance (car distances)))
	    (push (cons time distance) races)
	    (setq times (cdr times))
	    (setq distances (cdr distances))))))
    (nreverse races)))

(defun beat-race (race)
  (let ((time (car race))
	(distance (cdr race))
	(n 0))
    (dotimes (i time)
      (let ((charge i)
	    (run (- time i)))
	(when (> (* charge run) distance)
	  (setq n (1+ n)))))
    n))

(defun puzzle-6a ()
  (let ((races (read-races "data/input-6.txt")))
    (seq-reduce #'* (mapcar #'beat-race races) 1)))

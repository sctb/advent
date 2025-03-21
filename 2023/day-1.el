;; -*- lexical-binding: t -*-

(defun read-line ()
  (buffer-substring (point) (line-end-position)))

(defun read-lines (file)
  (let ((lines nil))
    (with-temp-buffer
      (insert-file-contents file)
      (while (not (eobp))
	(push (read-line) lines)
	(forward-line)))
    (nreverse lines)))

(defvar single-digits
  '(?1 ?2 ?3 ?4 ?5 ?6 ?7 ?8 ?9))

(defun value-1 (line)
  (let ((first nil)
	(last nil))
    (seq-do (lambda (c)
	      (when (memq c single-digits)
		(setq last c)
		(when (null first)
		  (setq first c))))
	    line)
    (read (string first last))))

(defun puzzle-1a ()
  (let ((lines (read-lines "data/input-1.txt"))
	(sum 0))
    (dolist (line lines)
      (setq sum (+ sum (value-1 line))))
    sum))

(defvar spelled-digits
  '(("one"   . ?1)
    ("two"   . ?2)
    ("three" . ?3)
    ("four"  . ?4)
    ("five"  . ?5)
    ("six"   . ?6)
    ("seven" . ?7)
    ("eight" . ?8)
    ("nine"  . ?9)))

(defun spelled-digit (string)
  (catch :found
    (dolist (digit spelled-digits)
      (when (string-prefix-p (car digit) string)
	(throw :found (cdr digit))))))

(defun value-2 (line)
  (let ((first nil)
	(last nil))
    (dotimes (i (length line))
      (let ((c (elt line i)))
	(when (or (memq c single-digits)
		  (setq c (spelled-digit (substring line i))))
	  (setq last c)
	  (when (null first)
	    (setq first c)))))
    (read (string first last))))

(defun puzzle-1b ()
  (let ((lines (read-lines "data/input-1.txt"))
	(sum 0))
    (dolist (line lines)
      (setq sum (+ sum (value-2 line))))
    sum))

;; -*- lexical-binding: t -*-

(defvar card-rx "Card[ ]+[0-9]+: \\([ 0-9]+\\) | \\([ 0-9]+\\)")

(defun read-list (string)
  (read (concat "(" string ")")))

(defun read-cards (file)
  (let ((cards nil))
    (with-temp-buffer
      (insert-file-contents file)
      (while (re-search-forward card-rx nil t)
	(let ((win (read-list (match-string 1)))
	      (got (read-list (match-string 2))))
	  (push (cons win got) cards))))
    (nreverse cards)))

(defun puzzle-4a ()
  (let ((cards (read-cards "data/input-4.txt"))
	(total 0))
    (dolist (card cards)
      (let* ((win (car card))
	     (got (cdr card))
	     (n (length (seq-intersection win got))))
	(when (> n 0)
	  (let ((points (expt 2 (1- n))))
	    (setq total (+ total points))))))
    total))

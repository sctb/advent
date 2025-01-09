;; -*- lexical-binding: t -*-

(defvar a-rx "Button A: X\\+\\([0-9]\\{2\\}\\), Y\\+\\([0-9]\\{2\\}\\)")
(defvar b-rx "Button B: X\\+\\([0-9]\\{2\\}\\), Y\\+\\([0-9]\\{2\\}\\)")
(defvar p-rx "Prize: X=\\([0-9]\\{1,5\\}\\), Y=\\([0-9]\\{1,5\\}\\)")

(defun match-pair ()
  (cons (read (match-string 1))
	(read (match-string 2))))

(defun read-machines (file)
  (with-temp-buffer
    (insert-file-contents file)
    (let ((machines nil))
      (while (re-search-forward a-rx nil t)
	(let ((m nil))
	  (push (cons 'a (match-pair)) m)
	  (re-search-forward b-rx)
	  (push (cons 'b (match-pair)) m)
	  (re-search-forward p-rx)
	  (push (cons 'p (match-pair)) m)
	  (forward-line)
	  (push m machines)))
      (nreverse machines))))

(defun puzzle-13a ()
  (read-machines "data/example-13.txt"))

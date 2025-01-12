;; -*- lexical-binding: t -*-

(defvar p-rx "p=\\([0-9]\\{1,3\\}\\),\\([0-9]\\{1,3\\}\\)")
(defvar v-rx "v=\\(-?[0-9]\\{1,3\\}\\),\\(-?[0-9]\\{1,3\\}\\)")

(defun match-pair ()
  (cons (read (match-string 1))
	(read (match-string 2))))

(defun read-robots (file)
  (with-temp-buffer
    (insert-file-contents file)
    (let ((robots nil))
      (while (re-search-forward p-rx nil t)
	(let ((p (match-pair)))
	  (re-search-forward v-rx)
	  (push (cons p (match-pair)) robots)))
      (nreverse robots))))

(defun puzzle-14a ()
  (let ((robots (read-robots "data/example-14.txt")))
    robots))


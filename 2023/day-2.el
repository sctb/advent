;; -*- lexical-binding: t -*-

(defvar game-rx "Game \\([0-9]+\\): ")

(defun read-line ()
  (buffer-substring (point) (line-end-position)))

(defun read-games (file)
  (let ((games nil))
    (with-temp-buffer
      (insert-file-contents file)
      (while (re-search-forward game-rx nil t)
	(let ((id (read (match-string 1))))
	  (push (cons id (read-line)) games))))
    (nreverse games)))

(defun puzzle-2a ()
  (read-games "data/example-2.txt"))

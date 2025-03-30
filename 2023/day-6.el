;; -*- lexical-binding: t -*-

(defun read-line ()
  (buffer-substring (point) (line-end-position)))

(defun read-list (string)
  (read (concat "(" string ")")))

(defun read-sheet (file)
  (with-temp-buffer
    (insert-file-contents file)
    (let ((time (cdr (read-list (read-line)))))
      (forward-line)
      (let ((distance (cdr (read-list (read-line)))))
	(cons time distance)))))

(defun puzzle-6a ()
  (read-sheet "data/example-6.txt"))

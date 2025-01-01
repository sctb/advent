;; -*- lexical-binding: t -*-

(defun sread (string)
  (car (read-from-string string)))

(defun read-equations (file)
  (let ((equations nil))
    (with-temp-buffer
      (insert-file-contents file)
      (while (not (eobp))
	(let* ((line (buffer-substring (point) (line-end-position)))
	       (split (split-string line ":"))
	       (value (sread (car split)))
	       (numbers (sread (format "(%s)" (cadr split)))))
	  (push (cons value numbers) equations))
	(forward-line)))
    (nreverse equations)))

(defun max-size (equations)
  (let ((max 0))
    (dolist (e equations)
      (let ((n (length (cdr e))))
	(setq max (max max n))))
    max))

(defun puzzle-7a ()
  (let ((equations (read-equations "data/example-7.txt")))
    (max-size equations)))

;; -*- lexical-binding: t -*-

(defun read-line ()
  (buffer-substring (point) (line-end-position)))

(defun read-hands (file)
  (with-temp-buffer
    (insert-file-contents file)
    (let ((hands nil))
      (while (not (eobp))
	(let* ((line (read-line))
	       (split (split-string line))
	       (hand (cons (car split) (read (cadr split)))))
	  (push hand hands))
	(forward-line))
      (nreverse hands))))

(defun puzzle-7a ()
  (read-hands "data/example-7.txt"))

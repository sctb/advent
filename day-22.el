;; -*- lexical-binding: t -*-

(defun read-line ()
  (buffer-substring (point) (line-end-position)))

(defun read-secrets (file)
  (let ((secrets nil))
    (with-temp-buffer
      (insert-file-contents file)
      (while (not (eobp))
	(push (read (read-line)) secrets)
	(forward-line)))
    (nreverse secrets)))

(defun puzzle-22a ()
  (let ((file "data/example-22.txt"))
    (read-secrets file)))

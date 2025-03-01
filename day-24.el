;; -*- lexical-binding: t -*-

(defun read-line ()
  (buffer-substring (point) (line-end-position)))

(defvar wire-rx "\\(.*\\): \\([01]\\)")
(defvar gate-rx "\\(.*\\) \\([ANDXOR]+\\) \\(.*\\) -> \\(.*\\)")

(defun read-wires ()
  (let ((wires nil))
    (while (re-search-forward wire-rx nil t)
      (let ((w (match-string 1))
	    (v (match-string 2)))
	(push (cons w (read v)) wires)))
    (nreverse wires)))

(defun read-gates ()
  (let ((gates nil))
    (while (re-search-forward gate-rx nil t)
      (let ((i1 (match-string 1))
	    (i2 (match-string 3))
	    (g (read (match-string 2)))
	    (o (match-string 4)))
	(push (list g i1 i2 o) gates)))
    (nreverse gates)))

(defun read-device (file)
  (with-temp-buffer
    (insert-file-contents file)
    (cons (read-wires) (read-gates))))

(defun puzzle-24a ()
  (let* ((file "data/example-24.txt")
	 (device (read-device file))
	 (wires (car device))
	 (gates (cdr device)))
    (list wires gates)))

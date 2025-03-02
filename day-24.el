;; -*- lexical-binding: t -*-

(defvar wire-rx "\\(.*\\): \\([01]\\)")
(defvar gate-rx "\\(.*\\) \\([ANDXOR]+\\) \\(.*\\) -> \\(.*\\)")

(defun read-wires ()
  (let ((wires (make-hash-table :test 'equal)))
    (while (re-search-forward wire-rx nil t)
      (let ((w (match-string 1))
	    (v (read (match-string 2))))
	(puthash w v wires)))
    wires))

(defun read-gates ()
  (let ((gates (make-hash-table :test 'equal)))
    (while (re-search-forward gate-rx nil t)
      (let ((in1 (match-string 1))
	    (in2 (match-string 3))
	    (op (read (match-string 2)))
	    (out (match-string 4)))
	(puthash out (list op in1 in2) gates)))
    gates))

(defun read-device (file)
  (with-temp-buffer
    (insert-file-contents file)
    (cons (read-wires) (read-gates))))

(defun puzzle-24a ()
  (let* ((file "data/example-24.txt")
	 (device (read-device file))
	 (wires (car device))
	 (gates (cdr device)))
    (hash-table-count gates)))

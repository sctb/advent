;; -*- lexical-binding: t -*-

(defvar wire-rx "\\(.*\\): \\([01]\\)")
(defvar gate-rx "\\(.*\\) \\([ANDXOR]+\\) \\(.*\\) -> \\(.*\\)")

(defun read-device (file)
  (with-temp-buffer
    (insert-file-contents file)
    (let ((wires (make-hash-table :test 'equal)))
      (while (re-search-forward wire-rx nil t)
	(let ((wire (match-string 1))
	      (value (read (match-string 2))))
	  (puthash wire (list 'VAL value) wires)))
      (while (re-search-forward gate-rx nil t)
	(let ((in1 (match-string 1))
	      (in2 (match-string 3))
	      (op (read (match-string 2)))
	      (out (match-string 4)))
	  (puthash out (list op in1 in2) wires)))
      wires)))

(defun decimal (wire bit)
  (let ((shift (read (substring wire 1))))
    (ash bit shift)))

(defun input (wire wires)
  (operate (gethash wire wires) wires))

(defun operate (gate wires)
  (if (eq (car gate) 'VAL)
      (cadr gate)
    (pcase-let ((`(,op ,in1 ,in2) gate))
      (let ((a (input in1 wires))
	    (b (input in2 wires)))
	(pcase op
	  ('AND (if (and (eq a 1) (eq b 1)) 1 0))
	  ('XOR (if (not (eq a b)) 1 0))
	  ('OR  (if (or (eq a 1) (eq b 1)) 1 0)))))))

(defun puzzle-24a ()
  (let* ((file "data/example-24.txt")
	 (wires (read-device file)))
    (maphash (lambda (wire gate)
	       (when (eq (elt wire 0) ?z)
		 (message "%s: %s" wire (operate gate wires))))
	     wires)))

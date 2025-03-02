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

(defun wire-pos (wire)
  (read (substring wire 1)))

(defun decimal (wire bit)
  (let ((shift (wire-pos wire)))
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
  (let* ((file "data/input-24.txt")
	 (wires (read-device file))
	 (output 0))
    (maphash (lambda (wire gate)
	       (when (eq (elt wire 0) ?z)
		 (let* ((bit (operate gate wires))
			(n (decimal wire bit)))
		   (setq output (+ output n)))))
	     wires)
    output))

(defun width (wires)
  (let ((max 0))
    (maphash (lambda (wire _)
	       (when (eq (elt wire 0) ?z)
		 (let ((n (wire-pos wire)))
		   (setq max (max n max)))))
	     wires)
    (1+ max)))

(defun wire-name (prefix n)
  (concat prefix (format "%02d" n)))

(defun adder (x y c)
  ;; X Y C ⇒ S C
  ;; ------------
  ;; 0 0 0    0 0
  ;; 0 0 1    1 0
  ;; 0 1 0    1 0
  ;; 1 0 0    1 0
  ;; 0 1 1    0 1
  ;; 1 0 1    0 1
  ;; 1 1 0    0 1
  ;; 1 1 1    1 1
  (pcase (+ x y c)
    (0 (cons 0 0))
    (1 (cons 1 0))
    (2 (cons 0 1))
    (3 (cons 1 1))))

(defun inputs (wire wires)
  (let ((inputs nil)
	(gate (gethash wire wires)))
    (unless (eq (car gate) 'VAL)
      (dolist (in (cdr gate))
	(push in inputs)
	(dolist (in (inputs in wires))
	  (push in inputs))))
    inputs))

(defun puzzle-24b ()
  (let* ((file "data/input-24.txt")
	 (wires (read-device file))
	 (width (width wires))
	 (carry 0)
	 (suspect nil))
    (dotimes (n (1- width))
      (let* ((x (input (wire-name "x" n) wires))
	     (y (input (wire-name "y" n) wires))
	     (wire (wire-name "z" n))
	     (z (input wire wires)))
	(pcase-let ((`(,s . ,c) (adder x y carry)))
	  (unless (eq z s)
	    ;; TODO: install something in ‘suspect’, check if there is
	    ;; a common ancestor to previous failed gates
	    (message "%s ⇒ %s" wire (inputs wire wires))
	    (push wire suspect))
	  (setq carry c))))))

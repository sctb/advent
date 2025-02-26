;; -*- lexical-binding: t -*-

(defun read-line ()
  (buffer-substring (point) (line-end-position)))

(defun read-codes (file)
  (let ((codes nil))
    (with-temp-buffer
      (insert-file-contents file)
      (while (not (eobp))
	(push (read-line) codes)
	(forward-line)))
    (nreverse codes)))

(defun code-number (code)
  (read (substring code 0 -1)))

(defun numpad (key)
  (pcase key
    (?7 '(0 . 0))
    (?8 '(0 . 1))
    (?9 '(0 . 2))
    (?4 '(1 . 0))
    (?5 '(1 . 1))
    (?6 '(1 . 2))
    (?1 '(2 . 0))
    (?2 '(2 . 1))
    (?3 '(2 . 2))
    (?G '(3 . 0)) ; gap
    (?0 '(3 . 1))
    (?A '(3 . 2))))

(defun dirpad (key)
  (pcase key
    (?G '(0 . 0)) ; gap
    (?^ '(0 . 1))
    (?A '(0 . 2))
    (?< '(1 . 0))
    (?v '(1 . 1))
    (?> '(1 . 2))))

(defun heading (from to)
  (cond ((eq from to) 0)
	((< from to) 1)
	(t -1)))

(defun presses (from to avoid)
  "Returns a list of possible directional keypress sequences that would
move from ‘from’ to ‘to’, avoiding the blank space indicated by ‘avoid’."
  (if (equal from to)
      (list (list ?A))
    (pcase-let ((`(,y . ,x) from))
      (let* ((dy (heading y (car to)))
	     (dx (heading x (cdr to)))
	     (seq nil)
	     (a (cons (+ dy y) x))
	     (b (cons y (+ dx x))))
	(unless (or (equal a from) (equal a avoid))
	  (dolist (rest (presses a to avoid))
	    (push (concat (if (> dy 0) '(?v) '(?^)) rest) seq)))
	(unless (or (equal b from) (equal b avoid))
	  (dolist (rest (presses b to avoid))
	    (push (concat (if (> dx 0) '(?>) '(?<)) rest) seq)))
	seq))))

(defun press (seq pad)
  (let ((seqs nil)
	;; convert to list, start from initial position A
	(seq (cons ?A (append seq nil))))
    (while-let ((a (car seq))
		(b (cadr seq)))
      (let* ((from (funcall pad a))
	     (to (funcall pad b))
	     (avoid (funcall pad ?G))
	     (presses (presses from to avoid)))
	(push presses seqs))
      (setq seq (cdr seq)))
    seqs))

(defun know (seq times memo)
  (gethash (vector seq times) memo))

(defun save (seq times n memo)
  (puthash (vector seq times) n memo))

(defun encoded (seq times memo)
  "Returns the least number of directional keypresses needed to encode
‘seq’ through a number of indirect keypads denoted by ‘times’."
  (if (eq times 0)
      (length seq)
    (or (know seq times memo)
	(let ((sum 0))
	  (dolist (options (press seq #'dirpad))
	    (let ((len nil))
	      (dolist (seq options)
		(let ((n (encoded seq (1- times) memo)))
		  (when (or (null len) (< n len))
		    (setq len n))))
	      (setq sum (+ sum len))))
	  (save seq times sum memo)))))

(defun code (code times)
  (let ((memo (make-hash-table :test 'equal))
	(sum 0))
    (dolist (options (press code #'numpad))
      (let ((len nil))
	(dolist (seq options)
	  (let ((n (encoded seq times memo)))
	    (when (or (null len) (< n len))
	      (setq len n))))
	(setq sum (+ sum len))))
    sum))

(defun puzzle-21a ()
  (let ((codes (read-codes "data/input-21.txt"))
	(sum 0))
    (dolist (code codes)
      (let* ((len (code code 2))
	     (n (code-number code))
	     (complexity (* n len)))
	(setq sum (+ sum complexity))))
    sum))

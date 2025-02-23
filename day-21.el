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
  (if (equal from to)
      (list (list ?A))
    (pcase-let ((`(,y . ,x) from))
      (let* ((dy (heading y (car to)))
	     (dx (heading x (cdr to)))
	     (paths nil)
	     (a (cons (+ dy y) x))
	     (b (cons y (+ dx x))))
	(unless (or (equal a from) (equal a avoid))
	  (dolist (path (presses a to avoid))
	    (push (cons (if (> dy 0) ?v ?^) path) paths)))
	(unless (or (equal b from) (equal b avoid))
	  (dolist (path (presses b to avoid))
	    (push (cons (if (> dx 0) ?> ?<) path) paths)))
	paths))))

(defun numpress (a b)
  (presses (numpad a) (numpad b) (numpad ?G)))

(defun dirpress (a b)
  (presses (dirpad a) (dirpad b) (dirpad ?G)))

(defun puzzle-21a ()
  (let ((codes (read-codes "data/example-21.txt")))
    codes))

;; -*- lexical-binding: t -*-

(defun sread (string)
  (car (read-from-string string)))

(defvar rule-rx "\\([0-9]\\{2\\}\\)|\\([0-9]\\{2\\}\\)$")
(defvar update-rx "\\([0-9]\\{2\\},?\\)+")

(defun read-print-queue (filename)
  (let ((rules nil)
	(updates nil))
    (with-temp-buffer
      (insert-file-contents filename)
      (while (re-search-forward rule-rx nil t)
	(let ((p1 (sread (match-string 1)))
	      (p2 (sread (match-string 2))))
	  (push (cons p1 p2) rules)))
      (re-search-forward "^$")
      (forward-line)
      (while (looking-at update-rx)
	(let* ((line (buffer-substring (point) (line-end-position)))
	       (split (split-string line ","))
	       (update (mapcar #'sread split)))
	  (push update updates))
	(forward-line)))
    (cons (nreverse rules) (nreverse updates))))

(defun middle (pages)
  (let* ((l (length pages))
	 (n (floor l 2)))
    (nth n pages)))

(defun priors (p rules)
  (let ((rules (seq-filter (lambda (r) (eq p (cdr r))) rules)))
    (mapcar #'car rules)))

(defun check-update (u rules)
  (catch :disordered
    (let ((nope (make-hash-table)))
      (dolist (p u)
	(when (gethash p nope)
	  (throw :disordered nil))
	;; cannot print any pages which must come before
	(dolist (r (priors p rules))
	  (puthash r t nope)))
      (middle u))))

(defun puzzle-5a ()
  (let* ((queue (read-print-queue "data/input-5.txt"))
	 (rules (car queue))
	 (updates (cdr queue))
	 (sum 0))
    (dolist (u updates)
      (when-let* ((n (check-update u rules)))
	(setq sum (+ sum n))))
    sum))

(defun ruled-by (u rules)
  (seq-filter (lambda (r)
		(and (memq (car r) u)
		     (memq (cdr r) u)))
	      rules))


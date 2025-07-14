;; -*- lexical-binding: t -*-

(defun read-line ()
  (buffer-substring (point) (line-end-position)))

(defun read-reports (file)
  (let ((lines nil))
    (with-temp-buffer
      (insert-file-contents file)
      (while (not (eobp))
	(let* ((text (read-line))
	       (line (car (read-from-string (format "(%s)" text)))))
	  (push line lines))
	(forward-line)))
    (nreverse lines)))

(defun safe-delta (a b)
  (let ((d (abs (- a b))))
    (and (>= d 1)
	 (<= d 3))))

(defun safe-report (report &optional sign)
  (let ((a (car report))
	(b (cadr report))
	(safe t))
    (while b
      (if (safe-delta a b)
	  (cond ((null sign)
		 ;; establish the expected sign
		 (setq sign (- a b)))
		((not (eq (> sign 0)
			  (> (- a b) 0)))
		 ;; levels must be all increasing or decreasing
		 (setq b nil)))
	(setq b nil))
      (if (null b)
	  (setq safe nil)
	(setq report (cdr report))
	(setq a (car report))
	(setq b (cadr report))))
    safe))

(defun puzzle-2a ()
  ;; example-2.txt: 2
  ;; input-2.txt: 236
  (let ((count 0))
    (dolist (report (read-reports "data/input-2.txt"))
      (when (safe-report report)
	(incf count)))
    count))

(defun remove-nth (n seq)
  (append (seq-take seq n) (seq-drop seq (1+ n))))

(defun dampener (report)
  (let ((dampened nil))
    (dotimes (i (length report))
      (push (remove-nth i report) dampened))
    dampened))

(defun safe-report-2 (report)
  (or (safe-report report)
      (seq-some #'safe-report (dampener report))))

(defun puzzle-2b ()
  ;; example-2.txt: 4
  ;; input-2.txt: 308
  (let ((count 0))
    (dolist (report (read-reports "data/input-2.txt"))
      (when (safe-report-2 report)
	(incf count)))
    count))

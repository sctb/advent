;; -*- lexical-binding: t -*-

(defun read-lines (filename)
  (let ((lines nil))
    (with-temp-buffer
      (insert-file-contents filename)
      (while (not (eobp))
	(let ((beg (point)))
	  (move-end-of-line nil)
	  (let* ((text (buffer-substring beg (point)))
		 (line (car (read-from-string (format "(%s)" text)))))
	    (push line lines))
	  (forward-char))))
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
  (let ((count 0))
    (dolist (report (read-lines "data/input-2.txt"))
      (when (safe-report report)
	(setq count (1+ count))))
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
  (let ((count 0))
    (dolist (report (read-lines "data/input-2.txt"))
      (when (safe-report-2 report)
	(setq count (1+ count))))
    count))

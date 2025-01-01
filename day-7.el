;; -*- lexical-binding: t -*-

(defun sread (string)
  (car (read-from-string string)))

(defun read-equations (file)
  (let ((equations nil))
    (with-temp-buffer
      (insert-file-contents file)
      (while (not (eobp))
	(let* ((line (buffer-substring (point) (line-end-position)))
	       (split (split-string line ":"))
	       (value (sread (car split)))
	       (numbers (sread (format "(%s)" (cadr split)))))
	  (push (cons value numbers) equations))
	(forward-line)))
    (nreverse equations)))

(defun max-size (equations)
  (let ((max 0))
    (dolist (e equations)
      (let ((n (length (cdr e))))
	(setq max (max max n))))
    max))

(defun wholep (n)
  (zerop (mod n 1)))

(defun run-search (value numbers)
  (if-let* ((rest (cdr numbers)))
      (let* ((n (car numbers))
	     (delta (- value n))
	     (factor (/ (float value) n)))
	(when (> delta 0)
	  (run-search delta rest))
	(when (and (>= factor 1) (wholep factor))
	  (run-search (truncate factor) rest)))
    (when (= value (car numbers))
      (throw :true t))))

(defun test-equation (e)
  (let ((value (car e))
	;; this approach inverts the operators to recursively search
	;; for matches, so we also need to reverse the application
	;; order
	(numbers (reverse (cdr e))))
    (when (catch :true (run-search value numbers))
      value)))

(defun puzzle-7a ()
  (let ((equations (read-equations "data/input-7.txt"))
	(sum 0))
    (dolist (e equations)
      (when-let* ((value (test-equation e)))
	(setq sum (+ sum value))))
    sum))

;; -*- lexical-binding: t -*-

(defun read-equations (file)
  (let ((equations nil))
    (with-temp-buffer
      (insert-file-contents file)
      (while (not (eobp))
	(let* ((line (buffer-substring (point) (line-end-position)))
	       (split (split-string line ":"))
	       (value (read (car split)))
	       (numbers (read (format "(%s)" (cadr split)))))
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

(defun run-search-2 (value numbers)
  (if-let* ((rest (cdr numbers)))
      (let* ((n (car numbers))
	     (delta (- value n))
	     (factor (/ (float value) n)))
	(when (> delta 0)
	  (run-search-2 delta rest))
	(when (and (>= factor 1) (wholep factor))
	  (run-search-2 (truncate factor) rest))
	(let ((a (number-to-string value))
	      (b (number-to-string n)))
	  ;; to invert the concatenation operator we strip the value
	  ;; of its suffix, if possible
	  (when (string-suffix-p b a)
	    (let* ((prefix (substring a 0 (- (length b))))
		   (value (string-to-number prefix)))
	      (run-search-2 value rest)))))
    (when (= value (car numbers))
      (throw :true t))))

(defun test-equation-2 (e)
  (let ((value (car e))
	;; this approach inverts the operators to recursively search
	;; for matches, so we also need to reverse the application
	;; order
	(numbers (reverse (cdr e))))
    (when (catch :true (run-search-2 value numbers))
      value)))

(defun puzzle-7b ()
  (let ((equations (read-equations "data/input-7.txt"))
	(sum 0))
    (dolist (e equations)
      (when-let* ((value (test-equation-2 e)))
	(setq sum (+ sum value))))
    sum))

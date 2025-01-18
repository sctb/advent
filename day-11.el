;; -*- lexical-binding: t -*-

(defun read-all ()
  (let ((forms nil))
    (while-let ((form (ignore-errors (read (current-buffer)))))
      (push form forms))
    (nreverse forms)))

(defun read-stones (file)
  (with-temp-buffer
    (insert-file-contents file)
    (read-all)))

(defun evenp (n)
  (and (> n 0) (= (% n 2) 0)))

(defun blink-stone (n times memo)
  (if (= times 0)
      1
    (or (know n times memo)
	(let ((sum (if (eq n 0)
		       (blink-stone 1 (1- times) memo)
		     (let ((len (1+ (floor (log n 10)))))
		       (if (evenp len)
			   (let ((d (expt 10 (/ len 2))))
			     (+ (blink-stone (/ n d) (1- times) memo)
				(blink-stone (mod n d) (1- times) memo)))
			 (blink-stone (* n 2024) (1- times) memo))))))
	  (save n times sum memo)))))

(defun know (n times memo)
  (gethash (vector n times) memo))

(defun save (n times sum memo)
  (puthash (vector n times) sum memo))

(defun blink (stones times memo)
  (let ((sum 0))
    (dolist (n stones)
      (setq sum (+ sum (blink-stone n times memo))))
    sum))

(defun puzzle-11a ()
  (let* ((stones (read-stones "data/input-11.txt"))
	 (memo (make-hash-table :test 'equal))
	 (times 25))
    (blink stones times memo)))

(defun puzzle-11b ()
  (let* ((stones (read-stones "data/input-11.txt"))
	 (memo (make-hash-table :test 'equal))
	 (times 75))
    (blink stones times memo)))

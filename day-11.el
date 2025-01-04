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

(defun change-stone (n)
  (if (eq n 0)
      '(1)
    (let* ((digits (number-to-string n))
	   (len (length digits)))
      (if (evenp len)
	  (let ((i (/ len 2)))
	    (list (string-to-number (substring digits 0 i))
		  (string-to-number (substring digits i))))
	(list (* n 2024))))))

(defun blink (stones)
  (seq-mapcat #'change-stone stones))

(defun puzzle-11a ()
  (let* ((stones (read-stones "data/input-11.txt"))
	 (times 25)
	 (p (make-progress-reporter "Blinking" 0 times)))
    (dotimes (i times)
      (setq stones (blink stones))
      (progress-reporter-update p i))
    (progress-reporter-done p)
    (length stones)))


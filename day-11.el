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

(defun cc (file)
  (let ((result (call-process "cc" nil nil nil file)))
    (eq result 0)))

(defun puzzle-11b ()
  (let ((file "c/11b.c"))
    (when (cc file)
      (let ((output (shell-command-to-string "./a.out")))
	(read output)))))

;; 20 ⇒ 24809     (max 400481269760)
;; 30 ⇒ 1607523   (max 409526509568)
;; 40 ⇒ 104986693 (max 409526509568)
;; 50 ⇒ ~6824135045
;; 60 ⇒ ~443568777925
;; 70 ⇒ ~28831970565125
;; 80 ⇒ ~1874078086733125

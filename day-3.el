;; -*- lexical-binding: t -*-

(defvar mul-regex "mul(\\([0-9]\\{1,3\\}\\),\\([0-9]\\{1,3\\}\\))")

(defun puzzle-3a ()
  (with-temp-buffer
    (insert-file-contents "data/input-3.txt")
    (let ((sum 0))
      (while (re-search-forward mul-regex nil t)
	(let* ((l (car (read-from-string (match-string 1))))
	       (r (car (read-from-string (match-string 2))))
	       (product (* l r)))
	  (setq sum (+ sum product))))
      sum)))

(defun puzzle-3b ()
  (with-temp-buffer
    (insert-file-contents "data/input-3.txt")
    (let ((sum 0)
	  (enabled t)
	  (more t))
      (while more
	(cond (enabled
	       (let ((beg (point))
		     (end (re-search-forward "don't()" nil t)))
		 (when end
		   (goto-char beg))
		 (cond ((re-search-forward mul-regex end t)
			(let* ((l (car (read-from-string (match-string 1))))
			       (r (car (read-from-string (match-string 2))))
			       (product (* l r)))
			  (setq sum (+ sum product))))
		       (end (goto-char end) (setq enabled nil))
		       (t (setq more nil)))))
	      ((re-search-forward "do()" nil t)
	       (setq enabled t))
	      (t (setq more nil))))
      sum)))

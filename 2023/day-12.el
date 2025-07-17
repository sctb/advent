;; -*- lexical-binding: t -*-

(defun read-records (file)
  (with-temp-buffer
    (insert-file-contents file)
    (let ((records nil))
      (while (not (eobp))
	(let ((start (point))
	      (springs nil)
	      (sizes nil))
	  (re-search-forward " ")
	  (setq springs (buffer-substring start (1- (point))))
	  (let* ((text (buffer-substring (point) (line-end-position)))
		 (split (split-string text ",")))
	    (setq sizes (mapcar #'read split)))
	  (push (cons springs sizes) records))
	(forward-line))
      (nreverse records))))

(defun arrangements (springs sizes)
  1)

(defun puzzle-12a ()
  ;; example-12.txt: 21
  ;; input-12.txt: 
  (let* ((file "data/example-12.txt")
	 (records (read-records file))
	 (count 0))
    (dolist (row records)
      (incf count (arrangements (car row) (cdr row))))
    count))

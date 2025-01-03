;; -*- lexical-binding: t -*-

(defun read-diskmap (file)
  (with-temp-buffer
    (let ((digits nil))
      (insert-file-contents file)
      (while-let ((c (char-after)))
	(unless (eq c ?\n)
	  (let ((n (string-to-number (string c))))
	    (push n digits)))
	(forward-char))
      (nreverse digits))))

(defun parse-diskmap (map)
  (let ((blocks [])
	(id 0)
	(count 0))
    ;; This function is slow to allocate and copy the block vector on
    ;; every iteration. A power-of-two growth strategy should be an
    ;; obvious optimization, but for now just report progress instead.
    (let* ((i 0)
	   (p (make-progress-reporter "Parsing diskmap" i (length map))))
      (dolist (n map)
	(let ((v (make-vector n id)))
	  (setq blocks (vconcat blocks v)))
	(if id
	    (setq id nil)
	  (setq count (+ count 1))
	  (setq id count))
	(setq i (+ i 1))
	(progress-reporter-update p i))
      (progress-reporter-done p))    
    blocks))

(defun print-disk (blocks)
  (let* ((n (length blocks))
	 (s (make-string n ?.)))
    (dotimes (i n)
      (when-let* ((x (aref blocks i)))
	(aset s i (+ x 48))))
    s))

(defun compact-disk (blocks)
  (let ((r (1- (length blocks)))
	(w 0))
    (while (> r w)
      (let ((rb (aref blocks r))
	    (wb (aref blocks w)))
	(when (and rb (null wb))
	  (aset blocks w rb)
	  (aset blocks r nil))
	(when (null rb)
	  (setq r (- r 1)))
	(when wb
	  (setq w (+ w 1)))))
    blocks))

(defun checksum (blocks)
  (let ((sum 0))
    (dotimes (i (length blocks))
      (when-let* ((b (aref blocks i)))
	(setq sum (+ sum (* b i)))))
    sum))

(defun puzzle-9a ()
  (let* ((map (read-diskmap "data/input-9.txt"))
	 (blocks (parse-diskmap map)))
    (compact-disk blocks)
    (checksum blocks)))

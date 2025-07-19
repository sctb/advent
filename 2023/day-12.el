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

(defun count-char (char string)
  (seq-count (lambda (c) (eq c char)) string))

(defun consistent-p (springs sizes)
  ;; Check to see if ‘springs’ (containing no unknowns) are correctly
  ;; grouped into contiguous broken springs separated by at least one
  ;; operational spring
  (let ((size nil))
    (catch :inconsistent
      (dotimes (i (length springs))
	(let ((c (elt springs i)))
	  (if (eq c ?.)
	      (unless (null size)
		(if (> size 0)
		    (throw :inconsistent nil)
		  (setq size nil)))
	    (when (null size)
	      (setq size (pop sizes)))
	    (decf size)
	    (when (< size 0)
	      (throw :inconsistent nil)))))
      (and (null sizes) (or (null size) (zerop size))))))

(consistent-p "#.#.###" '(1 1 3))
(consistent-p ".#...#....###." '(1 1 3))
(consistent-p ".#.###.#.######" '(1 3 1 6))
(consistent-p "####.#...#..." '(4 1 1))
(consistent-p "#....######..#####." '(1 6 5))
(consistent-p ".###.##....#" '(3 2 1))

(defun arrangements (springs sizes)
  0)

(defun puzzle-12a ()
  ;; example-12.txt: 21
  ;; input-12.txt: 
  (let* ((file "data/example-12.txt")
	 (records (read-records file))
	 (count 0))
    (dolist (row records)
      (incf count (arrangements (car row) (cdr row))))
    count))

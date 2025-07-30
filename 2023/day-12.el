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

(defun positions (char string)
  (let ((positions))
    (dotimes (i (length string))
      (when (eq char (elt string i))
	(push i positions)))
    (nreverse positions)))

(defun range (n)
  (let ((list nil))
    (dotimes (i n)
      (push i list))
    (nreverse list)))

(defvar comb-cache (make-hash-table :test 'equal))

(defun combinations (k n)
  ;; "One way is to track k index numbers of the elements selected,
  ;; starting with {0 .. k−1} (zero-based) or {1 .. k} (one-based) as
  ;; the first allowed k-combination. Then, repeatedly move to the
  ;; next allowed k-combination by incrementing the smallest index
  ;; number for which this would not create two equal index numbers,
  ;; at the same time resetting all smaller index numbers to their
  ;; initial values."
  ;; https://en.wikipedia.org/wiki/Combination#Enumerating_k-combinations
  (let ((key (cons k n)))
    (or (gethash key comb-cache)
	(let* ((index (range k))
	       (combs (list (copy-sequence index)))
	       (again t))
	  (while again
	    (setq again nil)
	    (let ((i 0))
	      ;; find the smallest index number to increment
	      (while (and (< i (1- k))
			  (= (elt index i)
			     (1- (elt index (1+ i)))))
		(incf i))
	      (let ((m (elt index i)))
		(when (< m (1- n))
		  (incf (elt index i))
		  (dotimes (j i)
		    ;; reset smaller index numbers
		    (setf (elt index j) j))
		  (setq again t)
		  (push (copy-sequence index) combs)))))
	  (puthash key combs comb-cache)))))

(defun consistent-p (springs sizes broken)
  ;; Check to see if ‘springs’ (containing no unknowns) are correctly
  ;; grouped into contiguous broken springs separated by at least one
  ;; operational spring
  (let ((b (pop broken))
	(size nil))
    (catch :inconsistent
      (dotimes (i (length springs))
	(let ((c (elt springs i)))
	  (when (eq c ??)
	    (if (eq i b)
		(setq c ?# b (pop broken))
	      (setq c ?.)))
	  (if (eq c ?.)
	      (unless (null size)
		(if (> size 0)
		    (throw :inconsistent nil)
		  (setq size nil)))
	    (when (null size)
	      (setq size (pop sizes)))
	    (when size
	      (decf size))
	    (when (and size (< size 0))
	      (throw :inconsistent nil)))))
      (and (null sizes) (or (null size) (zerop size))))))

(defun at (a b)
  (let ((list nil))
    (dolist (i b)
      (push (elt a i) list))
    (nreverse list)))

(defun arrangements (springs sizes)
  (let* ((need (apply #'+ sizes))
	 (have (count-char ?# springs))
	 (k (- need have))
	 (slots (positions ?? springs)))
    (if (or (zerop k) (null slots))
	1
      (let* ((n (length slots))
	     (combs (combinations k n))
	     (valid 0))
	(dolist (comb combs)
	  (let ((broken (at slots comb)))
	    (when (consistent-p springs sizes broken)
	      (incf valid))))
	valid))))

(defun puzzle-12a ()
  ;; example-12.txt: 21
  ;; input-12.txt: 7007
  (let* ((file "data/input-12.txt")
	 (comb-cache (make-hash-table :test 'equal))
	 (records (read-records file))
	 (n 0)
	 (p (make-progress-reporter "Arranging" n (length records)))
	 (count 0))
    (dolist (row records)
      (incf count (arrangements (car row) (cdr row)))
      (incf n)
      (progress-reporter-update p n))
    (progress-reporter-done p)
    count))

(defun ways (springs sizes)
  (let ((len (length springs)))
    (catch :done
      ;; base cases
      (when (zerop len)
	(throw :done (if sizes 0 1)))
      (when (null sizes)
	(throw :done (if (seq-contains-p springs ?#) 0 1)))
      (when (< len (1- (apply #'+ sizes)))
	(throw :done 0))
      (let ((c (aref springs 0)))
	(pcase c
	  (?. (ways (substring springs 1) sizes))
	  (?# (let ((size (car sizes)))
		(when (< len size)
		  (throw :done 0))
		(when (and (> len size)
			   (eq (aref springs size) ?#))
		  (throw :done 0))
		(let ((run (substring springs 0 size)))
		  (when (seq-contains-p run ?.)
		    (throw :done 0)))
		(if (> len size)
		    (ways (substring springs (1+ size)) (cdr sizes))
		  (ways "" (cdr sizes)))))
	  (?? (let ((rest (substring springs 1)))
		(+ (ways (concat "#" rest) sizes)
		   (ways (concat "." rest) sizes)))))))))

(defun puzzle-12b ()
  ;; example-12.txt: 525152
  ;; input-12.txt: 
  (let* ((file "data/example-12.txt")
	 (records (read-records file))
	 (count 0))
    (dolist (row records)
      (let* ((springs (car row))
	     (sizes (cdr row))
	     (n (ways springs sizes)))
	(message "%s: %s" springs n)
	(incf count n)))
    count))

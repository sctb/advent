;; -*- lexical-binding: t -*-

(defun read-grid (file)
  (with-temp-buffer
    (insert-file-contents file)
    (let* ((w (1- (line-end-position)))
	   (h (line-number-at-pos (1- (point-max))))
	   (g (make-grid h w)))
      (dotimes (i h)
	(let ((s (buffer-substring (point) (line-end-position))))
	  (dotimes (j w)
	    (gset g i j (aref s j)))
	  (forward-line)))
      g)))

(defun make-grid (h w &optional blank)
  (let ((g (make-vector h nil)))
    (dotimes (i h)
      (aset g i (make-vector w (or blank ?.))))
    g))

(defun grid-like (g &optional blank)
  (make-grid (grid-height g) (grid-width g) blank))

(defun copy-grid (g)
  (let* ((h (length g))
	 (c (make-vector h nil)))
    (dotimes (i h)
      (aset c i (copy-sequence (aref g i))))
    c))

(defun reset-grid (a b)
  (dotimes (i (grid-height a))
    (dotimes (j (grid-width a))
      (gset b i j (gref a i j)))))

(defun count-grid (g c)
  (let ((count 0))
    (dotimes (i (grid-height g))
      (dotimes (j (grid-width g))
	(when (eq (gref g i j) c)
	  (setq count (+ count 1)))))
    count))

(defun grid-height (g)
  (length g))

(defun grid-width (g)
  (length (aref g 0)))

(defun insert-grid (g)
  (seq-do (lambda (r) (seq-do #'insert r) (insert ?\n)) g))

(defun gset (g i j v)
  "Ignores out-of-bounds references"
  (when (and (>= i 0) (< i (length g)))
    (let ((r (aref g i)))
      (when (and (>= j 0) (< j (length r)))
	(aset r j v)))))

(defun gref (g i j)
  "Returns nil for out-of-bounds references"
  (when (and (>= i 0) (< i (length g)))
    (let ((r (aref g i)))
      (when (and (>= j 0) (< j (length r)))
	(aref r j)))))

(defun around (g i j)
  (list (gref g (1- i) j)
	(gref g i (1+ j))
	(gref g (1+ i) j)
	(gref g i (1- j))))

(defun perimeter (c plots g)
  (let ((count 0))
    (dolist (plot plots)
      (let ((i (car plot))
	    (j (cdr plot)))
	(dolist (d (around g i j))
	  (unless (eq d c)
	    (setf count (+ count 1))))))
    count))

(defun adjacentp (a b)
  (pcase-let ((`(,ai . ,aj) a)
	      (`(,bi . ,bj) b))
    (or (and (eq ai bi)
	     (or (eq aj (1- bj))
		 (eq aj (1+ bj))))
	(and (eq aj bj)
	     (or (eq ai (1- bi))
		 (eq ai (1+ bi)))))))

(defun stick (region regions)
  (seq-find (lambda (plots)
	      (seq-find (lambda (other)
			  (seq-find (lambda (plot)
				      (adjacentp plot other))
				    region))
			plots))
	    regions))

(defun consolidate (plots)
  (let ((regions (mapcar #'list plots))
	(changed t))
    (while changed
      (setq changed nil)
      (dolist (region regions)
	(let ((other (remq region regions)))
	  (when-let* ((stuck (stick region other)))
	    (nconc stuck region)
	    (setq regions other)
	    (setq changed t)))))
    regions))

(defun region-price (c plots g)
  (let ((area (length plots))
	(perimeter (perimeter c plots g)))
    (* area perimeter)))

(defun puzzle-12a ()
  (let ((g (read-grid "data/input-12.txt"))
	(plots (make-hash-table))
	(total 0))
    (dotimes (i (grid-height g))
      (dotimes (j (grid-width g))
	(let ((c (gref g i j)))
	  (push (cons i j) (gethash c plots)))))
    (maphash (lambda (c plots)
	       (dolist (region (consolidate plots))
		 (let ((price (region-price c region g)))
		   (setq total (+ total price)))))
	     plots)
    total))

(defun deltas (dir)
  (pcase dir
    (:up    '(-1 .  0))
    (:right '( 0 .  1))
    (:down  '( 1 .  0))
    (:left  '( 0 . -1))))

(defun scanwise (dir)
  (pcase dir
    (:up :right)
    (:right :down)
    (:down :right)
    (:left :down)))

(defun look (g i j dir)
  (let ((d (deltas dir)))
    (gref g (+ i (car d)) (+ j (cdr d)))))

(defun borders (i j)
  (list (list :up (1- i) j)
	(list :right i (1+ j))
	(list :down (1+ i) j)
	(list :left i (1- j))))

(defun mark-side (c dir i j g x)
  ;; after counting one boundary between regions, mark all of the
  ;; plots along its edge as accounted for
  (let ((next (deltas (scanwise dir))))
    ;; move within the current region along the current edge...
    (while (and (eq c (gref g i j))
		;; ... as long as the border holds
		(not (eq c (look g i j dir))))
      (let ((dirs (gref x i j)))
	(gset x i j (cons dir dirs)))
      (setq i (+ i (car next)))
      (setq j (+ j (cdr next))))))

(defun sides (c plots g x)
  (let ((count 0))
    (dolist (plot plots)
      (let ((i (car plot))
	    (j (cdr plot)))
	(dolist (border (borders i j))
	  (pcase-let ((`(,dir ,n ,m) border))
	    (let ((d (gref g n m)))
	      (unless (or (eq d c) (memq dir (gref x i j)))
		(mark-side c dir i j g x)
		(setf count (+ count 1))))))))
    count))

(defun discounted-price (c plots g x)
  (let ((area (length plots))
	(sides (sides c plots g x)))
    (* area sides)))

(defun puzzle-12b ()
  (let* ((g (read-grid "data/input-12.txt"))
	 (x (grid-like g t))
	 (plots (make-hash-table))
	 (total 0))
    (dotimes (i (grid-height g))
      (dotimes (j (grid-width g))
	(let ((c (gref g i j)))
	  (push (cons i j) (gethash c plots)))))
    (let* ((n 0)
	   (max (hash-table-count plots))
	   (p (make-progress-reporter "Computing" n max)))
      (maphash (lambda (c plots)
		 (dolist (region (consolidate plots))
		   (let ((price (discounted-price c region g x)))
		     (setq total (+ total price))))
		 (setq n (+ n 1))
		 (progress-reporter-update p n))
	       plots)
      (progress-reporter-done p))
    total))

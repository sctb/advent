;; -*- lexical-binding: t -*-

(defvar p-rx "p=\\([0-9]\\{1,3\\}\\),\\([0-9]\\{1,3\\}\\)")
(defvar v-rx "v=\\(-?[0-9]\\{1,3\\}\\),\\(-?[0-9]\\{1,3\\}\\)")

(defun match-pair ()
  (cons (read (match-string 1))
	(read (match-string 2))))

(defun read-robots (file)
  (with-temp-buffer
    (insert-file-contents file)
    (let ((robots nil))
      (while (re-search-forward p-rx nil t)
	(let ((p (match-pair)))
	  (re-search-forward v-rx)
	  (push (cons p (match-pair)) robots)))
      (nreverse robots))))

(defun simulate1 (r bounds)
  (pcase-let ((`(,bx . ,by) bounds)
	      (`((,rx . ,ry) ,rvx . ,rvy) r))
    (let ((rp (car r)))
      (setcar rp (mod (+ rx rvx) bx))
      (setcdr rp (mod (+ ry rvy) by)))))

(defun simulate (robots bounds secs)
  (dotimes (_ secs)
    (dolist (r robots)
      (simulate1 r bounds))))

(defun quarter (bounds)
  (let* ((x (car bounds))
	 (y (cdr bounds))
	 (x‾ (/ (1- x) 2))
	 (y‾ (/ (1- y) 2)))
    (list (cons (cons 0 x‾)
		(cons 0 y‾))
	  (cons (cons (1+ x‾) x)
		(cons 0 y‾))
	  (cons (cons 0 x‾)
		(cons (1+ y‾) y))
	  (cons (cons (1+ x‾) x)
		(cons (1+ y‾) y)))))

(defun inside (r q)
  (pcase-let ((`((,rx . ,ry)) r)
	      (`((,qx1 . ,qx2) ,qy1 . ,qy2) q))
    (and (>= rx qx1) (< rx qx2)
	 (>= ry qy1) (< ry qy2))))

(defun safety-factor (robots bounds)
  (let ((factor 1))
    (dolist (q (quarter bounds))
      (let ((n 0))
	(dolist (r robots)
	  (when (inside r q)
	    (setq n (1+ n))))
	(setq factor (* factor n))))
    factor))

(defun puzzle-14a ()
  (let ((robots (read-robots "data/input-14.txt"))
	(bounds '(101 . 103)))
    (simulate robots bounds 100)
    (safety-factor robots bounds)))

(defun cc (file)
  (let ((result (call-process "cc" nil nil nil file)))
    (eq result 0)))

(defun puzzle-14ac ()
  (let ((file "c/14a.c"))
    (when (cc file)
      (let ((output (shell-command-to-string "./a.out")))
	(read output)))))

(defun symmetrical-n (robots bounds)
  (let* ((x (car bounds))
	 (x‾ (/ (1- x) 2))
	 (track (make-hash-table)))
    (dolist (r robots)
      (pcase-let ((`((,rx . ,ry)) r))
	(let* ((rx (abs (- x‾ rx)))
	       ;; generate a unique key for each (x, y) point
	       ;; reflected around the x midpoint
	       (key (+ rx (* ry x))))
	  (when (> rx 0) 		; center is always symmetrical
	    (if (gethash key track)
		;; remove symmetrical points from the tracking table
		(remhash key track)
	      (puthash key t track))))))
    (hash-table-count track)))

(defun symmetrical-p (robots bounds)
  (= (symmetrical-n robots bounds) 0))

(defun symmetrical-ish (robots bounds)
  (< (symmetrical-n robots bounds) 100))

(defun puzzle-14b ()
  (let* ((robots (read-robots "data/input-14.txt"))
	 (bounds '(101 . 103))
	 (i 0)
	 (max 1000000)
	 (p (make-progress-reporter "Simulating" i max)))
    (while (and (< i max) (not (symmetrical-ish robots bounds)))
      (simulate robots bounds 1)
      (setq i (1+ i))
      (progress-reporter-update p i))
    (progress-reporter-done p)
    (and (< i max) i)))

(defun convert-robots ()
  "Convenience function for generating C struct literals"
  (interactive)
  (while (re-search-forward p-rx nil t)
    (replace-match "{\\1, \\2},")
    (re-search-forward v-rx)
    (replace-match "{\\1, \\2}")))

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
      (aset g i (make-vector w (unless blank ?.))))
    g))

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

(defun wake-robots (g)
  (let ((robots nil))
    (dotimes (y (grid-height g))
      (dotimes (x (grid-width g))
	(when (eq (gref g y x) ?*)
	  (push `((,x . ,y)) robots))))
    robots))

(defun insert-robots-c (robots)
  (dolist (r robots)
    (insert (format "  {{%d, %d}, {0, 0}},\n" (caar r) (cdar r)))))

(defun mirror ()
  (interactive)
  (save-excursion
    (while (not (eobp))
      (let ((line (buffer-substring (point) (line-end-position))))
	(goto-char (line-end-position))
	(insert (substring (reverse line) 1))
	(forward-line)))))

(defun puzzle-14x ()
  (let* ((g (read-grid "data/tree-14.txt"))
	 ;; (bounds (cons (grid-width g) (grid-height g)))
	 (robots (wake-robots g)))
    (insert-robots-c robots)))

;; -*- lexical-binding: t -*-

(defun read-line ()
  (buffer-substring (point) (line-end-position)))

(defun read-hands (file)
  (with-temp-buffer
    (insert-file-contents file)
    (let ((hands nil))
      (while (not (eobp))
	(let* ((line (read-line))
	       (split (split-string line))
	       (hand (cons (car split) (read (cadr split)))))
	  (push hand hands))
	(forward-line))
      (nreverse hands))))

(defun hand-type (hand)
  (let ((cards nil))
    (seq-do (lambda (c)
	      (let ((n (alist-get c cards 0)))
		(setf (alist-get c cards) (1+ n))))
	    hand)
    (let* ((counts (mapcar #'cdr cards))
	   (lead (car (sort counts :reverse t))))
      (pcase (length cards)
	(5 1)				; distinct
	(4 2)				; one pair
	(3 (if (eq lead 3) 4 3))	; three of a kind or two pair
	(2 (if (eq lead 4) 6 5))	; four of a kind or full house
	(1 7)))))			; five of a kind

(defun hand-score (hand)
  (let* ((order "23456789TJQKA")
	 (base (length order))
	 (high (seq-reverse hand))
	 (score 0))
    (dotimes (i (length hand))
      (let* ((c (elt high i))
	     (factor (expt base i))
	     (rank (seq-position order c)))
	(setq score (+ score (* factor rank)))))
    (let* ((i (length hand))
	   (factor (expt base i))
	   (type (hand-type hand)))
      (setq score (+ score (* factor type))))
    score))

(defun sort-hands (pairs)
  (sort pairs :key (lambda (pair) (hand-score (car pair)))))

(defun puzzle-7a ()
  (let* ((pairs (read-hands "data/input-7.txt"))
	 (sorted (sort-hands pairs))
	 (rank 1)
	 (winnings 0))
    (dolist (pair sorted)
      (let* ((bid (cdr pair))
	     (win (* bid rank)))
	(setq winnings (+ winnings win)))
      (setq rank (1+ rank)))
    winnings))

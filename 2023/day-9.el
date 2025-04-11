;; -*- lexical-binding: t -*-

(defun read-line ()
  (prog1
      (buffer-substring (point) (line-end-position))
    (forward-line)))

(defun read-list (string)
  (read (concat "(" string ")")))

(defun read-histories (file)
  (with-temp-buffer
    (insert-file-contents file)
    (let ((hists nil))
      (while (not (eobp))
	(let* ((line (read-line))
	       (hist (read-list line)))
	  (push hist hists)))
      (nreverse hists))))

(defun deltas (list)
  (let ((deltas nil)
	(a (pop list)))
    (while-let ((b (pop list)))
      (push (- b a) deltas)
      (setq a b))
    (nreverse deltas)))

(defun sequences (list)
  (let ((seqs (list list)))
    (while (not (and (zerop (elt list 0))
		     (zerop (elt list 1))))
      (setq list (deltas list))
      (push list seqs))
    (nreverse seqs)))

(defun extrapolate (seqs)
  (let* ((seqs (reverse seqs))
	 (last (mapcar #'car (mapcar #'reverse seqs))))
    (seq-reduce #'+ last 0)))

(defun puzzle-9a ()
  (let ((hists (read-histories "data/input-9.txt"))
	(sum 0))
    (dolist (hist hists)
      (let* ((seqs (sequences hist))
	     (next (extrapolate seqs)))
	(setq sum (+ sum next))))
    sum))

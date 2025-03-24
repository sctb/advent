;; -*- lexical-binding: t -*-

(defvar card-rx "Card[ ]+[0-9]+: \\([ 0-9]+\\) | \\([ 0-9]+\\)")

(defun read-list (string)
  (read (concat "(" string ")")))

(defun read-cards (file)
  (let ((cards nil))
    (with-temp-buffer
      (insert-file-contents file)
      (while (re-search-forward card-rx nil t)
	(let ((win (read-list (match-string 1)))
	      (got (read-list (match-string 2))))
	  (push (cons win got) cards))))
    (nreverse cards)))

(defun puzzle-4a ()
  (let ((cards (read-cards "data/input-4.txt"))
	(total 0))
    (dolist (card cards)
      (let* ((win (car card))
	     (got (cdr card))
	     (n (length (seq-intersection win got))))
	(when (> n 0)
	  (let ((points (expt 2 (1- n))))
	    (setq total (+ total points))))))
    total))

(defmacro defmemo (name arglist &rest body)
  (declare (indent 2))
  (let ((table (gensym))
	(result (gensym))
	(key (car arglist)))
    `(let ((,table (make-hash-table)))
       (defun ,name ,arglist
	 (if-let* ((,result (gethash ,key ,table)))
	     ,result
	   (let ((,result (progn ,@body)))
	     (puthash ,key ,result ,table)))))))

(defmemo tally (cards)
  (let* ((card (car cards))
	 (win (car card))
	 (got (cdr card))
	 (n (length (seq-intersection win got)))
	 (total 1))
    (when (> n 0)
      (let ((cards (cdr cards)))
	(dotimes (_ n)
	  (setq total (+ total (tally cards)))
	  (setq cards (cdr cards)))))
    total))

(defun puzzle-4b ()
  (let ((cards (read-cards "data/input-4.txt"))
	(total 0))
    (while cards
      (setq total (+ total (tally cards)))
      (setq cards (cdr cards)))
    total))

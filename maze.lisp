(defpackage :maze
  (:use :common-lisp))

(in-package :maze)

(defun read-maze (file)
  (let ((in (open file))
	(rows nil))
    (loop for line = (read-line in nil)
	  for bytes = (map 'list #'char-code line)
	  while line do (push bytes rows))
    (let ((height (length rows))
	  (width (length (car rows))))
      (make-array (list height width)
		  :element-type 'unsigned-byte
		  :initial-contents (nreverse rows)))))

(defun make-scores (maze)
  (make-array (array-dimensions maze)
	      :element-type 'fixnum
	      :initial-element most-positive-fixnum))

(defun make-paths (maze)
  (make-array (array-dimensions maze)
	      :element-type 'fixnum
	      :initial-element 0))

(defun find-reindeer (maze)
  (destructuring-bind (n m) (array-dimensions maze)
    (block found
      (loop for i from 0 below n do
            (loop for j from 0 below m
		  when (eq (aref maze i j) (char-code #\S))
		  do (return-from found (cons i j)))))))

(defun find-end (maze)
  (destructuring-bind (n m) (array-dimensions maze)
    (block found
      (loop for i from 0 below n do
        (loop for j from 0 below m
	      when (eq (aref maze i j) (char-code #\E))
	      do (return-from found (cons i j)))))))

(defun around (deer)
  (destructuring-bind (dir score (i . j)) deer
    (declare (ignore score))
    `(,@(unless (eq dir :west)
	  (list (list :east (cons i (1+ j)))))
	,@(unless (eq dir :south)
	    (list (list :north (cons (1- i) j))))
	,@(unless (eq dir :east)
	    (list (list :west (cons i (1- j)))))
	,@(unless (eq dir :north)
	    (list (list :south (cons (1+ i) j)))))))

(defun score (from to)
  (ecase from
    (:west (ecase to (:west 1) (:north 1001) (:south 1001)))
    (:south (ecase to (:south 1) (:east 1001) (:west 1001)))
    (:east (ecase to (:east 1) (:north 1001) (:south 1001)))
    (:north (ecase to (:north 1) (:east 1001) (:west 1001)))))

(defun passable (tile)
  (or (eq tile (char-code #\.))
      (eq tile (char-code #\E))))

(defun gref (grid pos)
  (aref grid (car pos) (cdr pos)))

(defun gset (grid pos value)
  (setf (aref grid (car pos) (cdr pos)) value))

(defun moves (maze deer)
  (let ((moves nil))
    (dolist (a (around deer))
      (destructuring-bind (to pos) a
	(when (passable (gref maze pos))
	  (destructuring-bind (from score prev) deer
	    (declare (ignore prev))
	    (let ((score (+ score (score from to))))
	      (push (list to score pos) moves))))))
    moves))

(defvar *token* 0)
(defvar *finishers* nil)

(defun finish (score)
  (let* ((token (incf *token*))
	 (receipt (cons token score)))
    (push receipt *finishers*)
    receipt))

(defun register (paths pos receipt)
  (gset paths pos (car receipt))
  receipt)

(defun escape (maze scores paths deer)
  (destructuring-bind (dir score pos) deer
    (declare (ignore dir))
    (let ((par (gref scores pos)))
      (unless (> score (+ 1000 par))
	(gset scores pos (min score par))
	(if (eq (gref maze pos) (char-code #\E))
	    (register paths pos (finish score))
	    (let ((best nil))
	      (loop for deer in (moves maze deer) do
	        (let ((receipt (escape maze scores paths deer)))
		  (when receipt
		    (register paths (elt deer 2) receipt)
		    (when (or (null best)
			      (< (cdr receipt) (cdr best)))
		      (setq best receipt)))))
	      best))))))

(defun winners (best)
  (loop for (token . score) in *finishers*
	when (<= score best)
	collect token))

(defun count-winners (paths best)
  (let ((count 1) ; include starting tile
	(winners (winners best)))
    (destructuring-bind (n m) (array-dimensions paths)
      (loop for i from 0 below n do
        (loop for j from 0 below m
	      when (member (aref paths i j) winners)
	      do (incf count))))
    count))

(defun solve ()
  (let* ((maze (read-maze "data/input-16.txt"))
	 (scores (make-scores maze))
	 (paths (make-paths maze))
	 (deer (list :east 0 (find-reindeer maze)))
	 (end (find-end maze))
	 (*finishers* nil)
	 (*token* 0))
    (escape maze scores paths deer)
    (let ((best (gref scores end)))
      (count-winners paths best))))

;; ⇒ 452 too low

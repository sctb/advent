;; -*- lexical-binding: t -*-

(defun read-line ()
  (buffer-substring (point) (line-end-position)))

(defun read-towels ()
  (let ((line (read-line)))
    (forward-line)
    (forward-line)
    (split-string line "," nil " ")))

(defun read-designs ()
  (let ((designs nil))
    (while (not (eobp))
      (push (read-line) designs)
      (forward-line))
    (nreverse designs)))

(defvar onsen-towels nil)
(defvar onsen-designs nil)
(defvar onsen-size 0)
(defvar onsen-known (make-hash-table))

(defun match (prefix string offset)
  (let ((i 0)
	(p (length prefix))
	(s (- (length string) offset)))
    (catch :done
      (while (eq (elt prefix i) (elt string (+ i offset)))
	(setq i (1+ i))
	(when (= i p) (throw :done i))
	(when (= i s) (throw :done 0)))
      0)))

(defun key (index offset)
  (+ (* index onsen-size) offset))

(defun known (index offset)
  (gethash (key index offset) onsen-known))

(defun seen (index offset ways)
  (let ((code (key index offset)))
    (puthash code ways onsen-known)))

(defun possible (design index offset length)
  (cond* ((bind* (ways (known index offset))) ways)
	 ((eq offset length) 1)
	 (t (let ((ways 0))
	      (dolist (towel onsen-towels)
		(let ((i (match towel design offset)))
		  (when (> i 0)
		    (let ((n (possible design index (+ offset i) length)))
		      (setq ways (+ ways n))))))
	      (seen index offset ways)))))

(defmacro with-onsen (file &rest body)
  (declare (indent 1))
  `(let ((onsen-towels nil)
	 (onsen-designs nil)
	 (onsen-size 0)
	 (onsen-known (make-hash-table)))
     (with-temp-buffer
       (insert-file-contents ,file)
       (setq onsen-towels (read-towels))
       (setq onsen-designs (read-designs))
       (setq onsen-size (length onsen-designs)))
     ,@body))

(defun puzzle-19a ()
  (with-onsen "data/input-19.txt"
    (let ((index 0)
	  (count 0))
      (dolist (design onsen-designs)
	(when (> (possible design index 0 (length design)) 0)
	  (setq count (1+ count)))
	(setq index (1+ index)))
      count)))

(defun puzzle-19b ()
  (with-onsen "data/input-19.txt"
    (let ((index 0)
	  (ways 0))
      (dolist (design onsen-designs)
	(let ((n (possible design index 0 (length design))))
	  (setq ways (+ ways n)))
	(setq index (1+ index)))
      ways)))

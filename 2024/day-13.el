;; -*- lexical-binding: t -*-

(defvar a-rx "Button A: X\\+\\([0-9]\\{2\\}\\), Y\\+\\([0-9]\\{2\\}\\)")
(defvar b-rx "Button B: X\\+\\([0-9]\\{2\\}\\), Y\\+\\([0-9]\\{2\\}\\)")
(defvar p-rx "Prize: X=\\([0-9]\\{1,5\\}\\), Y=\\([0-9]\\{1,5\\}\\)")

(defun match-pair ()
  (cons (read (match-string 1))
	(read (match-string 2))))

(defun read-machines (file)
  (with-temp-buffer
    (insert-file-contents file)
    (let ((machines nil))
      (while (re-search-forward a-rx nil t)
	(let ((machine nil))
	  (push (cons 'a (match-pair)) machine)
	  (re-search-forward b-rx)
	  (push (cons 'b (match-pair)) machine)
	  (re-search-forward p-rx)
	  (push (cons 'p (match-pair)) machine)
	  (forward-line)
	  (push machine machines)))
      (nreverse machines))))

(defun max-times (x p)
  (min (/ (car p) (car x))
       (/ (cdr p) (cdr x))
       ;; "no more than 100 times"
       100))

(defun cost (a b p)
  (pcase-let ((`(,na ,ax . ,ay) a)
	      (`(,nb ,bx . ,by) b)
	      (`(,px . ,py) p))
    (when (and (eq (+ (* na ax) (* nb bx)) px)
	       (eq (+ (* na ay) (* nb by)) py))
      (+ (* na 3) nb))))

(defun search (a b p)
  (let ((cost nil))
    (pcase-let ((`(,ax . ,ay) a)
		(`(,px . ,py) p))
      ;; brute force
      (dotimes (na (1+ (max-times a p)))
	(let* ((tx (- px (* na ax)))
	       (ty (- py (* na ay)))
	       (nb (max-times b (cons tx ty))))
	  (when-let* ((c (cost (cons na a) (cons nb b) p)))
	    (if (null cost)
		(setq cost c)
	      (setq cost (min cost c)))))))
    cost))

(defun play (machine)
  (let* ((p (alist-get 'p machine))
	 (a (alist-get 'a machine))
	 (b (alist-get 'b machine)))
    (search a b p)))

(defun puzzle-13a ()
  (let* ((machines (read-machines "data/input-13.txt"))
	 (cost 0))
    (dolist (machine machines)
      (when-let* ((c (play machine)))
	(setq cost (+ cost c))))
    cost))

(defun fudge (n)
  (let ((m (round n)))
    (cond ((= n m) m)
	  ((< (abs (- n m)) 0.00000001) m)
	  (t nil))))

(defun solve (a b p)
  (pcase-let ((`(,ax . ,ay) a)
	      (`(,bx . ,by) b)
	      (`(,px . ,py) p))
    ;; Cramer's rule
    (let ((det (float (- (* ax by) (* ay bx)))))
      (unless (= (fudge det) 0)
	(let ((na (fudge (/ (- (* by (float px))
			       (* bx (float py)))
			    det)))
	      (nb (fudge (/ (- (* ax (float py))
			       (* ay (float px)))
			    det))))
	  (when (and na nb)
	    (+ (* na 3) nb)))))))

(defun play-hard (machine)
  (let* ((p (alist-get 'p machine))
	 (a (alist-get 'a machine))
	 (b (alist-get 'b machine)))
    (pcase-let* ((`(,x . ,y) p))
      (let ((x (+ x 10000000000000))
	    (y (+ y 10000000000000)))
	(solve a b (cons x y))))))

(defun puzzle-13b ()
  (let* ((machines (read-machines "data/input-13.txt"))
	 (cost 0))
    (dolist (machine machines)
      (when-let* ((c (play-hard machine)))
	(setq cost (+ cost c))))
    cost))

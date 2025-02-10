;; -*- lexical-binding: t -*-

(defconst rega-rx "Register A: \\([0-9]+\\)")
(defconst regb-rx "Register B: \\([0-9]+\\)")
(defconst regc-rx "Register C: \\([0-9]+\\)")
(defconst prog-rx "Program: \\([0-9,]+\\)")

(defun read-reg ()
  (let ((string (match-string 1)))
    (if string (read string) 0)))

(defun read-prog ()
  (mapcar #'read (split-string (match-string 1) ",")))

(defvar comp-rega)
(defvar comp-regb)
(defvar comp-regc)
(defvar comp-ip)
(defvar comp-prog)
(defvar comp-out)
(defvar comp-jmp)

(defun load-program (file)
  (with-temp-buffer
    (insert-file-contents file)
    (re-search-forward rega-rx)
    (setq comp-rega (read-reg))
    (re-search-forward regb-rx)
    (setq comp-regb (read-reg))
    (re-search-forward regc-rx)
    (setq comp-regc (read-reg))
    (re-search-forward prog-rx)
    (setq comp-prog (read-prog)))
  (setq comp-ip 0)
  (setq comp-out nil)
  (setq comp-jmp nil))

(defun literal-operand ()
  (elt comp-prog (1+ comp-ip)))

(defun combo-operand ()
  (let ((arg (literal-operand)))
    (cond ((< arg 4) arg)
	  ((= arg 4) comp-rega)
	  ((= arg 5) comp-regb)
	  ((= arg 6) comp-regc)
	  ((= arg 7) (error "Reserved combo operand")))))

(defconst inst-adv 0)
(defconst inst-bxl 1)
(defconst inst-bst 2)
(defconst inst-jnz 3)
(defconst inst-bxc 4)
(defconst inst-out 5)
(defconst inst-bdv 6)
(defconst inst-cdv 7)

(defun common-xdv ()
  (let* ((numerator comp-rega)
	 (denominator (expt 2 (combo-operand))))
    ;; integer division automatically truncated
    (/ numerator denominator)))

(defun micro-adv ()
  (setq comp-rega (common-xdv)))

(defun micro-bxl ()
  (let ((result (logxor comp-regb (literal-operand))))
    (setq comp-regb result)))

(defun micro-bst ()
  (let ((result (mod (combo-operand) 8)))
    (setq comp-regb result)))

(defun micro-jnz ()
  (unless (eq comp-rega 0)
    (setq comp-jmp t)
    (setq comp-ip (literal-operand))))

(defun micro-bxc ()
  (let ((result (logxor comp-regb comp-regc)))
    (setq comp-regb result)))

(defun micro-out ()
  (let ((result (mod (combo-operand) 8)))
    (push result comp-out)))

(defun micro-bdv ()
  (setq comp-regb (common-xdv)))

(defun micro-cdv ()
  (setq comp-regc (common-xdv)))

(defun puzzle-17a ()
  (load-program "data/input-17.txt")
  (while-let ((opcode (elt comp-prog comp-ip)))
    (cond
     ((eq opcode inst-adv) (micro-adv))
     ((eq opcode inst-bxl) (micro-bxl))
     ((eq opcode inst-bst) (micro-bst))
     ((eq opcode inst-jnz) (micro-jnz))
     ((eq opcode inst-bxc) (micro-bxc))
     ((eq opcode inst-out) (micro-out))
     ((eq opcode inst-bdv) (micro-bdv))
     ((eq opcode inst-cdv) (micro-cdv)))
    (if comp-jmp
	(setq comp-jmp nil)
      (setq comp-ip (+ comp-ip 2))))
  (let* ((numbers (nreverse comp-out))
	 (strings (mapcar #'number-to-string numbers)))
    (string-join strings ",")))

(defun reset-program (rega)
  (setq comp-rega rega)
  (setq comp-regb 0)
  (setq comp-regc 0)
  (setq comp-ip 0)
  (setq comp-out nil)
  (setq comp-jmp nil))

(defun prefix-match (a b)
  (cond ((and (car a) (eq (car a) (car b)))
	 (prefix-match (cdr a) (cdr b)))
	((and (null (car a)) (null (car b))) 'same)
	((null (car a)) t)))

(defun astray-p ()
  (let ((out (reverse comp-out)))
    (and out (not (prefix-match out comp-prog)))))

(defun quine-p ()
  (let ((out (reverse comp-out)))
    (eq (prefix-match out comp-prog) 'same)))

(defun step (rega)
  (reset-program rega)
  (while-let ((opcode (elt comp-prog comp-ip)))
    (cond
     ((eq opcode inst-adv) (micro-adv))
     ((eq opcode inst-bxl) (micro-bxl))
     ((eq opcode inst-bst) (micro-bst))
     ((eq opcode inst-bxc) (micro-bxc))
     ((eq opcode inst-out) (micro-out))
     ((eq opcode inst-bdv) (micro-bdv))
     ((eq opcode inst-cdv) (micro-cdv)))
    (setq comp-ip (+ comp-ip 2))))

(defun search (a rev-prog)
  (if (null rev-prog)
      a
    (let ((a (ash a 3)))
      (catch :found
	(dotimes (d 8)
	  (step (+ a d))
	  (when (eq (car comp-out) (car rev-prog))
	    (when-let* ((a (search (+ a d) (cdr rev-prog))))
	      (throw :found a))))))))

(defun puzzle-17b ()
  (load-program "data/input-17.txt")
  (search 0 (reverse comp-prog)))

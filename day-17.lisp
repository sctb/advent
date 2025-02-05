(defpackage :comp
  (:use :common-lisp))

(in-package :comp)

(defun read-entry (stream prefix)
  (let ((line (read-line stream nil)))
    (when (and line (> (length line) 0))
      (subseq line (length prefix)))))

(defun split-commas (string)
  (loop for i = 0 then (1+ j)
	as j = (position #\, string :start i)
	collect (subseq string i j)
	while j))

(defun comma-join (strings)
    (format nil "~{~A~^,~}" strings))

(defvar *register-a*)
(defvar *register-b*)
(defvar *register-c*)
(defvar *program*)

(defun load-program (file)
  (let ((in (open file)))
    (setq *register-a* (read-from-string (read-entry in "Register A: ")))
    (setq *register-b* (read-from-string (read-entry in "Register B: ")))
    (setq *register-c* (read-from-string (read-entry in "Register C: ")))
    (read-line in)
    (let* ((text (read-entry in "Program: "))
	   (strings (split-commas text))
	   (program (mapcar #'read-from-string strings)))
      (setq *program* program))
    (setq *output* nil)
    (setq *ip* 0)
    (setq *jmp* nil)))

(defvar *output*)
(defvar *ip*)
(defvar *jmp*)

(defun literal-operand ()
  (elt *program* (1+ *ip*)))

(defun combo-operand ()
  (let ((arg (literal-operand)))
    (cond ((< arg 4) arg)
	  ((= arg 4) *register-a*)
	  ((= arg 5) *register-b*)
	  ((= arg 6) *register-c*)
	  ((= arg 7) (error "Reserved combo operand")))))

(defvar *opcode-adv* 0)
(defvar *opcode-bxl* 1)
(defvar *opcode-bst* 2)
(defvar *opcode-jnz* 3)
(defvar *opcode-bxc* 4)
(defvar *opcode-out* 5)
(defvar *opcode-bdv* 6)
(defvar *opcode-cdv* 7)

(defun common-xdv ()
  (let* ((numerator *register-a*)
	 (denominator (expt 2 (combo-operand))))
    (truncate (/ numerator denominator))))

(defun micro-adv ()
  (setq *register-a* (common-xdv)))

(defun micro-bxl ()
  (let ((result (logxor *register-b* (literal-operand))))
    (setq *register-b* result)))

(defun micro-bst ()
  (let ((result (mod (combo-operand) 8)))
    (setq *register-b* result)))

(defun micro-jnz ()
  (unless (eq *register-a* 0)
    (setq *jmp* t)
    (setq *ip* (literal-operand))))

(defun micro-bxc ()
  (let ((result (logxor *register-b* *register-c*)))
    (setq *register-b* result)))

(defun micro-out ()
  (let ((result (mod (combo-operand) 8)))
    (push result *output*)))

(defun micro-bdv ()
  (setq *register-b* (common-xdv)))

(defun micro-cdv ()
  (setq *register-c* (common-xdv)))

(defun puzzle-17a ()
  (load-program "data/input-17.txt")
  (loop while (< *ip* (length *program*)) do
    (let ((opcode (elt *program* *ip*)))
      (cond
	((eq opcode *opcode-adv*) (micro-adv))
	((eq opcode *opcode-bxl*) (micro-bxl))
	((eq opcode *opcode-bst*) (micro-bst))
	((eq opcode *opcode-jnz*) (micro-jnz))
	((eq opcode *opcode-bxc*) (micro-bxc))
	((eq opcode *opcode-out*) (micro-out))
	((eq opcode *opcode-bdv*) (micro-bdv))
	((eq opcode *opcode-cdv*) (micro-cdv)))
      (if *jmp*
	  (setq *jmp* nil)
	  (setq *ip* (+ *ip* 2)))))
  (let* ((numbers (nreverse *output*))
	 (strings (mapcar #'princ-to-string numbers)))
    (comma-join strings)))

(defun reset-program (rega)
  (setq *register-a* rega)
  (setq *register-b* 0)
  (setq *register-c* 0)
  (setq *ip* 0)
  (setq *output* nil)
  (setq *jmp* nil))

(defun prefix-match (a b)
  (cond ((and (car a) (eq (car a) (car b)))
	 (prefix-match (cdr a) (cdr b)))
	((and (null (car a)) (null (car b))) 'same)
	((null (car a)) t)))

(defun astray-p ()
  (let ((out (reverse *output*)))
    (and out (not (prefix-match out *program*)))))

(defun quine-p ()
  (let ((out (reverse *output*)))
    (eq (prefix-match out *program*) 'same)))

(defun puzzle-17b ()
  (load-program "data/example-17b.txt")
  (let ((rega 0)
	(done nil))
    (loop while (and (< rega 10000000) (not done)) do
      (reset-program (setq rega (1+ rega)))
      (let ((limit 10000))
	(loop while (and (< *ip* (length *program*)) (> limit 0)) do
	  (let ((opcode (elt *program* *ip*)))
	    (cond
	      ((eq opcode *opcode-adv*) (micro-adv))
	      ((eq opcode *opcode-bxl*) (micro-bxl))
	      ((eq opcode *opcode-bst*) (micro-bst))
	      ((eq opcode *opcode-jnz*) (micro-jnz))
	      ((eq opcode *opcode-bxc*) (micro-bxc))
	      ((eq opcode *opcode-out*) (micro-out))
	      ((eq opcode *opcode-bdv*) (micro-bdv))
	      ((eq opcode *opcode-cdv*) (micro-cdv)))
	    (if *jmp*
		(setq *jmp* nil)
		(setq *ip* (+ *ip* 2)))
	    (if (astray-p)
		(setq limit 0)
		(setq limit (1- limit))))))
      (setq done (quine-p)))
    rega))

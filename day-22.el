;; -*- lexical-binding: t -*-

(defun read-line ()
  (buffer-substring (point) (line-end-position)))

(defun read-secrets (file)
  (let ((secrets nil))
    (with-temp-buffer
      (insert-file-contents file)
      (while (not (eobp))
	(push (read (read-line)) secrets)
	(forward-line)))
    (nreverse secrets)))

(defun mix (n secret)
  (logxor n secret))

(defun prune (secret)
  (mod secret 16777216))

(defun evolve-1 (secret)
  (let ((n (* secret 64)))
    (prune (mix n secret))))

(defun evolve-2 (secret)
  (let ((n (/ secret 32)))
    (prune (mix n secret))))

(defun evolve-3 (secret)
  (let ((n (* secret 2048)))
    (prune (mix n secret))))

(defun evolve (secret)
  (setq secret (evolve-1 secret))
  (setq secret (evolve-2 secret))
  (evolve-3 secret))

(defun next (secret times)
  (dotimes (_ times)
    (setq secret (evolve secret)))
  secret)

(defun puzzle-22a ()
  (let* ((file "data/input-22.txt")
	 (secrets (read-secrets file))
	 (sum 0))
    (dolist (secret secrets)
      (let ((n (next secret 2000)))
	(setq sum (+ sum n))))
    sum))

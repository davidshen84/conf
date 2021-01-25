;;; my.el --- Summary
;;; Commentary: My Lisp utility functions.
;; -*- coding: utf-8 -*-

;;; Code:

(defun my/dev-common ()
  "Common development settings."

  ;; make it `interactive' so it can be invoked anywhere
  (interactive)
  (editorconfig-mode t)
  (highlight-indentation-mode t)
  ;; (hs-minor-mode t)
  (linum-mode t)
  (eldoc-mode t))

(defun my/new-scratch-buffer ()
  "Create a new scratch buffer with a random name."
  (interactive)
  (switch-to-buffer (get-buffer-create (format "*scratch %X*" (random)))))

(defun my/duplicate-line ()
  "Duplicate current line."

  (interactive)
  (let ((begin (line-beginning-position))
        (end (line-end-position)))
    (forward-line)
    (if (= end (point)) (newline))
    (insert-buffer-substring (current-buffer) begin end))
  (newline)
  (forward-line -1)
  (beginning-of-line))

(provide 'my)

;;; my.el ends here

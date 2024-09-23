;;; my.el --- Summary
;;; Commentary: My Lisp utility functions.
;; -*- coding: utf-8 -*-

;;; Code:

(defun my/prog ()
  "Common development settings."

  ;; make it `interactive' so it can be invoked anywhere
  (interactive)
  (column-number-mode t)
  (display-line-numbers-mode t)
  (editorconfig-mode t)
  (eldoc-mode t)
  ;; (hs-minor-mode t)
  (indent-bars-mode t)
  (setq truncate-lines t))

(add-hook 'prog-mode-hook 'my/prog)

(defun my/new-scratch-buffer ()
  "Create a new scratch buffer with a random name."

  (interactive)
  (switch-to-buffer (get-buffer-create (format "*scratch %X*" (random))))
  (org-mode))

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

(defun my/previous-window ()
  "Move cursour to the previous window."

  (interactive)
  (other-window -1))

(defun my/password (user host)
  "Get the password using the `auth-source-search' function and
set the it in the clipboard. The clipboard is set to an empty
string in 10 sec."

  (interactive "suser: \nshost: ")
  (let ((search-args (unless (string= "" user)
                       (plist-put
                        (unless (string= "" host)
                          (plist-put '() :host host))
                        :user user))))
    (kill-new
     (let ((search-result (car
                           (apply #'auth-source-search
                                  search-args))))
       (cond (search-result (message "ok")
                            (funcall
                             (plist-get
                              search-result
                              :secret)))
             (t (message "%s@%s was not found" user host)
                "")))
     t))
  (run-at-time "10 sec" nil
               (lambda ()
                 (kill-new "" t))))

(provide 'my)

;;; my.el ends here

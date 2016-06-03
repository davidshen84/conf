;; -*- Emacs-Lisp -*-

;; load customize script
(add-to-list 'load-path "~/.emacs.d/lisp")

;; add melpa package
(progn
  (require 'package)
  (package-initialize)
  (add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t))

;; load theme
(cond ((display-graphic-p) (load-theme 'deeper-blue))
      ('t (load-theme 'manoj-dark)))

(defun new-scratch-buffer ()
  "create a new scratch buffer with a random name"
  (interactive)
  (switch-to-buffer (get-buffer-create (format "*scratch %X*" (random)))))

(defun duplicate-line ()
  "duplicate current line"
  (interactive)
  (let ((begin (line-beginning-position))
	(end (line-end-position)))

    (cond ((> (forward-line) 0) (newline)))
    (insert-buffer-substring (current-buffer) begin end))
  (newline)
  (previous-line)
  (beginning-of-line))

(defun dev-common ()
  "common development settings"

  ;; setup editorconfig
  (setq editorconfig-get-properties-function
        'editorconfig-core-get-properties-hash)
  ;; disable editorconfig for these major modes
  (setq editorconfig-exclude-modes
	'(emacs-lisp-mode lisp-interaction-mode json-mode))
  (editorconfig-mode t)

  (linum-mode t)
  (highlight-indentation-mode)
  (auto-complete-mode))

;; my key binding
(progn
  (global-set-key (kbd "C-c g") 'goto-line)
  (global-set-key (kbd "C-c l") 'linum-mode)
  (global-set-key (kbd "C-c b") 'whitespace-mode)
  (global-set-key (kbd "C-c c") 'comment-region)
  (global-set-key (kbd "C-c C") 'uncomment-region)
  (global-set-key (kbd "C-c M-c") 'comment)
  (global-set-key (kbd "C-c n") 'new-scratch-buffer)
  (global-set-key (kbd "C-c d") 'duplicate-line)
  )

;; some basic settings
(progn
  (add-to-list 'default-frame-alist '(fullscreen . maximized))
  (setq org-agenda-files '("~/notebook/agenda"))
  (setq safe-local-variable-values '((make-backup-files)))

  (ido-mode)
  (show-paren-mode t)

  (autoload 'dirtree "dirtree" "Add directory to tree view" t)
  ;; bind list buffer to ibuffer
  (defalias 'list-buffers 'ibuffer)

  ;; load ac
  (require 'auto-complete-config)
  (ac-config-default)
  ;; In your project root directory, do follow command to make tags file.
  ;; etags --verbose -R --fields="+afikKlmnsSzt"
  ;; (require 'auto-complete-exuberant-ctags)
  ;; (ac-exuberant-ctags-setup)
  )

;; for shell script
(add-hook 'sh-mode-hook
          '(lambda ()
	     (dev-common)))

;; for elisp
(add-hook 'emacs-lisp-mode-hook
          '(lambda ()
             (dev-common)))

;; for python
(add-hook 'python-mode-hook
          '(lambda ()
             (dev-common)
             (hs-minor-mode t)))

(add-hook 'pylint-mode-hook
          '(lambda ()
             (setq pylint-options '("--reports=n"))))

;; bind js to js3-mode
(add-to-list 'auto-mode-alist '("\\.js\\'" . js3-mode))
(add-hook 'js3-mode-hook
	  '(lambda ()
	     (dev-common)))

;; bind json to json-mode
(add-hook 'json-mode-hook
	  '(lambda ()
	     (dev-common)))

;; for css
(add-hook 'css-mode-hook
	  '(lambda ()
	     (dev-common)))

;; for markdown-mode
(autoload 'markdown-mode "markdown-mode"
  "Major mode for editing Markdown files" t)
;; bind file extension with mode
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))

;; for org-mod
(add-hook 'org-mode-hook
          '(lambda ()
             (auto-fill-mode t)
	     (setq org-log-done 'time)
             (org-babel-do-load-languages 'org-babel-load-languages
					  '((python . t)
					    (sh . t)
					    ;; add more languages
					    ))))

;; for LaTeX
(add-hook 'LaTeX-mode-hook
          '(lambda ()
             (auto-complete-mode t)))

;; for html
;; bind file extension to web-mode
(add-to-list 'auto-mode-alist '("\\.html\\'" . web-mode))

;; for cuda
(add-hook 'cuda-mode-hook
	  '(lambda ()
	     (dev-common)))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(menu-bar-mode nil)
 '(tool-bar-mode nil))

(cond ((display-graphic-p)
    (custom-set-faces
     ;; custom-set-faces was added by Custom.
     ;; If you edit it by hand, you could mess it up, so be careful.
     ;; Your init file should contain only one such instance.
     ;; If there is more than one, they won't work right.
     '(default ((t (:inherit nil :weight normal :height 180 :width normal :foundry "outline" :family "Source Code Pro")))))))

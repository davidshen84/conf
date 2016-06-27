;; -*- coding: utf-8 -*-

;; load custom scripts
(add-to-list 'load-path "~/.emacs.d/lisp")

;; setup elpa package source
(require 'package)
(package-initialize)
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t)

;; load theme
(if (display-graphic-p) (load-theme 'deeper-blue)
  (load-theme 'manoj-dark))

(defun new-scratch-buffer ()
  "Create a new scratch buffer with a random name."
  (interactive)
  (switch-to-buffer (get-buffer-create (format "*scratch %X*" (random)))))

(defun duplicate-line ()
  "Duplicate current line."
  (interactive)
  (let ((begin (line-beginning-position))
        (end (line-end-position)))

    (if (> (forward-line) 0) (newline))
    (insert-buffer-substring (current-buffer) begin end))
  (newline)
  (previous-line)
  (beginning-of-line))

(defun dev-common ()
  "Common development settings."

  ;; make it `interactive' so it can be invoked anywhere
  (interactive)

  ;; setup editorconfig
  (setq editorconfig-get-properties-function
        'editorconfig-core-get-properties-hash)
  ;; disable editorconfig for some major modes
  (setq editorconfig-exclude-modes
        '(emacs-lisp-mode lisp-mode json-mode))
  (editorconfig-mode 1)

  (linum-mode 1)
  (highlight-indentation-mode 1)
  (auto-complete-mode 1))

;; some basic settings
(require 'org)
(add-to-list 'default-frame-alist '(fullscreen . maximized))
(autoload 'dirtree "dirtree" "Add directory to tree view" t)
(ido-mode 1)
(show-paren-mode 1)
(delete-selection-mode 1)

;; my key binding
(global-set-key (kbd "C-c g") 'goto-line)
(global-set-key (kbd "C-c l") 'linum-mode)
(global-set-key (kbd "C-c b") 'whitespace-mode)
(global-set-key (kbd "C-c /") 'comment-region)
(global-set-key (kbd "C-c M-/") 'uncomment-region)
(global-set-key (kbd "C-c n") 'new-scratch-buffer)
(global-set-key (kbd "C-c d") 'duplicate-line)
(global-set-key (kbd "C-c c") 'org-capture)

;; bind list buffer to ibuffer
(defalias 'list-buffers 'ibuffer)

;; require ac
(require 'auto-complete-config)
(ac-config-default)
;; (require 'auto-complete-exuberant-ctags)
;; (ac-exuberant-ctags-setup)

;; start emacs server
(server-start)

;; for shell script
(add-hook 'sh-mode-hook
          '(lambda ()
             (dev-common)))

;; for lisp
(add-hook 'emacs-lisp-mode-hook
          '(lambda ()
             (dev-common)
             (setq indent-tabs-mode nil)))

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

;; for org-mod
(require 'org-notify)
;; (org-notify-start)
(setq org-default-notes-file (concat org-directory "/.notes"))
(setq org-capture-templates
      '(("t" "Task" entry (file+headline nil "Tasks")
         "* TODO %? %^g")
        ("q" "Quick note" entry (file+headline nil "Quick Notes")
         "* On %t %^g \n  %i%?")))

(add-hook 'org-mode-hook
          '(lambda ()
             (auto-fill-mode t)
             (setq org-log-done 'time)
             (org-babel-do-load-languages 'org-babel-load-languages
                                          '((python . t)
                                            (sh . t)
                                            (sql . t)
                                            ;; add more languages
                                            ))))

;; for LaTeX
(add-hook 'LaTeX-mode-hook
          '(lambda ()
             (auto-complete-mode 1)))

;; for html
(add-to-list 'auto-mode-alist '("\\.html\\'" . web-mode))

;; for cuda
(add-hook 'cuda-mode-hook
          '(lambda ()
             (dev-common)))

;; for c/c++
(mapc '(lambda (hook)
         (add-hook hook
                   '(lambda ()
                      (require 'clang-format)
                      (setq clang-format-style "Google"))))
      '(c-mode-hook c++-mode-hook))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(menu-bar-mode nil)
 '(org-agenda-files '("~/org/agenda"))
 '(org-src-fontify-natively t)
 '(safe-local-variable-values '((make-backup-files)))
 '(tool-bar-mode nil))

(if (display-graphic-p)
    (custom-set-faces
     ;; custom-set-faces was added by Custom.
     ;; If you edit it by hand, you could mess it up, so be careful.
     ;; Your init file should contain only one such instance.
     ;; If there is more than one, they won't work right.
     '(default ((t (:inherit nil :weight normal :height 180 :width normal :foundry "outline" :family "Source Code Pro"))))))


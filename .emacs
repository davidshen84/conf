;;; .emacs --- Summary
;;; Commentary:
;; -*- coding: utf-8 -*-

;;; Code:

;; load custom scripts
(add-to-list 'load-path "~/.emacs.d/lisp")

;; setup elpa package source
(require 'package)
(package-initialize)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t)

;; load theme
(if (display-graphic-p)
    ; (load-theme 'deeper-blue)
    (require 'dracula-theme)
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

    (forward-line)
    (if (= end (point)) (newline))
    (insert-buffer-substring (current-buffer) begin end))
  (newline)
  (forward-line -1)
  (beginning-of-line))

(defun dev-common ()
  "Common development settings."

  ;; make it `interactive' so it can be invoked anywhere
  (interactive)
  (editorconfig-mode t)
  (linum-mode t)
  (highlight-indentation-mode t)
  (auto-complete-mode t)
  (hs-minor-mode t))

;; some basic settings
(require 'dirtree)
(add-to-list 'default-frame-alist '(fullscreen . maximized))
(ido-mode t)
(show-paren-mode t)
(delete-selection-mode t)
(global-flycheck-mode)

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
(require 'auto-complete)
(require 'auto-complete-config)
(eval-after-load "etags"
  '(progn
     (ac-etags-setup)))

;; start emacs server
(server-start)

;; for shell script
(add-hook 'sh-mode-hook
          #'(lambda ()
              (dev-common)))

;; for lisp
(add-hook 'emacs-lisp-mode-hook
          #'(lambda ()
              (dev-common)
              (setq indent-tabs-mode nil)))

;; for python
(add-hook 'python-mode-hook
          #'(lambda ()
              (dev-common)
              (hs-minor-mode t)))

;; bind js to js3-mode
(add-to-list 'auto-mode-alist '("\\.js\\'" . js3-mode))
(add-hook 'js3-mode-hook
          #'(lambda ()
              (dev-common)))

;; bind json to json-mode
(add-hook 'json-mode-hook
          #'(lambda ()
              (dev-common)))

;; for css
(add-hook 'css-mode-hook
          #'(lambda ()
              (dev-common)))

;; for org-mod
(require 'org)
(require 'org-notify)
;; (org-notify-start)
(setq org-default-notes-file (concat org-directory "/.notes"))

(add-hook 'org-mode-hook
          #'(lambda ()
              (auto-fill-mode t)
              (setq org-log-done 'time)
              (org-babel-do-load-languages 'org-babel-load-languages
                                           '((python . t)
                                             (sh . t)
                                             (shell . t)
                                             (sql . t)
                                             ;; add more languages
                                             ))))

;; for LaTeX
(add-hook 'LaTeX-mode-hook
          #'(lambda ()
              (auto-complete-mode t)))

;; for html
(add-to-list 'auto-mode-alist '("\\.html\\'" . web-mode))

;; for cuda
(add-hook 'cuda-mode-hook
          #'(lambda ()
              (dev-common)))

;; for c/c++
(require 'clang-format)
(setq clang-format-style "Google")
(mapc #'(lambda (hook)
          (add-hook hook
                    #'(lambda ()
                        (dev-common)
                        (ac-etags-ac-setup)
                        )))
      '(c-mode-hook c++-mode-hook))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(editorconfig-exclude-modes (quote (emacs-lisp-mode lisp-mode json-mode)))
 '(editorconfig-get-properties-function (quote editorconfig-core-get-properties-hash))
 '(editorconfig-mode t)
 '(erc-nick "davidshen84")
 '(eshell-path-env (mapconcat 'identity `("/usr/local/bin", eshell-path-env) ":"))
 '(flycheck-clang-language-standard "c++11")
 '(menu-bar-mode nil)
 '(org-agenda-files (list org-default-notes-file "~/org/agenda"))
 '(org-capture-templates
   (quote
    (("t" "Task" entry (file+headline nil "Tasks") "* TODO %? %^g")
     ("q" "Quick note" entry (file+headline nil "Quick Notes") "* On %t %^g %i%?"))))
 '(org-src-fontify-natively t)
 '(package-selected-packages
   (quote
    (;; sorted alphabetically
     auto-complete
     clang-format
     cmake-mode
     dirtree
     docker
     dockerfile-mode
     dracula-theme
     editorconfig
     flycheck-pyflakes
     groovy-mode
     highlight-indentation
     js2-mode
     json-mode
     magit
     markdown-mode
     markdown-preview-mode
     yaml-mode)))
 '(safe-local-variable-values (quote ((make-backup-files))))
 '(tool-bar-mode nil))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 (if (display-graphic-p)
     '(default ((t (:inherit nil :weight normal :height 180 :width normal :foundry "outline" :family "Source Code Pro"))))))

(provide '.emacs)
;;; .emacs ends here

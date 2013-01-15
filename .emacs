;; -*- Emacs-Lisp -*-

(dolist (p '("~/.emacs.d"
             ;"~/.emacs.d/color-theme"
             "~/.emacs.d/org/lisp"))
  (add-to-list 'load-path p))

; load theme
(load-theme 'deeper-blue 1)

;; some basic default settings
(progn
  ;; set tab mode
  (setq-default  tab-width 4
                 indent-tabs-mode nil
                 indent-line-function 'insert-tab)
  ;; disable tool bar
  (tool-bar-mode -1)
  ;; bind goto line function with Ctrl-c-g
  (global-set-key (kbd "C-c g") 'goto-line)
  (global-set-key (kbd "C-c t") 'set-color-theme))

;; load highlight indentation
(require 'highlight-indentation)

;; load orgmod
(require 'org-install)

;; for shell script
(add-hook 'sh-mode-hook
          '(lambda ()
             (setq sh-basic-offset 2)
             (linum-mode)
             (set-color-theme)))

;; for elisp
(add-hook 'emacs-lisp-mode-hook
          '(lambda ()
             (show-paren-mode t)
             (linum-mode t)
             (highlight-indentation-mode)))

;; for python
(add-hook 'python-mode-hook
          '(lambda ()
             (linum-mode t)
             (setq python-indent 2)
             ;; (whitespace-mode t)
             (highlight-indentation-mode)))

;; for orgmod
(add-hook 'org-mode-hook
          '(lambda ()
             (auto-fill-mode t)))


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
)

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "courier" :foundry "adobe" :slant normal :weight normal :height 180 :width normal)))))

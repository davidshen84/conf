;; -*- Emacs-Lisp -*-

(dolist (p '("~/.emacs.d"
             "~/.emacs.d/org/lisp"
             "~/.emacs.d/auto-complete"
             "~/.emacs.d/evil"))
  (add-to-list 'load-path p))

;; load theme
(if (display-graphic-p)
    (load-theme 'deeper-blue 1)
  (load-theme 'manoj-dark 1))

;; my key binding
(progn
  ;; bind goto line function with Ctrl-c-g
  (global-set-key (kbd "C-c g") 'goto-line)
  (global-set-key (kbd "C-c l") 'linum-mode)
  (global-set-key (kbd "C-c b") 'whitespace-mode))

;; some basic default settings
(progn
  ;; set tab offset
  (setq-default tab-width 4
                indent-tabs-mode nil
                indent-line-function 'insert-tab)
  (ido-mode)
  ;; bind list buffer to ibuffer
  (defalias 'list-buffers 'ibuffer))

;; load ac
(progn
  (require 'auto-complete-config)
  (add-to-list 'ac-dictionary-directories "~/.emacs.d/ac-dict")
  (ac-config-default))

;; load highlight indentation
(require 'highlight-indentation)

(defun dev-basic ()
  (linum-mode t)
  (highlight-indentation-mode))

;; for shell script
(add-hook 'sh-mode-hook
          '(lambda ()
             (dev-basic)
             (setq sh-basic-offset 2)))

;; for elisp
(add-hook 'emacs-lisp-mode-hook
          '(lambda ()
             (dev-basic)))

;; for python
(add-hook 'python-mode-hook
          '(lambda ()
             (dev-basic)
             (setq python-indent 2)))

;; for js
(progn
  ;; bind js/json to js2-mode
  (add-to-list 'auto-mode-alist
               '("\\(\\.js\\|\\.json\\)\\'" . js2-mode))
  (add-hook 'js2-mode-hook
            '(lambda ()
               (dev-basic)
               (setq js2-basic-offset 2))))

;; load orgmod
(progn
  (require 'org-install)
  (add-hook 'org-mode-hook
            '(lambda ()
               (auto-fill-mode t))))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(show-paren-mode t)
 '(tool-bar-mode nil)
 '(menu-bar-mode nil))

(if (display-graphic-p)
    (custom-set-faces
     ;; custom-set-faces was added by Custom.
     ;; If you edit it by hand, you could mess it up, so be careful.
     ;; Your init file should contain only one such instance.
     ;; If there is more than one, they won't work right.
     '(default ((t (:family "Source Code Pro" :weight normal :height 180 :width normal))))))


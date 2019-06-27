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
(add-to-list 'package-archives '("org" . "https://orgmode.org/elpa/") t)

;; load theme
(if (display-graphic-p)
    ;; (load-theme 'deeper-blue)
    (progn (require 'flatui-dark-theme)
           (set-face-attribute 'default nil
                               :family "Source Code Pro"
                               :foundry 'outline
                               :height 180
                               :inherit nil
                               :weight 'normal))
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
  (company-mode t)
  (editorconfig-mode t)
  (highlight-indentation-mode t)
  (hs-minor-mode t)
  (linum-mode t)
  (flycheck-mode t))

;; my key binding
(global-set-key (kbd "C-c g") 'goto-line)
(global-set-key (kbd "C-c l") 'linum-mode)
(global-set-key (kbd "C-c b") 'whitespace-mode)
(global-set-key (kbd "C-c /") 'comment-region)
(global-set-key (kbd "C-c M-/") 'uncomment-region)
(global-set-key (kbd "C-c n") 'new-scratch-buffer)
(global-set-key (kbd "C-c d") 'duplicate-line)
(global-set-key (kbd "C-c c") 'org-capture)

(add-hook 'after-init-hook
          #'(lambda ()
              (require 'dirtree)
              (require 'iedit)

              (ido-mode t)
              (show-paren-mode t)
              (delete-selection-mode t)

              ;; start emacs server
              (server-start)

              ;; bind list buffer to ibuffer
              (defalias 'list-buffers 'ibuffer)

               ;; maximize emacs
              (setq initial-frame-alist '((fullscreen . maximized)))
              (menu-bar-mode -1)
              (scroll-bar-mode -1)
              (tool-bar-mode -1)))

(declare-function ibuffer-switch-to-saved-filter-groups "ibuf-ext.el" (name))
;; ibuffer settings
(setq ibuffer-saved-filter-groups '(("default"
                                     ("magit" (name . "magit")))))
(add-hook 'ibuffer-mode-hook
          #'(lambda ()
              (ibuffer-switch-to-saved-filter-groups "default")))

;; for shell script
(add-hook 'sh-mode-hook 'dev-common)

;; for lisp
(add-hook 'emacs-lisp-mode-hook
          #'(lambda ()
              (dev-common)
              (setq indent-tabs-mode nil)))


;; for python
(add-hook 'python-mode-hook
          #'(lambda ()
              (dev-common)
              (add-to-list 'company-backends 'company-jedi)))

;; for js
(add-to-list 'auto-mode-alist '("\\.js\\'" . js3-mode))
(mapc #'(lambda (hook)
          (add-hook hook 'dev-common))
      '(js3-mode-hook json-mode-hook))

;; for css
(add-hook 'css-mode-hook 'dev-common)

;; for org-mod
(require 'org)
;; (require 'org-notify)
(setq org-capture-templates
 '(("t" "Todo" entry (file+headline "~/org/todo.org" "Tasks")
        "* TODO %?\n  %i\n  %a")
   ("j" "Journal" entry (file+datetree "~/org/journal.org" "Journal")
    "* %?\nEntered on %U\n  %i\n  %a")))
(setq org-agenda-files (list "~/org/todo.org"))
(setq org-log-done 'time)
(setq org-src-fontify-natively t)
(add-hook 'org-mode-hook
          #'(lambda ()
              (auto-fill-mode t)
              (org-babel-do-load-languages 'org-babel-load-languages
                                           '((python . t)
                                             (shell . t)
                                             (sql . t)
                                             ;; add more languages
                                             ))))

;; for LaTeX
; (add-hook 'LaTeX-mode-hook 'company-mode)

;; for html
(add-to-list 'auto-mode-alist '("\\.html\\'" . web-mode))

;; for cuda
; (add-hook 'cuda-mode-hook 'dev-common)

;; for xml

(add-hook 'nxml-mode-hook
          #'(lambda ()
              (dev-common)
              (require 'sgml-mode)))
(add-to-list 'hs-special-modes-alist
             '(nxml-mode
               "<!--\\|<[^/>]*[^/]>"
               "-->\\|</[^/>]*[^/]>"

               "<!--"
               sgml-skip-tag-forward
               nil))

;; for c/c++
;; (mapc #'(lambda (hook)
;;           (add-hook hook
;;                     #'(lambda ()
;;                         (dev-common)
;;                         (require 'clang-format)
;;                         (setq clang-format-style "Google")
;;                         ;; flycheck
;;                         (setq flycheck-clang-language-standard "c++11"))))
;;       '(c-mode-hook c++-mode-hook))

;; editorconfig settings
(setq editorconfig-exclude-modes (quote (emacs-lisp-mode lisp-mode json-mode)))
(setq editorconfig-get-properties-function 'editorconfig-core-get-properties-hash)

;; erc settings
(setq erc-nick "davidshen84")

;; eshell settings
(require 'esh-autosuggest)
(add-hook 'eshell-mode-hook
          #'(lambda ()
              (esh-autosuggest-mode t)
              ;; Needs to set everytime entering the mode.
              (setq eshell-path-env
                    (mapconcat 'identity
                               `("C:\\Program Files\\Git\\usr\\bin\\",
                                 eshell-path-env)
                               ";"))))

(add-hook 'eshell-load-hook
          #'(lambda ()
              ;; Only needs to set once.
              (setenv "PATH"
                      (mapconcat 'identity
                                 `("C:\\Program Files\\Git\\usr\\bin\\",
                                   (getenv "PATH"))
                                 ";"))))

;; for TypeScript
;; (add-hook 'typescript-mode-hook
;;           #'(lambda ()
;;               (dev-common)
;;               (tide-setup)
;;               (eldoc-mode t)))

 ;; package settings
(setq package-selected-packages
      '(;; sorted alphabetically
        clang-format
        cmake-mode
        company
        company-jedi
        dirtree
        docker
        dockerfile-mode
        dracula-theme
        editorconfig
        esh-autosuggest
        flatui-dark-theme
        flycheck-pyflakes
        groovy-mode
        highlight-indentation
        iedit
        js3-mode
        json-mode
        magit
        markdown-mode
        markdown-preview-mode
        org-plus-contrib
        tide
        typescript-mode
        web-mode
        yaml-mode
        ))

;; local-variable settings
(setq safe-local-variable-values '((make-backup-files)))

(provide '.emacs)

;; Local Variables:
;; byte-compile-warnings: (not free-vars)
;; End:

;;; .emacs ends here

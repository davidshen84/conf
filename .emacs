;;; .emacs --- Summary
;;; Commentary:
;; -*- coding: utf-8 -*-

;;; Code:

;; load custom scripts
(eval-when-compile
  (add-to-list 'load-path "~/github/conf/lisp")
  ;; setup elpa package source
  (require 'package)
  (package-initialize)
  (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
  (add-to-list 'package-archives '("org" . "https://orgmode.org/elpa/") t)
  (require 'use-package)
  (require 'my))

(use-package use-package-ensure-system-package
  :ensure t)

(use-package dirtree
  :ensure t)

(use-package dracula-theme)
(use-package flatui-dark-theme)

;; my key binding
(global-set-key (kbd "C-c g") 'goto-line)
(global-set-key (kbd "C-c l") 'linum-mode)
(global-set-key (kbd "C-c b") 'whitespace-mode)
;; (global-set-key (kbd "C-c /") 'comment-region)
;; (global-set-key (kbd "C-c M-/") 'uncomment-region)
(global-set-key (kbd "C-c n") 'my/new-scratch-buffer)
(global-set-key (kbd "C-c d") 'my/duplicate-line)
(global-set-key (kbd "S-C-<left>") 'shrink-window-horizontally)
(global-set-key (kbd "S-C-<right>") 'enlarge-window-horizontally)
(global-set-key (kbd "C-x O") (lambda ()
                                (interactive)
                                (other-window -1)))

;; for Windows environment
;; update Emacs' execution path to be the same as Windows'.
;; (if (string-equal system-type "windows-nt")
;;     (progn (setenv "PATH"
;;                    (mapconcat 'identity
;;                               `("c:\\windows",
;;                                 (getenv "PATH"))
;;                               ";"))
;;            (setq exec-path (split-string
;;                             (replace-regexp-in-string "\\\\" "/" (getenv "PATH"))
;;                             ";"))))

;; magit settings
(use-package magit
  :ensure t
  :custom
  (ibuffer-saved-filter-groups '(("default"
                                  ("magit" (name . "magit")))))
  :bind (:map global-map
              ("C-x g" . magit-status)))

;; ibuffer settings
(add-hook 'ibuffer-mode-hook
          #'(lambda ()
              (ibuffer-switch-to-saved-filter-groups "default")))

;; for shell script
;; (add-hook 'sh-mode-hook 'my/dev-common)

;; for lisp
(add-hook 'emacs-lisp-mode-hook
          #'(lambda ()
              (my/dev-common)
              (setq indent-tabs-mode nil)))

;; for python
(use-package python-mode
  :ensure t
  :hook my/dev-common)

(use-package ob-http
  :ensure t)

;; for org-mod
(use-package org-plus-contrib
  :ensure t
  :bind (("C-c c" . org-capture))
  :init
  (require 'org-tempo)
  :config
  (org-crypt-use-before-save-magic)
  :custom
  (org-capture-templates
   '(("t" "Todo" entry (file+headline "~/org/todo.org" "Tasks")
      "* TODO %?\n  %i\n  %a")
     ("j" "Journal" entry (file+olp+datetree "~/org/journal.org")
      "* %?\n")))
  (org-agenda-files (list "~/org/todo.org"))
  (org-log-done 'time)
  (org-src-fontify-natively t)
  (org-confirm-babel-evaluate nil)
  (org-babel-load-languages
   '((python . t)
     (shell . t)
     (sql . t)
     (lisp . t)
     (emacs-lisp . t)
     (http . t)
     ;; add more languages
     ))
  (auto-fill-mode t)
  ;; set this value to a real gpg key to use asymmetric encryption
  (org-crypt-key nil))

;; for LaTeX
;; (add-hook 'LaTeX-mode-hook 'company-mode)

;; for html
(use-package web-mode
  :ensure t
  :mode (("\\.html\\'" . web-mode))
  :hook my/dev-common)

;; for xml
(add-hook 'nxml-mode-hook
          #'(lambda ()
              (my/dev-common)
              (require 'sgml-mode)))
(add-to-list 'hs-special-modes-alist
             '(nxml-mode
               "<!--\\|<[^/>]*[^/]>"
               "-->\\|</[^/>]*[^/]>"

               "<!--"
               sgml-skip-tag-forward
               nil))

;; for c/c++
;; (use-package clang-format)
;; (use-package cmake-mode)
;; (mapc #'(lambda (hook)
;;           (add-hook hook
;;                     #'(lambda ()
;;                         (my/dev-common)
;;                         (require 'clang-format)
;;                         (setq clang-format-style "Google")
;;                         ;; flycheck
;;                         (setq flycheck-clang-language-standard "c++11"))))
;;       '(c-mode-hook c++-mode-hook))

;; editorconfig settings
(use-package editorconfig
  :ensure t
  :custom
  (editorconfig-exclude-modes '(emacs-lisp-mode lisp-mode))
  (editorconfig-get-properties-function 'editorconfig-core-get-properties-hash))

;; erc settings
(use-package erc
  :ensure t
  :custom
  (erc-nick "davidshen84")
  (erc-prompt-for-password nil))

;; for TypeScript
(use-package typescript-mode
  :ensure t
  :mode (("\\.tsx\\'" . typescript-mode))
  :custom
  (typescript-indent-level 2)
  (css-indent-offset 2)
  :hook (((lambda ()
            (setq flycheck-javascript-eslint-executable (string-trim (shell-command-to-string "npx which eslint")))
            (lsp-deferred)
            (my/dev-common)))))

(use-package lsp-mode
  :bind-keymap ("C-c C-l" . lsp-command-map)
  :commands (lsp lsp-deferred)

  :config
  (use-package lsp-ui
    :ensure t
    :commands lsp-ui-mode)

  (use-package lsp-origami
  :ensure t
  :hook (lsp-after-open . #'lsp-origami-try-enable))

  (use-package lsp-treemacs
  :ensure t
  :commands lsp-treemacs-errors-list)

(use-package helm-lsp
  :ensure t
  :bind ([remap xref-find-apropos] . helm-lsp-workspace-symbol)
  :commands (helm-lsp-workspace-symbol))

  (my/dev-common)
  (lsp-enable-which-key-integration t)
  (lsp-origami-mode t)

  :custom
  (lsp-enable-snippet nil)
  (lsp-eslint-server-command
           '("node"
             "/path/to/local/eslintServer.js"
             "--stdio")))

(use-package origami
  :bind (:map origami-mode-map
              ("C--" . origami-close-node)
              ("C-M--" . origami-open-node)))

(use-package which-key
  :ensure t
  :config (which-key-mode))

;; for js/json
;; (use-package js3-mode
;;   :ensure t
;;   :mode "\\.js\\'"
;;   :interpreter "js3"
;;   :hook (js3-mode . my/dev-common))
;; (use-package js2-mode
;;   :ensure t
;;   :mode "\\.js\\'"
;;   :interpreter "js2"
;;   :hook (js2-mode . (lambda ()
;;                       (my/dev-common)
;;                       (lsp)
;;                       )))

(use-package json-mode
  :ensure t
  :mode "\\.json\\'"
  :interpreter "json"
  :custom
  (js-indent-level 2)
  :hook my/dev-common)

(use-package company
  :config
  (use-package helm-company
    :ensure t
    :bind (("M-x" . helm-M-x)
           ("C-x C-f" . helm-find-files)
           :map company-mode-map
           ("C-:" . helm-company)
           :map company-active-map
           ("C-:" . helm-company)))
  (global-company-mode))

(use-package iedit
  :ensure t)

(use-package esh-autosuggest
  :ensure t
  :config
  (add-to-list 'company-backends 'esh-autosuggest))

;; for projectile
(use-package projectile
  :ensure t
  :bind-keymap ("C-c p" . projectile-command-map)
  :custom (projectile-switch-project-action #'projectile-dired))

(use-package ag
  :ensure t)

(use-package dirtree
  :ensure t)
(use-package docker
  :ensure t
  :config
  (use-package dockerfile-mode
    :ensure t))

(use-package eslint-fix
  :ensure t)
(use-package flycheck
  :ensure t
  :config
  ;; (global-flycheck-mode)
  (flycheck-add-mode 'javascript-eslint 'typescript-mode))

(use-package highlight-indentation
  :ensure t)
(use-package markdown-mode
  :ensure t
  :config
  (use-package markdown-preview-mode
    :ensure t))

(use-package yaml-mode
  :ensure t)

(use-package treemacs
  :ensure t
  :defer t
  :init
  (with-eval-after-load 'winum
    (define-key winum-keymap (kbd "M-0") #'treemacs-select-window))
  :config
  (setq treemacs-collapse-dirs                 (if (treemacs--find-python3) 3 0)
        treemacs-deferred-git-apply-delay      0.5
        treemacs-display-in-side-window        t
        treemacs-eldoc-display                 t
        treemacs-file-event-delay              5000
        treemacs-file-follow-delay             0.2
        treemacs-follow-after-init             t
        treemacs-git-command-pipe              ""
        treemacs-goto-tag-strategy             'refetch-index
        treemacs-indentation                   2
        treemacs-indentation-string            " "
        treemacs-is-never-other-window         nil
        treemacs-max-git-entries               5000
        treemacs-missing-project-action        'ask
        treemacs-no-png-images                 nil
        treemacs-no-delete-other-windows       t
        treemacs-project-follow-cleanup        nil
        treemacs-persist-file                  (expand-file-name ".cache/treemacs-persist" user-emacs-directory)
        treemacs-position                      'left
        treemacs-recenter-distance             0.1
        treemacs-recenter-after-file-follow    nil
        treemacs-recenter-after-tag-follow     nil
        treemacs-recenter-after-project-jump   'always
        treemacs-recenter-after-project-expand 'on-distance
        treemacs-show-cursor                   nil
        treemacs-show-hidden-files             t
        treemacs-silent-filewatch              nil
        treemacs-silent-refresh                nil
        treemacs-sorting                       'alphabetic-desc
        treemacs-space-between-root-nodes      t
        treemacs-tag-follow-cleanup            t
        treemacs-tag-follow-delay              1.5
        treemacs-width                         35)

  ;; The default width and height of the icons is 22 pixels. If you are
  ;; using a Hi-DPI display, uncomment this to double the icon size.
  ;;(treemacs-resize-icons 44)

  (pcase (cons (not (null (executable-find "git")))
               (not (null (treemacs--find-python3))))
    (`(t . t)
     (treemacs-git-mode 'deferred))
    (`(t . _)
     (treemacs-git-mode 'simple)))

  (use-package treemacs-projectile
    :after treemacs projectile
    :ensure t)

  (use-package treemacs-icons-dired
    :after treemacs dired
    :ensure t
    :config (treemacs-icons-dired-mode))

  (use-package treemacs-magit
    :after treemacs magit
    :ensure t)

  :bind (:map global-map
              ("M-0"       . treemacs-select-window)
              ("C-x t 1"   . treemacs-delete-other-windows)
              ("C-x t t"   . treemacs)
              ("C-x t B"   . treemacs-bookmark)
              ("C-x t C-t" . treemacs-find-file)
              ("C-x t M-t" . treemacs-find-tag))

  :hook (projectile-mode))

(use-package pinentry
  :ensure t
  :custom
  (epa-pinentry-mode 'loopback))

(use-package ediff
  :custom
  (ediff-split-window-function 'split-window-horizontally)
  (ediff-window-setup-function 'ediff-setup-windows-plain))

;; init.
(add-hook 'after-init-hook
          #'(lambda ()
              (ido-mode t)
              (show-paren-mode t)
              (delete-selection-mode t)


              ;; bind list buffer to ibuffer
              (defalias 'list-buffers 'ibuffer)

              ;; set window style
              (menu-bar-mode -1)
              (scroll-bar-mode -1)
              (tool-bar-mode -1)

              ;; set font face
              (set-face-attribute 'default nil
                                  :family "Source Code Pro"
                                  :height 180
                                  :inherit nil
                                  :weight 'normal)

              (setq-default
               initial-frame-alist '((fullscreen . maximized))
               indent-tabs-mode nil
               default-terminal-coding-system 'utf-8
               select-active-regions nil)

              ;; load theme
              (if (display-graphic-p)
                   (load-theme 'flatui-dark nil)
              ;; (load-theme 'dracula nil)
                )

              ;; start emacs server
              (server-start)
              (pinentry-start)
              ))

;; modern grep setting
(require 'grep)
(grep-apply-setting 'grep-use-null-device nil)
(setq grep-find-command "find . -type f -exec grep -nHi \"{}\" \";\"")

(use-package pyim
  :ensure t
  :config
  (use-package pyim-basedict
    :config
    (pyim-basedict-enable))
  (setq default-input-method "pyim")
  (setq pyim-default-scheme "quanpin"))

(provide '.emacs)


;; Local Variables:
;; byte-compile-warnings: (not free-vars noruntime)
;; End:

;;; .emacs ends here

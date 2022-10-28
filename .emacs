;;; .emacs --- Summary
;;; Commentary:
;; -*- coding: utf-8 -*-

;;; Code:

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
                                  :font "Cascadia Code"
                                  :height 160
                                  :inherit nil
                                  :weight 'normal)

              (setq-default
               initial-frame-alist '((fullscreen . maximized))
               indent-tabs-mode nil
               default-terminal-coding-system 'utf-8
               select-active-regions nil)

              ;; start emacs server
              (server-start)
              (pinentry-start)))


;; setup elpa package source
(require 'package)
(package-initialize)
(add-to-list 'package-archives '("elpa" . "https://elpa.gnu.org/packages/") t)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)

(require 'use-package)
(use-package use-package-ensure-system-package
  :ensure t)

(use-package my
  :load-path "~/github/conf/lisp"
  :demand t
  :bind (:map global-map
              ("C-c n" . #'my/new-scratch-buffer)
              ("C-c d" . #'my/duplicate-line)
              ("C-x O" . #'my/previous-window)
              ("C-x M-o" . ace-select-window)))

(use-package dirtree
  :ensure t)

;; (use-package material-theme
;;   :config
;;   (enable-theme 'material))

(use-package solarized-theme
  :ensure t
  :custom
  (solarized-use-variable-pitch nil)
  :config
  (load-theme 'solarized-dark t))

;; my key binding
(global-set-key (kbd "C-c g") 'goto-line)
(global-set-key (kbd "C-c l") 'linum-mode)
(global-set-key (kbd "C-c b") 'whitespace-mode)
;; (global-set-key (kbd "C-c /") 'comment-region)
;; (global-set-key (kbd "C-c M-/") 'uncomment-region)
(global-set-key (kbd "S-C-<left>") 'shrink-window-horizontally)
(global-set-key (kbd "S-C-<right>") 'enlarge-window-horizontally)

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

;; customize emacs path
(setq exec-path (append exec-path '("~/.local/bin")))

;; magit settings
(use-package magit
  :ensure t
  :bind (:map global-map
              ("C-x g" . magit-status)))

;; ibuffer settings
(add-hook 'ibuffer-mode-hook
          #'(lambda ()
              (ibuffer-switch-to-saved-filter-groups "default")))
(setq ibuffer-saved-filter-groups '(("default"
                                     ("magit" (name . "magit"))
                                     ("erc" (mode . erc-mode)))))

;; for lisp
(add-hook 'emacs-lisp-mode-hook
          #'(lambda ()
              (setq indent-tabs-mode nil)))

;; for python
(use-package python-mode
  :ensure t)

;; EasyPG
(require 'epa-file)
(epa-file-enable)
;; use mode-line to select the gpg key
;; e.g. -*- epa-file-encrypt-to: ("e@mail.com") -*-

;; for org-mod
;; use mode-line to select gpg key
;; e.g. -*- org-crypt-key: "e@mail.com" -*-
(use-package org
  :ensure t
  :bind (("C-c c" . org-capture)
         ("C-c a" . org-agenda))
  :config
  (require 'org-tempo)
  (use-package ob-http
    :ensure t)
  :custom
  (org-capture-templates
   '(("a" "Agenda" entry (file+headline "~/org/agenda.org" "Agenda")
      "* Agenda %?\n  %i\n  %a")
     ("j" "Journal" entry (file+olp+datetree "~/org/journal.org")
      "* %?\n")))
  (org-crypt-use-before-save-magic)
  (org-agenda-files (list "~/org/agenda.org"))
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
  (auto-fill-mode t))

;; for LaTeX
;; (add-hook 'LaTeX-mode-hook 'company-mode)

;; for html
(use-package web-mode
  :ensure t
  :mode (("\\.html\\'" . web-mode)))

;; for xml
(add-hook 'nxml-mode-hook
          #'(lambda ()
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
  ;; (erc-default-server "irc.libera.chat")
  (erc-nick "davidshen84")
  (erc-prompt-for-password nil)
  :config
  (setq erc-default-server "irc.au.libera.chat")
  (setq erc-default-port-tls 6697))

;; for TypeScript
(use-package typescript-mode
  :ensure t
  :mode (("\\.tsx\\'" . typescript-mode))
  :custom
  (typescript-indent-level 2)
  (css-indent-offset 2)
  :config
  (use-package eslint-fix
    :ensure t)
  (use-package prettier-js
    :ensure t)
  :hook (typescript-mode . (lambda ()
                             (setq flycheck-javascript-eslint-executable (string-trim (shell-command-to-string "npx which eslint")))
                             (setq prettier-js-command (string-trim (shell-command-to-string "npx which prettier")))
                             (lsp-deferred))))

(use-package lsp-mode
  :bind-keymap ("C-c C-l" . lsp-command-map)
  :commands (lsp lsp-deferred)

  :custom
  (lsp-enable-file-watchers nil)
  (lsp-enable-links nil)
  (lsp-enable-which-key-integration t)
  (lsp-origami-mode t)
  ;; (lsp-eslint-server-command
  ;;  '("node"
  ;;    "/path/to/local/eslintServer.js"
  ;;    "--stdio"))

  :config
  (use-package lsp-ui
    :ensure t
    :commands lsp-ui-mode)

  (use-package lsp-origami
    :ensure t)

  (use-package lsp-treemacs
    :ensure t
    :commands lsp-treemacs-errors-list)

  (use-package yasnippet
    :ensure t)

  :hook
  (lsp-after-open . lsp-origami-try-enable)
  (lsp-mode . yas-minor-mode))

(use-package origami
  :bind (:map origami-mode-map
              ("C-c @ C-c" . origami-toggle-node)
              ("C-c @ C-l" . origami-recursively-toggle-node)))

(use-package which-key
  :ensure t
  :config (which-key-mode))

(use-package json-mode
  :ensure t
  :mode "\\.json\\'"
  :interpreter "json"
  :custom
  (js-indent-level 2))

(use-package company
  :ensure t
  :config
  (use-package company-prescient
    :ensure t
    :config
    (company-prescient-mode))
  (global-company-mode))

(use-package helm
  :ensure t
  :config
  (use-package helm-company
    :after (company)
    :ensure t
    :bind (
           :map company-mode-map
           ("C-." . helm-company)
           :map company-active-map
           ("C-." . helm-company)))
  (use-package helm-lsp
  :ensure t
  :bind (:map lsp-mode-map
              ([remap xref-find-apropos] . helm-lsp-workspace-symbol)))

  :bind (:map global-map
              ("M-x" . helm-M-x)
              ("C-x C-f" . helm-find-files)
              ("C-x b" . helm-buffers-list)
              ("C-s" . helm-occur)
              ("M-y" . helm-show-kill-ring)))

(use-package iedit
  :ensure t)

(use-package esh-autosuggest
  :ensure t
  :config
  (add-to-list 'company-backends 'esh-autosuggest)
  )

(use-package ag
  :ensure t)

(use-package dirtree
  :ensure t)

(use-package docker
  :ensure t
  :config
  (use-package dockerfile-mode
    :ensure t))

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

(use-package k8s-mode
  :ensure t)

(use-package treemacs
  :ensure t
  :defer t
  :init
  (with-eval-after-load 'winum
    (define-key winum-keymap (kbd "M-0") #'treemacs-select-window))
  :config
  (setq treemacs-collapse-dirs                 (if treemacs-python-executable 3 0)
        treemacs-deferred-git-apply-delay      0.5
        treemacs-directory-name-transformer    #'identity
        treemacs-display-in-side-window        t
        treemacs-eldoc-display                 t
        treemacs-file-event-delay              5000
        treemacs-file-extension-regex          treemacs-last-period-regex-value
        treemacs-file-follow-delay             0.2
        treemacs-file-name-transformer         #'identity
        treemacs-follow-after-init             t
        treemacs-git-command-pipe              ""
        treemacs-goto-tag-strategy             'refetch-index
        treemacs-indentation                   2
        treemacs-indentation-string            " "
        treemacs-is-never-other-window         nil
        treemacs-max-git-entries               5000
        treemacs-missing-project-action        'ask
        treemacs-move-forward-on-expand        nil
        treemacs-no-png-images                 nil
        treemacs-no-delete-other-windows       t
        treemacs-project-follow-cleanup        nil
        treemacs-persist-file                  (expand-file-name ".cache/treemacs-persist" user-emacs-directory)
        treemacs-position                      'left
        treemacs-read-string-input             'from-child-frame
        treemacs-recenter-distance             0.1
        treemacs-recenter-after-file-follow    nil
        treemacs-recenter-after-tag-follow     nil
        treemacs-recenter-after-project-jump   'always
        treemacs-recenter-after-project-expand 'on-distance
        treemacs-show-cursor                   nil
        treemacs-show-hidden-files             t
        treemacs-silent-filewatch              nil
        treemacs-silent-refresh                nil
        treemacs-sorting                       'alphabetic-asc
        treemacs-space-between-root-nodes      t
        treemacs-tag-follow-cleanup            t
        treemacs-tag-follow-delay              1.5
        treemacs-user-mode-line-format         nil
        treemacs-user-header-line-format       nil
        treemacs-width                         35
        treemacs-workspace-switch-cleanup      nil)

  ;; The default width and height of the icons is 22 pixels. If you are
  ;; using a Hi-DPI display, uncomment this to double the icon size.
  ;;(treemacs-resize-icons 44)

  (pcase (cons (not (null (executable-find "git")))
               (not (null treemacs-python-executable)))
    (`(t . t)
     (treemacs-git-mode 'deferred))
    (`(t . _)
     (treemacs-git-mode 'simple)))

  (use-package projectile
    :ensure t
    :defer t
    :bind-keymap ("C-c p" . projectile-command-map)
    :custom
    (projectile-switch-project-action #'projectile-dired)
    (projectile-tags-command "uctags -Re -f \"%s\" %s \"%s\""))

  (use-package treemacs-projectile
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

  :hook (treemacs-mode . projectile-mode))

(use-package pinentry
  :ensure t
  :custom
  (epa-pinentry-mode 'loopback))

(use-package ediff
  :custom
  (ediff-split-window-function 'split-window-horizontally)
  (ediff-window-setup-function 'ediff-setup-windows-plain))

(use-package company-terraform
  :ensure t
  :config
  (company-terraform-init))

(use-package ligature
  :ensure t
  ;; :load-path "path-to-ligature-repo"
  :config
  ;; Enable the "www" ligature in every possible major mode
  (ligature-set-ligatures 't '("www"))
  ;; Enable traditional ligature support in eww-mode, if the
  ;; `variable-pitch' face supports it
  (ligature-set-ligatures 'eww-mode '("ff" "fi" "ffi"))
  ;; Enable all Cascadia Code ligatures in programming modes
  (ligature-set-ligatures 'prog-mode '("|||>" "<|||" "<==>" "<!--" "####" "~~>" "***" "||=" "||>"
                                       ":::" "::=" "=:=" "===" "==>" "=!=" "=>>" "=<<" "=/=" "!=="
                                       "!!." ">=>" ">>=" ">>>" ">>-" ">->" "->>" "-->" "---" "-<<"
                                       "<~~" "<~>" "<*>" "<||" "<|>" "<$>" "<==" "<=>" "<=<" "<->"
                                       "<--" "<-<" "<<=" "<<-" "<<<" "<+>" "</>" "###" "#_(" "..<"
                                       "..." "+++" "/==" "///" "_|_" "www" "&&" "^=" "~~" "~@" "~="
                                       "~>" "~-" "**" "*>" "*/" "||" "|}" "|]" "|=" "|>" "|-" "{|"
                                       "[|" "]#" "::" ":=" ":>" ":<" "$>" "==" "=>" "!=" "!!" ">:"
                                       ">=" ">>" ">-" "-~" "-|" "->" "--" "-<" "<~" "<*" "<|" "<:"
                                       "<$" "<=" "<>" "<-" "<<" "<+" "</" "#{" "#[" "#:" "#=" "#!"
                                       "##" "#(" "#?" "#_" "%%" ".=" ".-" ".." ".?" "+>" "++" "?:"
                                       "?=" "?." "??" ";;" "/*" "/=" "/>" "//" "__" "~~" "(*" "*)"
                                       "\\\\" "://"))
  ;; Enables ligature checks globally in all buffers. You can also do it
  ;; per mode with `ligature-mode'.
  (global-ligature-mode t))

;; modern grep setting
(require 'grep)
(grep-apply-setting 'grep-use-null-device nil)
(setq grep-find-command "find . -type f -exec grep -nHi \"{}\" \";\"")

(provide '.emacs)

;;; .emacs ends here

;; Local Variables:
;; byte-compile-warnings: (not free-vars noruntime)
;; End:

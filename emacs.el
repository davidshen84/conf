;;; .emacs --- Summary
;;; Commentary:
;; -*- coding: utf-8 -*-

;;; Code:

(setq-default warning-minimum-level :error)
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
                                  :height 180
                                  :inherit nil
                                  :weight 'normal)

              (setq-default
               ;; initial-frame-alist '((fullscreen . maximized))
               indent-tabs-mode nil
               default-terminal-coding-system 'utf-8
               select-active-regions nil
               frame-title-format "%b@WSL")
              ))

;; setup elpa package source
(require 'package)
(add-to-list 'package-archives '("elpa" . "https://elpa.gnu.org/packages/") t)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

(unless (fboundp 'use-package)
  (package-install 'use-package))

(require 'use-package)
(use-package use-package-ensure-system-package)

(use-package ace-window)
(use-package my
  :load-path "~/github/conf/lisp"
  :demand t
  :bind (:map global-map
              ("C-c n" . #'my/new-scratch-buffer)
              ("C-c d" . #'my/duplicate-line)
              ("C-x O" . #'my/previous-window)
              ("M-o" . #'ace-window)
              ("C-c C-g" . #'goto-line)
              ("C-c l" . #'display-line-numbers-mode)
              ("C-c b" . #'whitespace-mode)
              ("S-C-<left>" . #'shrink-window-horizontally)
              ("S-C-<right>" . #'enlarge-window-horizontally)
              ("<backtab>" . #'ts-fold-toggle)
              ("C-c <backtab>" . #'ts-fold-open-recursively)
              ))

(use-package indent-bars
  :after (s)
  :config
  (require 'indent-bars-ts)
  :custom
  (indent-bars-treesit-support t)
  (indent-bars-no-descend-string t)
  (indent-bars-treesit-ignore-blank-lines-types '("module"))
  ;; (indent-bars-treesit-wrap '((python argument_list parameters ;; for python, as an example
  ;;                                     list list_comprehension
  ;;                                     dictionary dictionary_comprehension
  ;;                                     parenthesized_expression subscript)))
  :hook ((python-base-mode yaml-mode) . indent-bars-mode))

(cond
 ((window-system) (use-package solarized-theme
                    :config
                    (load-theme 'solarized-dark t)
                    :custom
                    (solarized-use-variable-pitch nil)))

 ((not (window-system)) (use-package material-theme
                          :config
                          (enable-theme 'material))))

;; customize emacs path
(setq exec-path (append exec-path '("~/.local/bin")))

(use-package dirtree
  )

(use-package tree-sitter
  :config
  (global-tree-sitter-mode))

(use-package tree-sitter-langs)

(use-package ts-fold
  ;; git@github.com:emacs-tree-sitter/ts-fold.git
  :load-path "~/github/ts-fold"
  :after (s)
  :init
  (use-package fringe-helper
    )
  :config
  (require 'ts-fold-indicators)
  (global-ts-fold-mode)
  (global-ts-fold-indicators-mode))

;; magit settings
(use-package magit
  :custom
  (magit-define-global-key-bindings 'recommended))

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
(use-package python-mode)

;; EasyPG
;; use mode-line to select the gpg key
;; e.g. -*- epa-file-encrypt-to: ("e@mail.com") -*-
(require 'epa-file)

;; for org-mod
;; use mode-line to select gpg key
;; e.g. -*- org-crypt-key: "e@mail.com" -*-
(use-package org
  :bind (("C-c c" . org-capture)
         ("C-c a" . org-agenda))

  :config
  (require 'org-tempo)
  (require 'org-crypt)
  (org-crypt-use-before-save-magic)

  :custom
  (org-capture-templates
   '(("a" "Agenda" entry (file+headline "~/org/agenda.org" "Agenda")
      "* Agenda %?\n  %i\n  %a")
     ("j" "Journal" entry (file+olp+datetree "~/org/journal.org")
      "* %?\n")))

  (org-tags-exclude-from-inheritance '("crypt"))
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

;; editorconfig settings
(use-package editorconfig
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

;; (use-package origami
;;   :bind (:map origami-mode-map
;;               ("C-c @ C-c" . origami-toggle-node)
;;               ("C-c @ C-l" . origami-recursively-toggle-node)))

(use-package which-key
  :config (which-key-mode))

(use-package json-mode
  :mode "\\.json\\'"
  :interpreter "json"
  :custom
  (js-indent-level 2))

(use-package company
  :config
  (use-package company-prescient
    :config
    (company-prescient-mode))
  (global-company-mode))

(use-package helm
  :config
  (use-package all-the-icons
    )
  (use-package helm-company
    :after (company)
    :bind (
           :map company-mode-map
           ("C-." . helm-company)
           :map company-active-map
           ("C-." . helm-company)))
  (use-package helm-ag
    )
  (use-package helm-xref
    )

  :bind (:map global-map
              ("C-x C-f" . #'helm-find-files)
              ("M-x" . #'helm-M-x)
              ("C-x b" . #'helm-mini)
              ("C-s" . #'helm-occur)
              ("M-y" . #'helm-show-kill-ring)))

(use-package iedit
  )

(use-package esh-autosuggest
  :config
  (add-to-list 'company-backends 'esh-autosuggest))

(use-package ag
  )

(use-package highlight-indentation
  )

(use-package markdown-mode
  :config
  (use-package markdown-preview-mode
    ))

(use-package yaml-mode
  )

(use-package treemacs
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
    :defer t
    :bind-keymap ("C-c p" . projectile-command-map)
    :custom
    (projectile-switch-project-action #'projectile-dired)
    (projectile-tags-command "uctags -Re -f \"%s\" %s \"%s\""))

  (use-package treemacs-projectile
    )

  (use-package treemacs-icons-dired
    :after (treemacs dired)
    :config (treemacs-icons-dired-mode))

  (use-package treemacs-magit
    :after (treemacs magit))

  :bind (:map global-map
              ("C-x t 1"   . treemacs-delete-other-windows)
              ("C-x t t"   . treemacs)
              ("C-x t B"   . treemacs-bookmark)
              ("C-x t C-t" . treemacs-find-file)
              ("C-x t M-t" . treemacs-find-tag))

  :hook (treemacs-mode . projectile-mode))

(use-package pinentry
  :custom
  (epa-pinentry-mode 'loopback)
  :config
  (pinentry-start))

(use-package ediff
  :custom
  (ediff-split-window-function 'split-window-horizontally)
  (ediff-window-setup-function 'ediff-setup-windows-plain))

(use-package ligature
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

(use-package eat
  :hook (eshell-mode . eat-eshell-mode))

(use-package emojify
  :config
  (global-emojify-mode))

(provide '.emacs)

;;; .emacs ends here

;; Local Variables:
;; byte-compile-warnings: (not free-vars noruntime)
;; End:
(put 'upcase-region 'disabled nil)

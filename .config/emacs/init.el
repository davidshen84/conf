;;; .emacs --- Summary
;;; Commentary:
;; -*- coding: utf-8 -*-

;;; Code:

(add-hook 'after-init-hook
          #'(lambda ()
              (show-paren-mode t)
              (delete-selection-mode t)

              ;; set window style
              (menu-bar-mode -1)
              (scroll-bar-mode -1)
              (tool-bar-mode -1)

              (setq-default
               ;; initial-frame-alist '((fullscreen . maximized))
               indent-tabs-mode nil
               default-terminal-coding-system 'utf-8
               select-active-regions nil)

              ;; start emacs server
              (server-start)
              ;; (pinentry-start)
              ))

(add-hook 'after-make-frame-functions
          #'(lambda (frame)
              (select-frame frame)
              (if (window-system)
                  (set-face-attribute
                   'default nil
                   :font "Cascadia Code"
                   :height 160
                   :inherit nil
                   :weight 'normal))

              (use-package solarized-theme
                :if (window-system)
                :custom
                (solarized-use-variable-pitch t)
                :config
                (load-theme 'solarized-dark t))

              (use-package material-theme
                :if (not window-system)
                :config
                (load-theme 'material t))))

;; setup elpa package source
(require 'package)
(add-to-list 'package-archives '("elpa" . "https://elpa.gnu.org/packages/") t)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)

(unless (fboundp 'use-package)
  (package-install 'use-package))

(require 'use-package)
(use-package use-package-ensure-system-package
  :ensure t)

(use-package auth-source
  :custom
  (auth-sources '("~/.authinfo.gpg")))
(use-package ace-window
  :ensure t
  :bind (:map global-map
              ("M-o" . #'ace-window)))

(use-package xclip
  :ensure t
  :config
  (xclip-mode t))
(use-package dirtree
  :ensure t)
(use-package iedit
  :ensure t)
(use-package ag
  :ensure t)
(use-package highlight-indentation
  :ensure t)
(use-package yaml-mode
  :ensure t)
(setq dired-listing-switches "-alh")

(use-package my
  :load-path "~/github/conf/lisp"
  :demand t
  :bind (:map global-map
              ("C-c n" . #'my/new-scratch-buffer)
              ("C-c d" . #'my/duplicate-line)
              ("C-x O" . #'my/previous-window)
              ("C-c C-g" . #'goto-line)
              ("C-c l" . #'display-line-numbers-mode)
              ("C-c b" . #'whitespace-mode)
              ("S-C-<left>" . #'shrink-window-horizontally)
              ("S-C-<right>" . #'enlarge-window-horizontally)
              ("<backtab>" . #'treesit-fold-toggle)
	      ))

;; `hs-minor-mode'
(add-hook 'hs-minor-mode-hook
          #'(lambda ()
              (define-key hs-minor-mode-map (kbd "<backtab>") #'hs-toggle-hiding)))

(use-package indent-bars
  :ensure t
  :after (s)
  :config
  (require 'indent-bars-ts)
  :custom
  ;; (indent-tabs-mode nil)
  (indent-bars-treesit-support t)
  (indent-bars-no-descend-string t)
  (indent-bars-treesit-ignore-blank-lines-types '("module"))
  :hook ((python-base-mode yaml-mode json-mode) . indent-bars-mode))

(use-package solarized-theme
  :ensure t
  :if (window-system)
  :config
  (load-theme 'solarized-dark t)
  :custom
  (solarized-use-variable-pitch nil))

(use-package material-theme
  :ensure t
  :if (not window-system)
  :config
  (enable-theme 'material))

;; `tree-sitter'
(use-package tree-sitter
  :ensure t
  :config
  (use-package tree-sitter-langs
    :ensure t)
  (use-package treesit-fold
    :ensure t)
  (global-tree-sitter-mode)
  (global-treesit-fold-mode)
  (global-treesit-fold-indicators-mode))

(use-package magit
  :ensure t
  :custom
  (magit-define-global-key-bindings 'recommended))

;; `ibuffer'
(add-hook 'ibuffer-mode-hook
          #'(lambda ()
              (ibuffer-switch-to-saved-filter-groups "default")))
(setq ibuffer-saved-filter-groups
      '(("default"
         ("magit" (name . "magit"))
         ;; ("erc" (mode . erc-mode))
         ("ssh" (filename . "/ssh.*"))
         )))

;; `lisp'
(add-hook 'emacs-lisp-mode-hook
          #'(lambda ()
              (setq indent-tabs-mode nil)))

;; EasyPG
;; use mode-line to select the gpg key
;; e.g. -*- epa-file-encrypt-to: ("e@mail.com") -*-
(require 'epa-file)

;; `org-mode'
;; use mode-line to select gpg key
;; e.g. -*- org-crypt-key: "e@mail.com" -*-
(use-package org
  ;; :init
  :custom
  (org-src-preserve-indentation nil)
  (org-edit-src-content-indentation 0)
  :config
  (use-package ob-http
    :ensure t)
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
  (auto-fill-mode t)
  :bind (:map global-map
              ("C-c c" . org-capture)
              ("C-c a" . org-agenda)))

;; for LaTeX
;; (add-hook 'LaTeX-mode-hook 'company-mode)

;; `xml'
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

(use-package editorconfig
  :ensure t
  :custom
  (editorconfig-exclude-modes '(emacs-lisp-mode lisp-mode))
  (editorconfig-get-properties-function 'editorconfig-core-get-properties-hash))

(use-package erc
  :ensure t
  :custom
  ;; (erc-default-server "irc.libera.chat")
  (erc-nick "davidshen84")
  (erc-prompt-for-password nil)
  :config
  (setq erc-default-server "irc.au.libera.chat")
  (setq erc-default-port-tls 6697))

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
    (add-to-list 'completion-styles 'prescient)
    (company-prescient-mode))
  (global-company-mode))

(use-package marginalia
  :ensure t
  :config
  (marginalia-mode t))

;; `vertico' - vertical completion UI
(use-package vertico
  :ensure t
  :init
  (vertico-mode t)
  :custom
  (vertico-count 13)
  (vertico-quick-insert nil)
  (vertico-resize t)
  (vertico-cycle t)
  :bind (:map vertico-map
              ;; Group navigation
              ("C-M-n" . vertico-next-group)
              ("C-M-p" . vertico-previous-group)

              ;; Directory navigation
              ("RET" . vertico-directory-enter)
              ("DEL" . vertico-directory-delete-char)
              ("M-DEL" . vertico-directory-delete-word)))

;; `orderless' - flexible completion matching
(use-package orderless
  :ensure t
  :custom
  ;; Use orderless for completion styles
  (completion-styles '(orderless basic))
  (completion-category-defaults nil)
  ;; Use basic style for file paths (better for tramp)
  (completion-category-overrides '((file (styles basic partial-completion)))))

;; `savehist' - persist minibuffer history
(use-package savehist
  :init
  (savehist-mode t)
  :custom
  ;; Save additional variables
  (savehist-additional-variables '(search-ring regexp-search-ring)))

(use-package consult
  :ensure t
  ;; :custom

  :config
  (recentf-mode t)
  :bind (:map global-map
              ;; ("C-x c f" . #'consult-find)
              ("C-x c r" . #'consult-recent-file)
              ("C-x c g" . #'consult-ripgrep)
              ("C-x c b" . #'consult-bookmark)
              ("C-x b" . #'consult-buffer)
              ("C-x c s" . #'consult-line))
  )

(add-hook 'eshell-mode-hook
          #'(lambda ()
              (eshell/addpath
               (expand-file-name "~/.local/bin"))))

(use-package esh-autosuggest
  :ensure t
  :config
  (add-to-list 'company-backends 'esh-autosuggest))

(use-package markdown-mode
  :ensure t
  :config
  (use-package markdown-preview-mode
    :ensure t))


(use-package treemacs
  :ensure t
  :defer t
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
    :ensure t
    :after (treemacs dired)
    :config (treemacs-icons-dired-mode))

  (use-package treemacs-magit
    :ensure t
    :after (treemacs magit))

  :bind (:map global-map
              ("C-c t t"   . #'treemacs-select-window)
              ("C-c t 1"   . #'treemacs-delete-other-windows)
              ("C-c t b"   . #'treemacs-bookmark)
              ("C-c t C-s" . #'treemacs-find-file)
              ("C-c t M-t" . #'treemacs-find-tag))

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
  :ensure t
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
;; (require 'grep)
;; (grep-apply-setting 'grep-use-null-device nil)
;; (setq grep-find-command "find . -type f -exec grep -nHi \"{}\" \";\"")

(use-package eat
  :ensure t
  :config
  (custom-set-variables
   '(eat-semi-char-non-bound-keys
     (add-to-list 'eat-semi-char-non-bound-keys [?\e ?o])))
  :hook (eshell-mode . eat-eshell-mode)
  )

(use-package emojify
  :ensure t
  :config
  (global-emojify-mode))

(use-package docker
  :config
  (setq docker-compose-command "docker compose"))

(add-hook 'after-init-hook
          #'(lambda ()
              ;; (ido-mode t)
              (fido-vertical-mode t)
              (show-paren-mode t)
              (delete-selection-mode t)

              ;; bind list buffer to ibuffer
              (defalias 'list-buffers 'ibuffer)

              ;; set window style
              (menu-bar-mode -1)
              (scroll-bar-mode -1)
              (tool-bar-mode -1)

              (setq-default
               ;; initial-frame-alist '((fullscreen . maximized))
               default-terminal-coding-system 'utf-8
               select-active-regions nil
               )

              ;; start emacs server
              (unless (server-running-p)
                (server-start))
              ))

(add-hook 'after-make-frame-functions
          #'(lambda (frame)
              (select-frame frame)
              (if (window-system)
                  (set-face-attribute
                   'default nil
                   :font "Cascadia Code"
                   :height 160
                   :inherit nil
                   :weight 'normal))

              (use-package solarized-theme
                :if (window-system)
                :custom
                (solarized-use-variable-pitch t)
                :config
                (load-theme 'solarized-dark t))

              (use-package material-theme
                :if (not window-system)
                :config
                (load-theme 'material t))))

(use-package my
  :load-path "~/github/conf/lisp"
  :requires (ace-window ts-fold)
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
              ("C-c <backtab>" . #'ts-fold-open-recursively)))


(provide '.emacs)

;;; .emacs ends here


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(json-mode eat markdown-preview-mode emojify ligature pinentry markdown-mode marginalia company-prescient company which-key ob-http python-mode yaml-mode highlight-indentation iedit magit fringe-helper material-theme indent-bars editorconfig-generate editorconfig consult docker xclip tree-sitter-langs dirtree ace-window)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; Local Variables:
;; byte-compile-warnings: (not free-vars noruntime)
;; End:

;;; .emacs --- Summary
;;; Commentary:
;; -*- coding: utf-8 -*-

;;; Code:

;; load custom scripts
(eval-when-compile
  (add-to-list 'load-path "~/.emacs.d/lisp")
  ;; setup elpa package source
  (require 'package)
  (package-initialize)
  (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
  (add-to-list 'package-archives '("org" . "https://orgmode.org/elpa/") t)
  (require 'use-package))

(use-package use-package-ensure-system-package
  :ensure t)

(use-package dirtree
  :ensure t)

(use-package dracula-theme
;; (use-package flatui-dark-theme
  :if (display-graphic-p)
  :ensure t
  :config
  (set-face-attribute 'default nil
                      :family "Source Code Pro"
                      :height 140
                      :inherit nil
                      :weight 'normal))

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
  (highlight-indentation-mode t)
  ;; (hs-minor-mode t)
  (linum-mode t)
  )

;; my key binding
(global-set-key (kbd "C-c g") 'goto-line)
(global-set-key (kbd "C-c l") 'linum-mode)
(global-set-key (kbd "C-c b") 'whitespace-mode)
;; (global-set-key (kbd "C-c /") 'comment-region)
;; (global-set-key (kbd "C-c M-/") 'uncomment-region)
(global-set-key (kbd "C-c n") 'new-scratch-buffer)
(global-set-key (kbd "C-c d") 'duplicate-line)
(global-set-key (kbd "S-C-<left>") 'shrink-window-horizontally)
(global-set-key (kbd "S-C-<right>") 'enlarge-window-horizontally)
(global-set-key (kbd "C-x O") (lambda ()
                                (interactive)
                                (other-window -1)))

;; for Windows environment
;; update Emacs' execution path to be the same as Windows'.
(if (string-equal system-type "windows-nt")
    (progn (setenv "PATH"
                   (mapconcat 'identity
                              `("c:\\windows",
                                (getenv "PATH"))
                              ";"))
           (setq exec-path (split-string
                            (replace-regexp-in-string "\\\\" "/" (getenv "PATH"))
                            ";"))))

;; eshell settins
(add-hook 'eshell-mode-hook
          #'(lambda ()
              ))

;; magit settings
(use-package magit
  :ensure t
  :custom
  (ibuffer-saved-filter-groups '(("default"
                                  ("magit" (name . "magit")))))
  :bind (:map global-map
              ("C-x g"     . magit-status)))

;; ibuffer settings
(declare-function ibuffer-switch-to-saved-filter-groups "ibuf-ext.el" (name))
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
(use-package python-mode
  :ensure t
  :hook dev-common)

;; for css
(add-hook 'css-mode-hook 'dev-common)

;; for org-mod
(use-package org-plus-contrib
  :ensure t
  :init
  ;; (require 'org-notify)
  (require 'org-tempo)
  :bind (("C-c c" . org-capture))
  :custom
  (org-capture-templates
   '(("t" "Todo" entry (file+headline "~/org/todo.org" "Tasks")
      "* TODO %?\n  %i\n  %a")
     ("j" "Journal" entry (file+olp+datetree "~/org/journal.org")
      "* %?\n")))
  (org-agenda-files (list "~/org/todo.org"))
  (org-log-done 'time)
  (org-src-fontify-natively t)
  (org-babel-load-languages '((python . t)
                              (shell . t)
                              (sql . t)
                              ;; add more languages
                              ))
  :hook (org-mode . (lambda ()
                      (auto-fill-mode t))))

;; for LaTeX
;; (add-hook 'LaTeX-mode-hook 'company-mode)

;; for html
(use-package web-mode
  :ensure t
  :mode (("\\.html\\'" . web-mode))
  :hook (web-mode . (lambda ()
                      (dev-common)
                      )))

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
;; (use-package clang-format)
;; (use-package cmake-mode)
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
  :custom ((typescript-indent-level 2))
  :hook (typescript-mode . (lambda ()
                             (eldoc-mode t)
                             (setq flycheck-javascript-eslint-executable (string-trim (shell-command-to-string "npx which eslint")))
                             (lsp)))
  )

(use-package lsp-ui
  :ensure t
  :commands lsp-ui-mode
  )

(use-package lsp-mode
  :bind-keymap ("C-c C-l" . lsp-command-map)
  :hook (lsp-mode . (lambda ()
                      (dev-common)
                      (lsp-enable-which-key-integration)
                      (lsp-origami-mode)))
  :config (progn (setq-default lsp-enable-snippet nil)
                 )
  :commands lsp
  )

(use-package lsp-origami
  :ensure t
  :bind (:map origami-mode-map
              ("C--" . origami-close-node)
              ("C-M--" . origami-open-node)))

(use-package which-key
  :ensure t
  :config (which-key-mode))

(use-package lsp-treemacs
  :ensure t
  :commands lsp-treemacs-errors-list)

(use-package helm-lsp
  :ensure t
  :bind ([remap xref-find-apropos] . helm-lsp-workspace-symbol)
  :commands (helm-lsp-workspace-symbol)
  )

;; for js/json
;; (use-package js3-mode
;;   :ensure t
;;   :mode "\\.js\\'"
;;   :interpreter "js3"
;;   :hook (js3-mode . dev-common))
(use-package js2-mode
  :ensure t
  :mode "\\.js\\'"
  :interpreter "js2"
  :hook (js2-mode . (lambda ()
                      (dev-common)
                      (lsp)
                      )))

(use-package json-mode
  :ensure t
  :mode "\\.json\\'"
  :interpreter "json"
  :hook (json-mode . dev-common))

(use-package helm-company
  :ensure t
  :bind (("M-x" . helm-M-x)
         ("C-x C-f" . helm-find-files)
         :map company-mode-map
         ("C-:" . helm-company)
         :map company-active-map
         ("C-:" . helm-company))
  )

(use-package company
  :config (global-company-mode))

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
  :ensure t)
(use-package dockerfile-mode
  :ensure t)
(use-package eslint-fix
  :ensure t)
(use-package flycheck
  :ensure t
  :config (progn
            ;; (global-flycheck-mode)
            (flycheck-add-mode 'javascript-eslint 'typescript-mode)
            ))

(use-package highlight-indentation
  :ensure t)
(use-package markdown-mode
  :ensure t)
(use-package markdown-preview-mode
  :ensure t)
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

  (treemacs-follow-mode t)
  (treemacs-filewatch-mode t)
  (treemacs-fringe-indicator-mode t)
  (projectile-mode t)
  (pcase (cons (not (null (executable-find "git")))
               (not (null (treemacs--find-python3))))
    (`(t . t)
     (treemacs-git-mode 'deferred))
    (`(t . _)
     (treemacs-git-mode 'simple)))
  :bind (:map global-map
              ("M-0"       . treemacs-select-window)
              ("C-x t 1"   . treemacs-delete-other-windows)
              ("C-x t t"   . treemacs)
              ("C-x t B"   . treemacs-bookmark)
              ("C-x t C-t" . treemacs-find-file)
              ("C-x t M-t" . treemacs-find-tag)))

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

(use-package verb
  :ensure t)

(use-package pinentry
  :ensure t)

;; init.
(setq default-terminal-coding-system 'utf-8)
(add-hook 'after-init-hook
          #'(lambda ()
              (ido-mode t)
              (show-paren-mode t)
              (delete-selection-mode t)


              ;; bind list buffer to ibuffer
              (defalias 'list-buffers 'ibuffer)

               ;; maximize emacs

              (menu-bar-mode -1)
              (scroll-bar-mode -1)
              (tool-bar-mode -1)
              (setq-default initial-frame-alist '((fullscreen . maximized))
                            ediff-split-window-function 'split-window-horizontally
                            ediff-window-setup-function 'ediff-setup-windows-plain
                            epa-pinentry-mode 'loopback
                            indent-tabs-mode nil
                            select-active-regions nil
                            org-confirm-babel-evaluate nil)

              ;; start emacs server
              (server-start)
              (pinentry-start)
              ))

;; modern grep setting
(require 'grep)
(grep-apply-setting 'grep-use-null-device nil)
(setq grep-find-command "find . -type f -exec grep -nHi \"{}\" \";\"")

;; local-variable settings
(setq safe-local-variable-values '((make-backup-files)))

(provide '.emacs)


;; Local Variables:
;; byte-compile-warnings: (not free-vars noruntime)
;; End:

;;; .emacs ends here

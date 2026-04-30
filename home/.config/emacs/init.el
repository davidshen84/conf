;;; init.el --- Emacs configuration entry point  -*- lexical-binding: t; -*-

;;; Code:

;; Add modules directory to load-path
(add-to-list 'load-path (expand-file-name "modules" user-emacs-directory))

;; Load common module first (includes package setup)
(require 'common)

;; Load other configuration modules
(require 'modes)
(require 'ui)
(require 'prog)
(require 'tools)

(require 'completion-module)
(require 'lsp-module)
(require 'org-module)
(require 'project-module)
(require 'shell-module)


;; Custom and personal configurations
(use-package my
  :bind-keymap ("C-c m" . my-map)
  :bind (:map global-map
              ("C-c C-g" . #'goto-line)
              ("C-c l" . #'display-line-numbers-mode)
              ("C-c b" . #'whitespace-mode)
              ("S-C-<left>" . #'shrink-window-horizontally)
              ("S-C-<right>" . #'enlarge-window-horizontally)))

(provide 'init)

;;; init.el ends here

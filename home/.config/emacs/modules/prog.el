;;; prog.el --- Programming infrastructure and cross-cutting tools  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(setq treesit-language-source-alist
      '(
        (python "https://github.com/tree-sitter/tree-sitter-python")
        (c-sharp "https://github.com/tree-sitter/tree-sitter-c-sharp" "main")
        (elisp "https://github.com/Wilfred/tree-sitter-elisp")
        ))

(use-package treesit-fold
  :ensure t
  :bind (:map treesit-fold-mode-map
              ("<backtab>" . #'treesit-fold-toggle)))

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
  (indent-bars-treesit-support t)
  (indent-bars-no-descend-string t)
  (indent-bars-treesit-ignore-blank-lines-types '("module"))
  :hook ((python-base-mode yaml-mode json-mode) . indent-bars-mode))

(use-package editorconfig
  :ensure t
  :custom
  (editorconfig-exclude-modes '(emacs-lisp-mode lisp-mode))
  (editorconfig-get-properties-function 'editorconfig-core-get-properties-hash))

(provide 'prog)
;;; prog.el ends here

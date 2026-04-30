;;; lsp-module.el --- Language Server Protocol configuration  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(use-package lsp-mode
  :ensure t
  :commands (lsp lsp-deferred)
  :init
  (setq lsp-keymap-prefix "C-c l")
  :custom
  ;; Optimization for large projects
  (read-process-output-max (* 1024 1024)) ;; 1mb
  (lsp-idle-delay 0.5)
  (lsp-log-io nil)
  (lsp-completion-provider :capf) ;; Use Company-capf via Company
  
  ;; UI settings
  (lsp-headerline-breadcrumb-enable t)
  (lsp-lens-enable t)
  :hook (;; add language modes
         ;; (python-base-mode . lsp-deferred)
         ;; (csharp-mode . lsp-deferred)
         ;; LSP-mode features
         (lsp-mode . lsp-enable-which-key-integration))
  :config
  (define-key lsp-mode-map [remap xref-find-definitions] #'lsp-find-definition)
  (define-key lsp-mode-map [remap xref-find-references] #'lsp-find-references))

(use-package lsp-ui
  :ensure t
  :commands lsp-ui-mode
  :custom
  (lsp-ui-doc-enable t)
  (lsp-ui-doc-position 'at-point)
  (lsp-ui-sideline-enable t)
  (lsp-ui-sideline-show-hover t)
  (lsp-ui-imenu-enable t)
  :hook (lsp-mode . lsp-ui-mode))

;; Integration with consult
(use-package consult-lsp
  :ensure t
  :after (lsp consult)
  :bind (:map lsp-mode-map
              ([remap lsp-find-workspace-symbol] . consult-lsp-symbols)))

(use-package lsp-treemacs
  :ensure t
  :after (lsp treemacs)
  :config
  (lsp-treemacs-sync-mode 1))

(provide 'lsp-module)
;;; lsp-module.el ends here

;;; shell-module.el --- Shell and terminal configuration  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(add-hook 'eshell-mode-hook
          #'(lambda ()
              (eshell/addpath
               (expand-file-name "~/.local/bin"))))

(use-package esh-autosuggest
  :ensure t
  :config
  (add-to-list 'company-backends 'esh-autosuggest))

(use-package eat
  :ensure t
  :config
  (custom-set-variables
   '(eat-semi-char-non-bound-keys
     (add-to-list 'eat-semi-char-non-bound-keys [?\e ?o])))
  :hook (eshell-mode . eat-eshell-mode))

(provide 'shell-module)
;;; shell-module.el ends here

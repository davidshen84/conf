;;; modes.el --- File-type specific modes and configurations  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(use-package yaml-mode
  :ensure t)

(use-package json-mode
  :ensure t
  :custom
  (js-indent-level 2))

(use-package markdown-mode
  :ensure t
  :config
  (use-package markdown-preview-mode
    :ensure t))

;; `lisp'
(add-hook 'emacs-lisp-mode-hook
          #'(lambda ()
              (setq indent-tabs-mode nil)))

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

(provide 'modes)
;;; modes.el ends here

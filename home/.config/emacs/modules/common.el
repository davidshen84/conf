;;; common.el --- Common functions and configurations
;;; Commentary:
;;; Code:

;; Package management
(require 'package)
(add-to-list 'package-archives '("elpa" . "https://elpa.gnu.org/packages/") t)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)

(unless package--initialized
  (package-initialize))

(defalias 'list-buffers 'ibuffer)

(setq dired-listing-switches "-alh")

(use-package dirtree
  :ensure t)

(use-package iedit
  :ensure t)

(use-package highlight-indentation
  :ensure t)

(use-package which-key
  :ensure t
  :config (which-key-mode))

;; `ibuffer'
(add-hook 'ibuffer-mode-hook
          #'(lambda ()
              (ibuffer-switch-to-saved-filter-groups "default")))

(setq ibuffer-saved-filter-groups
      '(("default"
         ("magit" (name . "magit"))
         ("erc" (mode . erc-mode))
         ("ssh" (filename . "/ssh.*")))))

;; `EasyPG'
(require 'epa-file)

(provide 'common)
;;; common.el ends here

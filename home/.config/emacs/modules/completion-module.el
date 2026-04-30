;;; completion-module.el --- Completion framework setup
;;; Commentary:
;;; Code:

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
  :init
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
  :config
  (recentf-mode t)
  :bind (:map global-map
              ("C-x b" . #'consult-buffer)
              ("C-c c b" . #'consult-bookmark)
              ("C-c c f" . #'consult-find)
              ("C-c c g" . #'consult-ripgrep)
              ("C-c c l" . #'consult-line)
              ("C-c c r" . #'consult-recent-file)))

(provide 'completion-module)
;;; completion-module.el ends here

;;; .emacs --- Summary
;;; Commentary:
;; -*- coding: utf-8 -*-

;;; Code:

;; load custom scripts
(add-to-list 'load-path "~/.emacs.d/lisp")

;; setup elpa package source
(require 'package)
(package-initialize)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t)

;; load theme
(if (display-graphic-p)
    ;; (load-theme 'deeper-blue)
    (progn (require 'dracula-theme)
           (set-face-attribute 'default nil
                               :family "Source Code Pro"
                               :foundry 'outline
                               :height 180
                               :inherit nil
                               :weight 'normal))
  (load-theme 'manoj-dark))

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
  (company-mode t)
  (editorconfig-mode t)
  (highlight-indentation-mode t)
  (hs-minor-mode t)
  (linum-mode t))

;; my key binding
(global-set-key (kbd "C-c g") 'goto-line)
(global-set-key (kbd "C-c l") 'linum-mode)
(global-set-key (kbd "C-c b") 'whitespace-mode)
(global-set-key (kbd "C-c /") 'comment-region)
(global-set-key (kbd "C-c M-/") 'uncomment-region)
(global-set-key (kbd "C-c n") 'new-scratch-buffer)
(global-set-key (kbd "C-c d") 'duplicate-line)
(global-set-key (kbd "C-c c") 'org-capture)

(add-hook 'after-init-hook
          #'(lambda ()
              (require 'dirtree)
              (require 'iedit)

              (ido-mode t)
              (show-paren-mode t)
              (delete-selection-mode t)

              ;; start emacs server
              (server-start)

              ;; bind list buffer to ibuffer
              (defalias 'list-buffers 'ibuffer)

               ;; maximize emacs
              (setq initial-frame-alist '((fullscreen . maximized)))
              (menu-bar-mode -1)
              (scroll-bar-mode -1)
              (tool-bar-mode -1)))

(declare-function ibuffer-switch-to-saved-filter-groups "ibuf-ext.el" (name))
;; ibuffer settings
(setq ibuffer-saved-filter-groups '(("default"
                                     ("magit" (name . "magit")))))
(add-hook 'ibuffer-mode-hook
          #'(lambda ()
              (ibuffer-switch-to-saved-filter-groups "default")))

;; for shell script
(add-hook 'sh-mode-hook 'dev-common)

;; for lisp
(add-hook 'emacs-lisp-mode-hook 'dev-common)

;; for python
(add-hook 'python-mode-hook
          #'(lambda ()
              (dev-common)))

;; for js
(add-to-list 'auto-mode-alist '("\\.js\\'" . js3-mode))
(add-hook 'js3-mode-hook 'dev-common)
(add-hook 'json-mode-hook 'dev-common)

;; for css
(add-hook 'css-mode-hook 'dev-common)

;; for org-mod
(require 'org)
(require 'org-notify)
;; (org-notify-start)
(setq org-default-notes-file (concat org-directory "/.notes"))
(setq org-agenda-files (list org-default-notes-file "~/org/agenda"))
(setq org-capture-templates
      '(("t" "Task" entry (file+headline "" "Tasks") "* TODO %? %^g")
        ("q" "Quick note" entry (file+datetree "") "* [%<%H:%M>] %?")))
(setq org-log-done 'time)
(setq org-src-fontify-natively t)
(add-hook 'org-mode-hook
          #'(lambda ()
              (auto-fill-mode t)
              (org-babel-do-load-languages 'org-babel-load-languages
                                           '((python . t)
                                             (sh . t)
                                             (shell . t)
                                             (sql . t)
                                             ;; add more languages
                                             ))))

;; for LaTeX
(add-hook 'LaTeX-mode-hook 'company-mode)

;; for html
(add-to-list 'auto-mode-alist '("\\.html\\'" . web-mode))

;; for cuda
(add-hook 'cuda-mode-hook 'dev-common)

;; for xml
(require 'sgml-mode)
(add-hook 'nxml-mode-hook 'dev-common)
(add-to-list 'hs-special-modes-alist
             '(nxml-mode
               "<!--\\|<[^/>]*[^/]>"
               "-->\\|</[^/>]*[^/]>"

               "<!--"
               sgml-skip-tag-forward
               nil))

;; for c/c++
(mapc #'(lambda (hook)
          (add-hook hook
                    #'(lambda ()
                        (dev-common)
                        (require 'clang-format)
                        (setq clang-format-style "Google")
                         ;; flycheck
                        (setq flycheck-clang-language-standard "c++11"))))
      '(c-mode-hook c++-mode-hook))

;; editorconfig settings
;; '(editorconfig-exclude-modes (quote (emacs-lisp-mode lisp-mode json-mode)))
(setq editorconfig-get-properties-function 'editorconfig-core-get-properties-hash)

;; erc settings
(setq erc-nick "davidshen84")

;; eshell settings
(add-hook 'eshell-load-hook
          #'(lambda ()
              (setq-default eshell-path-env (mapconcat 'identity `("/usr/local/bin", eshell-path-env) ":"))))

 ;; package settings
(setq package-selected-packages
      '(;; sorted alphabetically
        clang-format
        cmake-mode
        company
        dirtree
        docker
        dockerfile-mode
        dracula-theme
        editorconfig
        flycheck-pyflakes
        groovy-mode
        highlight-indentation
        iedit
        js3-mode
        json-mode
        magit
        markdown-mode
        markdown-preview-mode
        org-plus-contrib
        web-mode
        yaml-mode
        ))

;; local-variable settings
(setq safe-local-variable-values '((make-backup-files)))

(provide '.emacs)

;; Local Variables:
;; byte-compile-warnings: (not free-vars)
;; End:

;;; .emacs ends here

;; -*- Emacs-Lisp -*-

(add-to-list 'load-path "~/.emacs.d/lisp")
(autoload 'dirtree "dirtree" "Add directory to tree view" t)

;; add melpa package
(progn
  (require 'package)
  (package-initialize)
  (add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t))

;; markdown-mode
(progn 
  (autoload 'markdown-mode "markdown-mode"
    "Major mode for editing Markdown files" t)
  ;; bind file extension with mode
  (dolist (p '(("\\.markdown\\'" . markdown-mode)
               ("\\.md\\'" . markdown-mode)))
    (add-to-list 'auto-mode-alist p)))

;; load theme
(if (display-graphic-p)
    (load-theme 'deeper-blue 1)
  (load-theme 'manoj-dark 1))

;; my key binding
(progn
  ;; bind goto line function with Ctrl-c-g
  (global-set-key (kbd "C-c g") 'goto-line)
  (global-set-key (kbd "C-c l") 'linum-mode)
  (global-set-key (kbd "C-c b") 'whitespace-mode)
  (global-set-key (kbd "C-c c") 'comment)
  (global-set-key (kbd "C-c C") 'comment-region)
  (global-set-key (kbd "C-c U") 'uncomment-region)
  (global-set-key (kbd "C-c .") 'fold-this)
  (global-set-key (kbd "C-c C-.") 'fold-this-unfold-a))

;; some basic settings
(progn
  ;; enable editorconfig mode
  (setq editorconfig-get-properties-function
        'editorconfig-core-get-properties-hash)
  (editorconfig-mode 1)
  (ido-mode)
  ;; bind list buffer to ibuffer
  (defalias 'list-buffers 'ibuffer))

;; load ac
(progn
  (require 'auto-complete-config)
  (ac-config-default)
  ;; In your project root directory, do follow command to make tags file.
  ;; etags --verbose -R --fields="+afikKlmnsSzt"
  ;; (require 'auto-complete-exuberant-ctags)
  ;; (ac-exuberant-ctags-setup)
  )

(defun dev-basic ()
  (linum-mode t)
  (highlight-indentation-mode)
  (auto-complete-mode))

;; for shell script
(add-hook 'sh-mode-hook
          '(lambda ()
             (dev-basic)))

;; for elisp
(add-hook 'emacs-lisp-mode-hook
          '(lambda ()
             (dev-basic)))

;; for python
(add-hook 'python-mode-hook
          '(lambda ()
             (dev-basic)
             (hs-minor-mode t)))

(add-hook 'pylint-mode-hook
          '(lambda ()
             (setq pylint-options '("--reports=n"))))

;; for js/json
(progn
  ;; bind js to js3-mode
  (add-to-list 'auto-mode-alist
               '("\\.js\\'" . js3-mode))
  (add-hook 'js3-mode-hook
            '(lambda ()
               (dev-basic)))

  ;; bind json to json-mode
  (add-to-list 'auto-mode-alist
               '("\\.json\\'" . json-mode))
  (add-hook 'json-mode-hook
            '(lambda ()
               (dev-basic))))

;; for css
(progn
  (add-hook 'css-mode-hook
            '(lambda ()
               (dev-basic))))

;; for org-mod
(add-hook 'org-mode-hook
          '(lambda ()
             (auto-fill-mode t)
             ;; load python in org-mod
             (org-babel-do-load-languages
              'org-babel-load-languages
              '((python . t)
                ;; add more languages
                ))))

;; For LaTeX
(add-hook 'LaTeX-mode-hook
          '(lambda ()
             (auto-complete-mode t)))

;; for html
(progn
  ;; bind file extension to web-mode
  (add-to-list 'auto-mode-alist
               '("\\.html\\'" . web-mode)))

;; for cuda
(progn
  (add-hook 'cuda-mode-hook
            '(lambda ()
               (dev-basic))))

;; load project settings
;; (load-file "~/.emacs.d/project-settings.el")


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(menu-bar-mode nil)
 '(show-paren-mode t)
 '(tool-bar-mode nil))

(if (display-graphic-p)
    (custom-set-faces
     ;; custom-set-faces was added by Custom.
     ;; If you edit it by hand, you could mess it up, so be careful.
     ;; Your init file should contain only one such instance.
     ;; If there is more than one, they won't work right.
     '(default ((t (:family "Source Code Pro" :weight normal :height 180 :width normal))))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "Source Code Pro" :weight normal :height 180 :width normal)))))

;;; org-module.el --- Org-mode configuration
;;; Commentary:
;;; Code:

(use-package org
  :custom
  (org-src-preserve-indentation nil)
  (org-edit-src-content-indentation 0)
  (org-capture-templates
   '(("a" "Agenda" entry (file+headline "~/org/agenda.org" "Agenda")
      "* Agenda %?\n  %i\n  %a")
     ("j" "Journal" entry (file+olp+datetree "~/org/journal.org")
      "* %?\n")))
  (org-tags-exclude-from-inheritance '("crypt"))
  (org-agenda-files (list "~/org/agenda.org"))
  (org-log-done 'time)
  (org-src-fontify-natively t)
  (org-confirm-babel-evaluate nil)
  (org-babel-load-languages
   '((python . t)
     (shell . t)
     (sql . t)
     (lisp . t)
     (emacs-lisp . t)
     (http . t)))
  :config
  (require 'org-tempo)
  (require 'org-crypt)
  (org-crypt-use-before-save-magic))

(use-package ob-http
  :after org
  :ensure t)

(use-package org-roam
  :after org
  :ensure t
  :custom
  (org-roam-directory "~/org/roam")
  :config
  (org-roam-db-autosync-mode)
  :bind (:map global-map
              ("C-c n f" . #'org-roam-node-find)))

(provide 'org-module)
;;; org-module.el ends here

;;; tools.el --- Miscellaneous development tools  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(use-package docker
  :ensure t
  :config
  (setq docker-compose-command "docker compose"))

(use-package pinentry
  :ensure t
  :custom
  (epa-pinentry-mode 'loopback))

(use-package ediff
  :custom
  (ediff-split-window-function 'split-window-horizontally)
  (ediff-window-setup-function 'ediff-setup-windows-plain))

(use-package xclip
  :ensure t
  :config
  (xclip-mode t))

(use-package ace-window
  :ensure t
  :bind (:map global-map
              ("M-o" . #'ace-window)))

(use-package erc
  :ensure t
  :custom
  (erc-default-server "irc.libera.chat")
  (erc-default-port-tls 6697)
  (erc-nick "davidshen84")
  (erc-prompt-for-password nil))

(provide 'tools)
;;; tools.el ends here

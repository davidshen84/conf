;;; ui.el --- UI, fonts, and themes
;;; Commentary:
;;; Code:

(add-hook 'after-init-hook
          #'(lambda ()
              (show-paren-mode t)
              (delete-selection-mode t)
              (menu-bar-mode -1)
              (setq-default
               default-terminal-coding-system 'utf-8
               select-active-regions nil)))

(add-hook 'after-make-frame-functions
          #'(lambda (frame)
              (select-frame frame)
              (when (window-system)
                ;; set window style
                (tool-bar-mode -1)
                (scroll-bar-mode -1)

                (set-face-attribute
                 'default nil
                 :font "CaskaydiaCoveNerdFont"
                 :height 137
                 :inherit nil
                 :weight 'normal)
                (load-theme 'solarized-dark t))
              (unless (window-system)
                (load-theme 'material t))))

(use-package solarized-theme
  :ensure t
  :if (window-system)
  :custom
  (solarized-use-variable-pitch nil))

(use-package material-theme
  :ensure t
  :if (not window-system))

(use-package ligature
  :ensure t
  :config
  ;; Enable the "www" ligature in every possible major mode
  (ligature-set-ligatures 't '("www"))
  ;; Enable traditional ligature support in eww-mode, if the
  ;; `variable-pitch' face supports it
  (ligature-set-ligatures 'eww-mode '("ff" "fi" "ffi"))
  ;; Enable all Cascadia and Fira Code ligatures in programming modes
  (ligature-set-ligatures 'prog-mode
                        '(("=" (rx (+ (or ">" "<" "|" "/" "~" ":" "!" "="))))
                          (";" (rx (+ ";")))
                          ("&" (rx (+ "&")))
                          ("!" (rx (+ (or "=" "!" "\." ":" "~"))))
                          ("?" (rx (or ":" "=" "\." (+ "?"))))
                          ("%" (rx (+ "%")))
                          ("|" (rx (+ (or ">" "<" "|" "/" ":" "!" "}" "\]"
                                          "-" "=" ))))
                          ("\\" (rx (or "/" (+ "\\"))))
                          ("+" (rx (or ">" (+ "+"))))
                          (":" (rx (or ">" "<" "=" "//" ":=" (+ ":"))))
                          ("/" (rx (+ (or ">"  "<" "|" "/" "\\" "\*" ":" "!"
                                          "="))))
                          ("\." (rx (or "=" "-" "\?" "\.=" "\.<" (+ "\."))))
                          ("-" (rx (+ (or ">" "<" "|" "~" "-"))))
                          ("*" (rx (or ">" "/" ")" (+ "*"))))
                          ("w" (rx (+ "w")))
                          ("<" (rx (+ (or "\+" "\*" "\$" "<" ">" ":" "~"  "!"
                                          "-"  "/" "|" "="))))
                          (">" (rx (+ (or ">" "<" "|" "/" ":" "=" "-"))))
                          ("#" (rx (or ":" "=" "!" "(" "\?" "\[" "{" "_(" "_"
                                       (+ "#"))))
                          ("~" (rx (or ">" "=" "-" "@" "~>" (+ "~"))))
                          ("_" (rx (+ (or "_" "|"))))
                          ("0" (rx (and "x" (+ (in "A-F" "a-f" "0-9")))))
                          "Fl"  "Tl"  "fi"  "fj"  "fl"  "ft"
                          "{|"  "[|"  "]#"  "(*"  "}#"  "$>"  "^="))
  ;; Enables ligature checks globally in all buffers. You can also do it
  ;; per mode with `ligature-mode'.
  (global-ligature-mode t))

(use-package emojify
  :ensure t
  :init
  (setq emojify-download-emojis-p t)
  (global-emojify-mode t)
  :config
  (use-package company-emoji
    :ensure t
    :config
    (add-to-list 'company-backends 'company-emoji)))

(provide 'ui)
;;; ui.el ends here

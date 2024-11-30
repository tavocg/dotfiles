;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; ui
(setq display-line-numbers-type t)
(setq doom-font (font-spec :family "JetBrainsMono" :size 11.0))
(setq doom-theme 'doom-material)
(setq default-frame-alist '((undecorated . t)))
(add-to-list 'default-frame-alist '(fullscreen . maximized))
(scroll-bar-mode 0)

;; org mode
;; (setq org-directory "~/org/")
(setq org-agenda-files '("~/Documents/agenda"))
(after! org (plist-put org-format-latex-options :scale 3.2))
(dolist (mode '(org-mode-hook
                org-agenda-mode-hook
                dired-mode-hook
                term-mode-hook
                shell-mode-hook
                eshell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

;; binds
(global-set-key (kbd "M-p")
                (lambda ()
                  (interactive)
                  (pass)))

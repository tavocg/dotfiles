;;; init.el --- Tavo's emacs config
;;; Commentary:
;;; Prerequisites:
;;;     mkdir -p ~/.config/emacs/elpa/gnupg
;;;     gpg --homedir ~/.config/emacs/elpa/gnupg --keyserver hkp://keyserver.ubuntu.com --recv-keys 645357D2883A0966
;;;     find ~/.config/emacs/elpa/gnupg -type d -exec chmod 700 {} \;
;;;     find ~/.config/emacs/elpa/gnupg -type f -exec chmod 600 {} \;
;;;     mkdir -p ~/.local/share/emacs/backup
;;;     mkdir -p ~/.local/share/emacs/lock
;;; After:
;;;     M-x package-install RET nerd-icons
;;;     M-x package-install RET all-the-icons
;;;     M-x nerd-icons-install-fonts
;;;     M-x all-the-icons-install-fonts
;;; Code:

;; Preferences
(setq scroll-step 1 scroll-conservatively  10000)
(scroll-bar-mode 0)
(menu-bar-mode 0)
(tool-bar-mode 0)
(global-display-line-numbers-mode 1)
(setq-default display-line-numbers-width 3)
(delete-selection-mode 1)
(electric-indent-mode 0)
(global-hl-line-mode 1)
(electric-pair-mode 1)
(column-number-mode)
(setq backup-directory-alist '((".*" . "~/.local/share/emacs/backup")))
(setq temporary-file-directory "~/.local/share/emacs/lock")
(dolist (mode '(org-mode-hook term-mode-hook shell-mode-hook eshell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

;; Package manager
(require 'package)
(setq package-archives '(("melpa" . "https://melpa.org/packages/")))
(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))
(unless (package-installed-p 'use-package)
  (package-install 'use-package))
(require 'use-package)
(setq use-package-always-ensure t)

(dolist (pkg '(all-the-icons nerd-icons markdown-mode markdown-preview-mode))
  (unless (package-installed-p pkg)
    (package-install pkg)))

;; Theming
(set-face-attribute 'default nil :font "JetBrains Mono")
(add-to-list 'default-frame-alist '(font . "JetBrains Mono-10"))
(use-package doom-themes
  :init (load-theme 'doom-material t))

(use-package paren :ensure nil
  :init (setq show-paren-delay 0)
  :config (show-paren-mode +1))

(use-package dashboard
  :ensure t
  :init
  (setq dashboard-set-heading-icons t)
  (setq dashboard-set-file-icons t)
  (setq dashboard-center-content t)
  (setq dashboard-vertically-center-content t)
  (setq dashboard-startupify-list '(dashboard-insert-banner
                                    dashboard-insert-banner-title
                                    dashboard-insert-items))
  (setq dashboard-startup-banner "~/.config/emacs/banner.txt")
  (setq dashboard-banner-logo-title "✨ M'illumino d'immenso ✨")
  (setq dashboard-items '((recents  . 5)))
  :config
  (dashboard-setup-startup-hook))
(setq initial-buffer-choice (lambda () (get-buffer-create dashboard-buffer-name)))

(use-package evil
  :init
  (setq evil-want-integration t
	evil-want-keybinding nil
	evil-vsplit-window-right t
	evil-split-window-below t)
  :config
  (evil-mode 1))
(use-package evil-collection
  :after evil
  :config
  (evil-collection-init))

;; Org Mode
(use-package org-bullets)
(add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))
(add-hook 'org-mode-hook 'org-indent-mode)
(custom-set-faces
  '(org-level-1 ((t (:inherit outline-1 :height 1.7))))
  '(org-level-2 ((t (:inherit outline-2 :height 1.6))))
  '(org-level-3 ((t (:inherit outline-3 :height 1.5))))
  '(org-level-4 ((t (:inherit outline-4 :height 1.4))))
  '(org-level-5 ((t (:inherit outline-5 :height 1.3))))
  '(org-level-6 ((t (:inherit outline-5 :height 1.2))))
  '(org-level-7 ((t (:inherit outline-5 :height 1.1)))))
(setq org-highlight-latex-and-related '(latex script entities))
(setq org-format-latex-options (plist-put org-format-latex-options :scale 1.35))
(setq org-startup-with-latex-preview t)

;; Keybinds
(use-package general
  :config
  (general-evil-setup)
  (general-create-definer tavo/leader-keys
    :states '(normal insert visual emacs)
    :keymaps 'override
    :prefix "SPC"
    :global-prefix "M-SPC")
  (tavo/leader-keys
    "c" '(comment-line :wk "Comment lines")
    "p" 'org-latex-preview))
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)
(global-set-key (kbd "C-c") 'kill-this-buffer)
(global-set-key (kbd "C-=") 'text-scale-increase)
(global-set-key (kbd "C-+") 'text-scale-increase)
(global-set-key (kbd "C--") 'text-scale-decrease)

(setq custom-file (concat user-emacs-directory "custom.el"))
(load custom-file 'noerror)

;; init.el ends here

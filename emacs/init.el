;;; init.el --- Tavo's emacs config
;;; Commentary:
;;; Code:

;; Preferences
(menu-bar-mode 0)
(tool-bar-mode 0)
(scroll-bar-mode 0)
(global-display-line-numbers-mode 1)
(setq-default display-line-numbers-width 3)
(global-hl-line-mode 1)
(set-face-attribute 'default nil
		    :font "JetBrains Mono"
		    :weight 'normal)
(set-face-attribute 'fixed-pitch nil
		    :font "JetBrains Mono"
		    :weight 'normal)
(set-face-attribute 'font-lock-comment-face nil
		    :slant 'italic)
(add-to-list 'default-frame-alist '(font . "JetBrains Mono-10"))
(setq scroll-step 1
      scroll-conservatively  10000)
(delete-selection-mode 1)
(electric-indent-mode -1)
(electric-pair-mode 1)
(setq backup-directory-alist '((".*" . "~/.local/share/emacs/backup")))

;; Package manager
(require 'package)
(setq package-archives
      '(("melpa" . "https://melpa.org/packages/")
	("elpa" . "https://elpa.gnu.org/packages/")))
(package-initialize)
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(eval-when-compile (require 'use-package))

;; Packages
(dolist (pkg '(gruvbox-theme evil evil-collection nerd-icons all-the-icons all-the-icons-dired projectile))
  (unless (package-installed-p pkg)
    (package-install pkg)))

;; evil
(use-package evil
  :demand t
  :bind (("<escape>" . keyboard-escape-quit))
  :init
  (setq evil-want-integration t
	evil-want-keybinding nil
	evil-vsplit-window-right t
	evil-split-window-below t
	evil-undo-system 'undo-redo)
  (evil-mode))
(use-package evil-collection
  :after evil
  :config
  (add-to-list 'evil-collection-mode-list 'help)
  (evil-collection-init))

;; Theming
(use-package gruvbox-theme
  :config
  (load-theme 'gruvbox-dark-hard t))
(use-package all-the-icons
  :ensure t
  :if (display-graphic-p))
(use-package all-the-icons-dired
  :hook (dired-mode . (lambda () (all-the-icons-dired-mode t))))
(use-package dashboard
  :ensure t
  :init
  (setq dashboard-set-heading-icons t)
  (setq dashboard-set-file-icons t)
  (setq dashboard-center-content t)
  (setq dashboard-startupify-list '(dashboard-insert-banner
                                    dashboard-insert-banner-title
                                    dashboard-insert-newline
                                    dashboard-insert-items
                                    dashboard-insert-newline))
  (setq dashboard-projects-backend 'projectile)
  (setq dashboard-startup-banner "~/.config/emacs/default.txt")
  (setq dashboard-banner-logo-title "✨ M'illumino d'immenso ✨")
  (setq dashboard-items '((projects . 5)
                          (recents  . 5)))
  :config
  (dashboard-setup-startup-hook))
(setq initial-buffer-choice (lambda () (get-buffer-create dashboard-buffer-name)))

;; Keybinds
(global-set-key (kbd "C-<tab>") 'next-buffer)
(global-set-key (kbd "C-c") 'kill-this-buffer)
(global-set-key (kbd "C-=") 'text-scale-increase)
(global-set-key (kbd "C-+") 'text-scale-increase)
(global-set-key (kbd "C--") 'text-scale-decrease)

(use-package flycheck
  :ensure t
  :defer t
  :diminish
  :init (global-flycheck-mode))

(setq custom-file (concat user-emacs-directory "custom.el"))
(load custom-file 'noerror)

;;; init.el ends here

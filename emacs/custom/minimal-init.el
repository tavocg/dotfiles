;; Sane defaults
(setq inhibit-startup-message t)
(scroll-bar-mode -1)
(tool-bar-mode -1)
(tooltip-mode -1)
(fringe-mode 0)
(menu-bar-mode -1)
(global-display-line-numbers-mode 1)
(tab-bar-mode 1)
(tab-line-mode 1)

;; Clean up
(setq custom-file (concat user-emacs-directory "custom.el"))
(load custom-file 'noerror)

;; Customization
(set-frame-font "JetBrainsMono-10" nil t)

(setq modus-themes-common-palette-overrides
      '(
        ;; Tab bar
        (bg-tab-bar bg-main)
        (bg-tab-current bg-cyan-intense)
        (bg-tab-other bg-inactive)
        ;; Mode line
        (bg-mode-line-active bg-cyan-intense)
        (fg-mode-line-active fg-main)
        (border-mode-line-active cyan-intense)
        ))

(load-theme 'modus-vivendi t)

;; Package manager
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))
(unless (package-installed-p 'use-package)
  (package-install 'use-package))
(require 'use-package)
(setq use-package-always-ensure t)

;; Evil
(use-package evil
             :init
             (setq evil-want-integration t)
             (setq evil-want-keybinding nil)
             (setq evil-want-C-u-scroll t)
             (setq evil-want-C-i-jump nil)
             :config
             (evil-mode 1)
             (define-key evil-insert-state-map (kbd "C-g") 'evil-normal-state)
             (define-key evil-insert-state-map (kbd "C-h") 'evil-delete-backward-char-and-join)
             (evil-global-set-key 'motion "j" 'evil-next-visual-line)
             (evil-global-set-key 'motion "k" 'evil-previous-visual-line)
             (evil-set-initial-state 'messages-buffer-mode 'normal)
             (evil-set-initial-state 'dashboard-mode 'normal))

(use-package evil-collection
             :after evil
             :config
             (evil-collection-init))

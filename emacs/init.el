;; MELPA

(require 'package)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))
(package-initialize)
(package-refresh-contents)

;; Install packages

(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(unless (package-installed-p 'which-key)
  (package-install 'which-key))

(unless (package-installed-p 'undo-tree)
  (package-install 'undo-tree))
(require 'undo-tree)
(global-undo-tree-mode)

(unless (package-installed-p 'goto-chg)
  (package-install 'goto-chg))

(unless (package-installed-p 'evil-collection)
  (package-install 'evil-collection))
(setq evil-want-keybinding nil)

(unless (package-installed-p 'evil)
  (package-install 'evil))
(require 'evil)
(evil-mode 1)
(evil-set-undo-system 'undo-redo)

(unless (package-installed-p 'general)
  (package-install 'general))

;; Configure packages

(use-package evil-collection
    :after evil
    :config
    (setq evil-collection-mode-list '(dashboard dired ibuffer))
    (evil-collection-init))

(use-package evil
    :init
    (setq evil-want-integration t)
    (setq evil-want-keybinding t)
    (setq evil-vsplit-window-right t)
    (setq evil-split-window-below t)
    (evil-mode))

;; Keybinds

(use-package which-key
  :init
    (which-key-mode 1)
  :config
  (setq which-key-side-window-location 'bottom
	  which-key-sort-order #'which-key-key-order-alpha
	  which-key-sort-uppercase-first nil
	  which-key-add-column-padding 1
	  which-key-max-display-columns nil
	  which-key-min-display-lines 6
	  which-key-side-window-slot -10
	  which-key-side-window-max-height 0.25
	  which-key-idle-delay 0.8
	  which-key-max-description-length 25
	  which-key-allow-imprecise-window-fit t
	  which-key-separator " → " ))

(use-package general
  :config
  (general-evil-setup)
  (general-create-definer leader-keys
    :states '(normal insert visual emacs)
    :keymaps 'override
    :prefix "SPC"
    :global-prefix "M-SPC")

  (leader-keys
   "bn" '(next-buffer :wk "Next buffer")
   "bk" '(kill-this-buffer :wk "Kill this buffer")))

;; Theming

(menu-bar-mode 0)
(tool-bar-mode 0)
(scroll-bar-mode 0)
(global-display-line-numbers-mode 1)
(global-visual-line-mode t)

(set-face-attribute 'default nil
    :font "JetBrains Mono"
    :height 110
    :weight 'normal)

(set-face-attribute 'fixed-pitch nil
    :font "JetBrains Mono"
    :height 110
    :weight 'normal)

(set-face-attribute 'font-lock-comment-face nil
    :slant 'italic)

(add-to-list 'default-frame-alist '(font . "JetBrains Mono-10"))

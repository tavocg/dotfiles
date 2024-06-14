;;; init.el --- Tavo's emacs config
;;; Commentary:
;;; Prerequisites:
;;;     gpg --homedir ~/.config/emacs/elpa/gnupg --keyserver hkp://keyserver.ubuntu.com --recv-keys 645357D2883A0966
;;;     mkdir -p "~/.local/share/emacs/backup"
;;;     mkdir -p "~/.local/share/emacs/lock"
;;; After:
;;;     M-x nerd-icons-install-fonts
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
(setq temporary-file-directory "~/.local/share/emacs/lock")

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
(dolist (pkg '(evil evil-collection all-the-icons nerd-icons all-the-icons-dired
               flycheck projectile markdown-mode markdown-preview-mode
               company company-box general neotree highlight-indent-guides
               paren doom-themes doom-modeline))
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
(load-theme 'doom-gruvbox t)
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
(add-hook 'prog-mode-hook 'highlight-indent-guides-mode)
(setq highlight-indent-guides-method 'character)
(setq highlight-indent-guides-character ?⎸)
(use-package paren
  :ensure nil
  :init (setq show-paren-delay 0)
  :config (show-paren-mode +1))
(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1)
  :config
  ;; M-x nerd-icons-install-fonts
  (setq doom-modeline-height 35
        doom-modeline-bar-width 5
        doom-modeline-persp-name t
        doom-modeline-persp-icon t))

;; Keybinds
(global-set-key (kbd "C-<tab>") 'next-buffer)
(global-set-key (kbd "C-c") 'kill-this-buffer)
(global-set-key (kbd "C-=") 'text-scale-increase)
(global-set-key (kbd "C-+") 'text-scale-increase)
(global-set-key (kbd "C--") 'text-scale-decrease)

(use-package general
  :config
  (general-evil-setup)

  (general-create-definer tavo/leader-keys
    :states '(normal insert visual emacs)
    :keymaps 'override
    :prefix "SPC"
    :global-prefix "M-SPC")

  (tavo/leader-keys
    "c" '(comment-line :wk "Comment lines")))

;; Extra
(use-package company
  :defer 2
  :diminish
  :custom
  (company-begin-commands '(self-insert-command))
  (company-idle-delay .1)
  (company-minimum-prefix-length 2)
  (company-show-numbers t)
  (company-tooltip-align-annotations 't)
  (global-company-mode t))
(use-package company-box
  :after company
  :diminish
  :hook (company-mode . company-box-mode))
(use-package flycheck
  :ensure t
  :defer t
  :diminish
  :init (global-flycheck-mode))

(use-package neotree
  :config
  (setq neo-smart-open t
        neo-show-hidden-files t
        neo-window-width 40
        neo-window-fixed-size nil
        inhibit-compacting-font-caches t
        projectile-switch-project-action 'neotree-projectile-action) 
        (add-hook 'neo-after-create-hook
           #'(lambda (_)
               (with-current-buffer (get-buffer neo-buffer-name)
                 (setq truncate-lines t)
                 (setq word-wrap nil)
                 (make-local-variable 'auto-hscroll-mode)
                 (setq auto-hscroll-mode nil)))))

(setq neo-smart-open t)
(add-hook 'neo-after-create-hook
          (lambda (&rest _) (display-line-numbers-mode -1)))
(setq neo-theme nil)

(setq custom-file (concat user-emacs-directory "custom.el"))
(load custom-file 'noerror)

;;; init.el ends here

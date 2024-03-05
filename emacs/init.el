;;; init.el --- tavo's emacs configs
;;; Commentary:
;;; Code:

;; MELPA

(require 'package)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))
(package-initialize)
;;(package-refresh-contents)

;; Install packages

(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(unless (package-installed-p 'gruvbox-theme)
  (package-install 'gruvbox-theme))

(unless (package-installed-p 'nerd-icons)
  (package-install 'nerd-icons))

(unless (package-installed-p 'rainbow-mode)
  (package-install 'rainbow-mode))

(unless (package-installed-p 'dashboard)
  (package-install 'dashboard))

(unless (package-installed-p 'undo-tree)
  (package-install 'undo-tree))
(require 'undo-tree)
(global-undo-tree-mode)

(unless (package-installed-p 'goto-chg)
  (package-install 'goto-chg))

(unless (package-installed-p 'flycheck)
  (package-install 'flycheck))

(unless (package-installed-p 'ess)
  (package-install 'ess))

(unless (package-installed-p 'all-the-icons)
  (package-install 'all-the-icons))

(unless (package-installed-p 'all-the-icons-dired)
  (package-install 'all-the-icons-dired))

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

(use-package all-the-icons
    :ensure t
    :if (display-graphic-p))

(use-package all-the-icons-dired
  :hook (dired-mode . (lambda () (all-the-icons-dired-mode t))))

(use-package eshell-syntax-highlighting
  :after esh-mode
  :config
  (eshell-syntax-highlighting-global-mode +1))

(use-package rainbow-mode
  :hook org-mode prog-mode)

(use-package flycheck
  :ensure t
  :defer t
  :diminish
  :init (global-flycheck-mode))

;; Functions

(defvar shell-pop-last-buffer nil)
(defvar shell-pop-last-window nil)
(defvar shell-pop-window-height 30) ; percentage for shell-buffer window height
(defvar shell-pop-window-position "bottom")

(defvar shell-pop-internal-mode "shell")
(defvar shell-pop-internal-mode-buffer "*shell*")
(defvar shell-pop-internal-mode-func '(lambda () (shell)))
(defvar shell-pop-internal-mode-shell "/bin/bash")

(defvar shell-pop-internal-mode-list
  (list
    ; mode, buffer, function
    '("shell"     "*shell*"     '(lambda () (shell)))
    '("terminal"  "*terminal*"  '(lambda () (term shell-pop-internal-mode-shell)))
    '("ansi-term" "*ansi-term*" '(lambda () (ansi-term shell-pop-internal-mode-shell)))
    '("eshell"    "*eshell*"    '(lambda () (eshell)))))

(defun shell-pop-set-window-height (number)
  (interactive "nInput the number for the percentage of \
selected window height (10-100): ")
  (setq shell-pop-window-height number))

(defun shell-pop-set-window-position (position)
  (interactive "sInput the position for shell-pop (top|bottom): ")
  (setq shell-pop-window-position position))

(defun shell-pop-set-internal-mode (mode)
  (interactive "sInput your favorite mode (shell|terminal|ansi-term|eshell): ")
  (if (catch 'found
        (dolist (l shell-pop-internal-mode-list)
          (if (string-match mode (car l))
              (progn
                (setq shell-pop-internal-mode-buffer (nth 1 l))
                (setq shell-pop-internal-mode-func (nth 2 l))
                (throw 'found t)))))
      t
    nil))

(defun shell-pop-set-internal-mode-shell (shell)
  (interactive (list (read-from-minibuffer "Input your favorite shell:"
                                           shell-pop-internal-mode-shell)))
  (setq shell-pop-internal-mode-shell shell))

(defun shell-pop ()
  (interactive)
  (if (equal (buffer-name) shell-pop-internal-mode-buffer)
      (shell-pop-out)
    (shell-pop-up)))

(defun shell-pop-up ()
  (let ((w (get-buffer-window shell-pop-internal-mode-buffer)))
    (if w
        (select-window w)
      (progn
        ; save shell-pop-last-buffer and shell-pop-last-window to return
          (setq shell-pop-last-buffer (buffer-name))
          (setq shell-pop-last-window (selected-window))
          (if (not (eq shell-pop-window-height 100))
              (progn
                (split-window (selected-window)
                              (if (string= shell-pop-window-position "bottom")
                                  (round (* (window-height)
                                            (/ (- 100 shell-pop-window-height) 100.0)))
                                (round (* (window-height) (/ shell-pop-window-height 100.0)))))
                (if (string= shell-pop-window-position "bottom")
                    (other-window 1))))
          (if (not (get-buffer shell-pop-internal-mode-buffer))
              (funcall (eval shell-pop-internal-mode-func))
            (switch-to-buffer shell-pop-internal-mode-buffer))))))

(defun shell-pop-out ()
  (if (not (eq shell-pop-window-height 100))
      (progn
        (delete-window)
        (if (string= shell-pop-window-position "bottom")
            (select-window shell-pop-last-window))))
  (switch-to-buffer shell-pop-last-buffer))

(require 'windmove)

(defun buf-move-up ()
  (interactive)
  (let* ((other-win (windmove-find-other-window 'up))
	 (buf-this-buf (window-buffer (selected-window))))
    (if (null other-win)
        (error "No window above this one")
      (set-window-buffer (selected-window) (window-buffer other-win))
      (set-window-buffer other-win buf-this-buf)
      (select-window other-win))))

(defun buf-move-down ()
  (interactive)
  (let* ((other-win (windmove-find-other-window 'down))
	 (buf-this-buf (window-buffer (selected-window))))
    (if (or (null other-win) 
            (string-match "^ \\*Minibuf" (buffer-name (window-buffer other-win))))
        (error "No window under this one")
      (set-window-buffer (selected-window) (window-buffer other-win))
      (set-window-buffer other-win buf-this-buf)
      (select-window other-win))))

(defun buf-move-left ()
  (interactive)
  (let* ((other-win (windmove-find-other-window 'left))
	 (buf-this-buf (window-buffer (selected-window))))
    (if (null other-win)
        (error "No left split")
      (set-window-buffer (selected-window) (window-buffer other-win))
      (set-window-buffer other-win buf-this-buf)
      (select-window other-win))))

(defun buf-move-right ()
  (interactive)
  (let* ((other-win (windmove-find-other-window 'right))
	 (buf-this-buf (window-buffer (selected-window))))
    (if (null other-win)
        (error "No right split")
      (set-window-buffer (selected-window) (window-buffer other-win))
      (set-window-buffer other-win buf-this-buf)
      (select-window other-win))))

(defun reload-init-file ()
  (interactive)
  (load-file user-init-file)
  (load-file user-init-file))

;; Keybinds

(setq scroll-step            1
      scroll-conservatively  10000)

(use-package general
  :config
  (general-evil-setup)

  (general-create-definer leader-keys
    :states '(normal insert visual emacs)
    :keymaps 'override
    :prefix "C-w"
    :global-prefix "C-w")

  (leader-keys
   ;; Splits
   "h" '(evil-window-left :wk "Buffer focus left")
   "j" '(evil-window-down :wk "Buffer focus down")
   "k" '(evil-window-up :wk "Buffer focus up")
   "l" '(evil-window-right :wk "Buffer focus right")
   "H" '(buf-move-left :wk "Buffer focus left")
   "J" '(buf-move-down :wk "Buffer focus down")
   "K" '(buf-move-up :wk "Buffer focus up")
   "L" '(buf-move-right :wk "Buffer focus right")
   ;; Common
   "f" '(find-file :wk "Find file")
   "c" '(comment-line :wk "Comment lines")
   "rr" '(reload-init-file :wk "Reload config")))

(global-set-key (kbd "C-g") 'shell-pop)
(global-set-key (kbd "C-<tab>") 'next-buffer)
(global-set-key (kbd "C-c") 'kill-this-buffer)
(global-set-key (kbd "C-=") 'text-scale-increase)
(global-set-key (kbd "C-+") 'text-scale-increase)
(global-set-key (kbd "C--") 'text-scale-decrease)

;; Theming

(menu-bar-mode 0)
(tool-bar-mode 0)
(scroll-bar-mode 0)
(global-display-line-numbers-mode 1)
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

(load-theme 'gruvbox-dark-hard t)

(use-package dashboard
  :ensure t 
  :init
  (setq initial-buffer-choice 'dashboard-open)
  (setq dashboard-set-heading-icons t)
  (setq dashboard-set-file-icons t)
  (setq dashboard-center-content nil)
  (setq dashboard-banner-logo-title "✨ M'illumino d'immenso ✨")
  (setq dashboard-startup-banner "~/.config/emacs/default.txt")
  (setq dashboard-items '((recents . 5))))

(custom-set-faces
 '(hl-line ((t (:extend t :background "#3a3a3a"))))
 '(line-number ((t (:background "#1d2021" :foreground "#7c6f64")))))

;;; init.el ends here

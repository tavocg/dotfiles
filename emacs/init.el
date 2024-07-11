;;; init.el --- Tavo's emacs config
;;; Commentary:
;;;     M-x nerd-icons-install-fonts
;;;     M-x all-the-icons-install-fonts
;;; Code:

;; --- Prerequisites for elpa----
; (unless (file-directory-p "~/.config/emacs/elpa/gnupg")
;   (make-directory "~/.config/emacs/elpa/gnupg")
;   (shell-command "gpg --homedir ~/.config/emacs/elpa/gnupg --keyserver hkp://keyserver.ubuntu.com --recv-keys 645357D2883A0966")
;   (shell-command "find ~/.config/emacs/elpa/gnupg -type d -exec chmod 700 {} \;")
;   (shell-command "find ~/.config/emacs/elpa/gnupg -type f -exec chmod 600 {} \;"))
;; ---

;; --- Customization
(defvar custom-font  "JetBrains Mono-10.5")
(defvar custom-theme 'doom-gruvbox)

(custom-set-faces
  ;; '(hl-line ((t (:background "gray20" :underline nil))))
  '(mode-line-inactive ((t (:background "#282828" :foreground "#504945" :box nil))))
  '(mode-line ((t (:background "#282828" :foreground "#a89984" :box nil))))
  '(org-block ((t (:background "#282828"))))
  '(org-quote ((t (:background "#282828"))))
  '(default ((t (:background "#1d2021")))))

;; --- Global Preferences ---
(setq custom-file (concat user-emacs-directory "custom.el"))
(load custom-file 'noerror)
(setq warning-minimum-level :error)
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

(dolist (dir '("~/.local/share/emacs/auto-save/"
               "~/.local/share/emacs/backup/"
               "~/.local/share/emacs/lock/"
               "~/.local/share/emacs/tmp/"))
  (unless (file-directory-p dir)
    (make-directory dir t)))

(setq lock-file-name-transforms '(("\\`/.*/\\([^/]+\\)\\'" "~/.local/share/emacs/lock/\\1" t)))
(setq auto-save-file-name-transforms '((".*" "~/.local/share/emacs/auto-save/" t)))
(setq backup-directory-alist '((".*" . "~/.local/share/emacs/backup/")))
(setq temporary-file-directory "~/.local/share/emacs/tmp/")

(dolist (mode '(org-mode-hook
                 org-agenda-mode-hook
                 dired-mode-hook
                 term-mode-hook
                 shell-mode-hook
                 eshell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

;; --- Package manager ---
(require 'package)

(if (eq system-type 'android)
  (setq package-archives '(("melpa" . "https://melpa.org/packages/")))
  (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t))

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

;; --- my/functions ---
(defun my/visual-fill ()
  (setq visual-fill-column-width 100
        visual-fill-column-center-text t)
  (visual-fill-column-mode 1))

(defun my/hide-modeline ()
  (setq-local mode-line-format nil))

(defun my/insert-latex-equation ()
  "Insert a LaTeX equation environment."
  (interactive)
  (insert "\\begin{equation}\\label{}\\begin{aligned}\n\n\\end{aligned}\\end{equation}")
  (backward-char 28))

(defun my/org-open-pdf-in-zathura ()
  "Open the corresponding PDF file in Zathura for the current Org document."
  (interactive)
  (when (and (buffer-file-name)
             (string-equal (file-name-extension (buffer-file-name)) "org"))
    (let* ((org-file (buffer-file-name))
           (pdf-file (concat (file-name-sans-extension org-file) ".pdf"))
           (pdf-file (expand-file-name pdf-file)))
      (if (file-exists-p pdf-file)
          (shell-command (concat "zathura " (shell-quote-argument pdf-file)))
        (message "PDF file does not exist: %s" pdf-file)))))

(defun org-babel-execute:pic (body params)
  "Evaluate pic source code to create a png file
  Use with: '#+begin_src pic :file example.png'
  Optional: ':exports none' to avoid showing the source
  Optional: ':results silent' to avoid outputting #+RESULTS:"
  (or (cdr (assoc :file params))
      (error "You must specify output file :file [IMAGE.png]"))
  (let* ((quoted-text (replace-regexp-in-string "'" "'\\\\''" body))
	 (body-with-v (replace-regexp-in-string "\\\\v" "VERTICAL" quoted-text))
	 (cmd (concat "echo '" body-with-v "' | "
                      "preconv | sed \'s/VERTICAL/\\\\v/g\' | "
                      "pic2graph -density 200 2>/dev/null"))
         (image (shell-command-to-string cmd)))
    image))
(setq org-babel-default-header-args:pic '((:results . "file")))

;; --- Modes
(use-package evil
  :init
  (setq evil-want-integration t
        evil-want-keybinding nil
        evil-vsplit-window-right t
        evil-split-window-below t
        evil-undo-system 'undo-redo)
  :config
  (evil-mode 1))

(use-package evil-collection
  :after evil
  :config
  (evil-collection-init))

(use-package dashboard
  :ensure t
  :init
  (setq dashboard-startup-banner "~/.config/emacs/banner.txt")
  (setq dashboard-set-heading-icons t)
  (setq dashboard-set-file-icons t)
  (setq dashboard-items '((agenda    . 5) (bookmarks . 5) (recents   . 20)))
  (setq dashboard-startupify-list '(dashboard-insert-banner dashboard-insert-items))
  :config
  (dashboard-setup-startup-hook))
(setq initial-buffer-choice (lambda () (get-buffer-create dashboard-buffer-name)))
(add-hook 'dashboard-mode-hook 'my/visual-fill)
(add-hook 'dashboard-mode-hook 'my/hide-modeline)
(add-hook 'server-after-make-frame-hook 'dashboard-refresh-buffer)

(if (eq system-type 'android)
    (message "Android device, ignoring visual-fill") ;; Android
  (progn ;; Everywhere else
    (use-package pdf-tools
      :defer t
      :commands (pdf-loader-install)
      :mode "\\.pdf\\'"
      :bind (:map pdf-view-mode-map
		  ("j" . pdf-view-next-line-or-next-page)
		  ("k" . pdf-view-previous-line-or-previous-page)
		  ("C-=" . pdf-view-enlarge)
		  ("C--" . pdf-view-shrink))
      :init (pdf-loader-install)
      :config (add-to-list 'revert-without-query ".pdf"))
    (add-hook 'pdf-view-mode-hook #'(lambda () (interactive) (display-line-numbers-mode 0)))
    (add-hook 'pdf-view-mode-hook 'my/hide-modeline)))

(use-package dired
  :ensure nil
  :commands (dired dired-jump)
  :bind (("C-x C-j" . dired-jump))
  :custom ((dired-listing-switches "-AghoD --group-directories-first --color=auto --time-style=iso"))
  :config
  (evil-collection-define-key 'normal 'dired-mode-map
    "h" 'dired-single-up-directory
    "l" 'dired-single-buffer))
(use-package dired-single)
(use-package all-the-icons-dired
  :hook (dired-mode . all-the-icons-dired-mode))
(use-package dired-open
  :config
  ;; Doesn't work as expected!
  ;;(add-to-list 'dired-open-functions #'dired-open-xdg t)
  (setq dired-open-extensions '(("png" . "sxiv")
                                ("jpg" . "sxiv")
                                ("gif" . "sxiv")
                                ("webp" . "sxiv")
                                ("pdf" . "zathura")
                                ("mp4" . "mpv")
                                ("mkv" . "mpv"))))
(use-package dired-hide-dotfiles
  :hook (dired-mode . dired-hide-dotfiles-mode)
  :config
  (evil-collection-define-key 'normal 'dired-mode-map
    "." 'dired-hide-dotfiles-mode))

(if (eq system-type 'android)
  (set-face-attribute 'default nil :height 140) ;; Android
  (progn ;; Everywhere else
    (set-face-attribute 'default nil :font custom-font)
    (add-to-list 'default-frame-alist `(font . ,custom-font))))

(use-package doom-themes
  :init (load-theme custom-theme t))

;; --- Org Mode ---
(setq org-startup-folded t)
(require 'org-tempo)

(setq org-src-preserve-indentation t
      org-edit-src-content-indentation 0
      org-confirm-babel-evaluate nil)

(require 'ob)
(setq org-babel-load-languages '((pic . t)))
(add-to-list 'org-babel-tangle-lang-exts '("pic" . "pic"))

(if (eq system-type 'android)
  (message "Android device, ignoring visual-fill") ;; Android
  (add-hook 'org-mode-hook 'my/visual-fill)) ;; Everywhere else

(add-hook 'org-mode-hook
          (lambda ()
            (setq-local electric-pair-inhibit-predicate
                        `(lambda (c)
                           (if (char-equal c ?<) t (,electric-pair-inhibit-predicate c))))
            (setq-local org-src-tab-acts-natively t org-src-tab-indentation 4)
            (local-set-key (kbd "C-c e") 'my/insert-latex-equation)))

(use-package org-bullets)
(custom-set-faces
  '(org-level-1 ((t (:inherit outline-1 :height 1.7))))
  '(org-level-2 ((t (:inherit outline-2 :height 1.5))))
  '(org-level-3 ((t (:inherit outline-3 :height 1.3))))
  '(org-level-4 ((t (:inherit outline-4 :height 1.1)))))

(if (eq system-type 'android)
  (progn ;; Android custom org-latex-preview
    (setq org-format-latex-options (plist-put org-format-latex-options :scale 100))
    (setq org-preview-latex-process-alist
          '((dvipngweb
              :programs ("curl")
              :description "dvi > png"
              :message "you need to install the programs: curl."
              :image-input-type "dvi"
              :image-output-type "png"
              :image-size-adjust (1.0 . 1.0)
              :latex-compiler ("curl -F \"file=@%f\" -F \"type=tex\" 0.0.0.0:8000 && curl 0.0.0.0:8000/tmp.dvi -o %O")
              :image-converter ("curl -F \"file=@%f\" -F \"type=dvi\" -F \"dpi=%D\" -o %O 0.0.0.0:8000 && curl 0.0.0.0:8000/tmp.png -o %O"))))
    (setq org-latex-create-formula-image-program 'dvipngweb))
  (progn ;; Everywhere else
    (unless (package-installed-p 'queue)
      (package-install 'queue))
    (use-package citeproc)
    (require 'oc-csl)))

(setq org-format-latex-options (plist-put org-format-latex-options :scale 1.35))
(setq org-latex-toc-command "\\clearpage \\tableofcontents \\clearpage")
(setq org-highlight-latex-and-related '(latex script entities))
;; (setq org-startup-with-latex-preview t)

(unless (file-directory-p "~/.local/share/emacs/ltximg/")
  (make-directory "~/.local/share/emacs/ltximg/"))
(setq org-preview-latex-image-directory "~/.local/share/emacs/ltximg/")

(setq org-agenda-files (quote ("~/Documents/agenda/archive.org"
                               "~/Documents/agenda/agenda.org")))

;; --- Keybinds ---
(use-package general
  :config
  (general-evil-setup)
  (general-create-definer my/leader-keys
    :states '(normal insert visual emacs)
    :keymaps 'override
    :prefix "SPC"
    :global-prefix "M-SPC")
  (my/leader-keys
    "c" '(comment-line :wk "Comment lines")
    "p" 'org-latex-preview
    "b" 'buffer-menu))
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)
(global-set-key (kbd "C-=") 'text-scale-increase)
(global-set-key (kbd "C-+") 'text-scale-increase)
(global-set-key (kbd "C--") 'text-scale-decrease)
;; Using RETURN to follow links in Org/Evil
(with-eval-after-load 'evil-maps
  (define-key evil-motion-state-map (kbd "SPC") nil)
  (define-key evil-motion-state-map (kbd "RET") nil)
  (define-key evil-motion-state-map (kbd "TAB") nil))
(setq org-return-follows-link  t)

(if (eq system-type 'android)
  (progn
    (use-package exec-path-from-shell
                 :custom
                 (shell-file-name "/data/data/com.termux/files/usr/bin/bash")
                 (exec-path-from-shell-variables '("PATH" "MANPATH" "PKG_CONFIG_PATH"))
                 :init
                 (if (string-equal system-type "android")
                   (exec-path-from-shell-initialize)))))

(message "init.el loaded successfully")

;; init.el ends here

(setq custom-file (concat user-emacs-directory "custom.el"))
(load custom-file 'noerror)

(setq my-emacs-data-dir
      (expand-file-name "emacs/"
			(or (getenv "XDG_DATA_HOME")
			    (expand-file-name ".local/share" (getenv "HOME")))))
(setq my-auto-save-dir (concat my-emacs-data-dir "auto-save/")
      my-backup-dir    (concat my-emacs-data-dir "backup/")
      my-lock-dir      (concat my-emacs-data-dir "lock/")
      my-temp-dir      (concat my-emacs-data-dir "tmp/"))
(dolist (dir (list my-auto-save-dir my-backup-dir my-lock-dir my-temp-dir))
  (unless (file-directory-p dir)
    (make-directory dir t)))
(setq lock-file-name-transforms `(("\\`/.*/\\([^/]+\\)\\'" ,(concat my-lock-dir "\\1") t)))
(setq auto-save-file-name-transforms `((".*" ,my-auto-save-dir t)))
(setq backup-directory-alist `((".*" . ,my-backup-dir)))
(setq temporary-file-directory my-temp-dir)

(setq warning-minimum-level :error)
(setq initial-scratch-message nil)
(setq inhibit-startup-message t)
(setq use-short-answers t)
(setq scroll-conservatively 10)
(setq scroll-step 1)
(scroll-bar-mode 0)
(menu-bar-mode 0)
(tool-bar-mode 0)
(global-display-line-numbers-mode)
(global-hl-line-mode)
(electric-pair-mode)
(column-number-mode)
(indent-tabs-mode)
(cua-mode)

(load-theme 'modus-vivendi t)

(add-to-list 'default-frame-alist '(font . "JetBrains Mono-11"))

(with-eval-after-load 'org
  (add-to-list 'org-preview-latex-process-alist
               '(tectonic
                 :programs ("tectonic" "convert")
                 :description "pdf > png"
                 :message "You need tectonic and imagemagick."
                 :image-input-type "pdf"
                 :image-output-type "png"
                 :image-size-adjust (1.0 . 1.0)
                 :latex-compiler
                 ("tectonic -Z shell-escape-cwd=%o --outfmt pdf --outdir %o %f")
                 :image-converter
                 ("convert -density %D -trim -fuzz 2%% -antialias %f -quality 300 %O")))
  (setq org-preview-latex-default-process 'tectonic)
  (setq org-format-latex-options
        (plist-put org-format-latex-options :scale 1.4)))

(org-babel-do-load-languages
 'org-babel-load-languages
 '((python . t)))

(unless (package-installed-p 'evil)
  (package-refresh-contents)
  (package-install 'evil))
(evil-mode)

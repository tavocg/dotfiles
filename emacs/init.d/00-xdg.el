;; Store all Emacs data under $XDG_DATA_HOME/emacs/ (defaults to ~/.local/share/emacs/)
;; This keeps the home directory clean and follows the XDG Base Directory spec.
(setq my-emacs-data-dir
      (expand-file-name "emacs/"
                        (or (getenv "XDG_DATA_HOME")
                            (expand-file-name ".local/share" "~"))))

;; Subdirectories for each type of file Emacs scatters around by default
(setq my-auto-save-dir (concat my-emacs-data-dir "auto-save/")
      my-backup-dir    (concat my-emacs-data-dir "backup/")
      my-lock-dir      (concat my-emacs-data-dir "lock/")
      my-temp-dir      (concat my-emacs-data-dir "tmp/"))

;; Create any missing directories on startup
(dolist (dir (list my-auto-save-dir my-backup-dir my-lock-dir my-temp-dir))
  (unless (file-directory-p dir)
    (make-directory dir t)))

;; Redirect all the files Emacs normally drops in visited file directories
(setq lock-file-name-transforms `(("\\`/.*/\\([^/]+\\)\\'" ,(concat my-lock-dir "\\1") t))
      auto-save-file-name-transforms `((".*" ,my-auto-save-dir t))
      backup-directory-alist `((".*" . ,my-backup-dir))
      temporary-file-directory my-temp-dir)

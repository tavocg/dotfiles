;;; 00-xdg.el --- Keep Emacs state under XDG data directories -*- lexical-binding: t; -*-

;;; Commentary:
;; Store generated state outside the config tree and away from project
;; directories.

;;; Code:

(defconst my-emacs-data-dir
  (expand-file-name
   "emacs/"
   (or (getenv "XDG_DATA_HOME")
       (expand-file-name ".local/share" "~")))
  "Base directory for Emacs data managed under XDG.")

(defconst my-auto-save-dir
  (expand-file-name "auto-save/" my-emacs-data-dir)
  "Directory used for auto-save files.")

(defconst my-backup-dir
  (expand-file-name "backup/" my-emacs-data-dir)
  "Directory used for backup files.")

(defconst my-lock-dir
  (expand-file-name "lock/" my-emacs-data-dir)
  "Directory used for lock files.")

(defconst my-temp-dir
  (expand-file-name "tmp/" my-emacs-data-dir)
  "Directory used for temporary files.")

;; Create the directory tree up front so later file operations stay quiet.
(dolist (dir (list my-auto-save-dir my-backup-dir my-lock-dir my-temp-dir))
  (make-directory dir t))

;; Keep visited directories clean by redirecting Emacs bookkeeping files.
(setq lock-file-name-transforms
      `(("\\`/.*/\\([^/]+\\)\\'" ,(concat my-lock-dir "\\1") t))
      auto-save-file-name-transforms
      `((".*" ,my-auto-save-dir t))
      backup-directory-alist
      `((".*" . ,my-backup-dir))
      temporary-file-directory my-temp-dir)

;;; 00-xdg.el ends here

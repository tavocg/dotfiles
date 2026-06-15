;;; 11-dired.el --- Dired defaults and local bindings -*- lexical-binding: t; -*-

;;; Commentary:
;; Keep Dired concise by default and route deletions through the trash.

;;; Code:

(use-package dired
  :ensure nil
  :custom
  (delete-by-moving-to-trash t)
  :hook
  (dired-mode . dired-hide-details-mode))

(use-package dired-x
  :ensure nil
  :after dired
  :custom
  (dired-omit-files "\\`[.]\\|\\`[.]?#")
  (dired-omit-verbose nil)
  :hook
  (dired-mode . dired-omit-mode))

(use-package general
  :after dired-x
  :functions general-def
  :config
  (general-def
    :states '(normal motion emacs)
    :keymaps 'dired-mode-map
    :prefix "SPC"

    "d" '(dired-hide-details-mode :wk "Toggle details")
    "." '(dired-omit-mode :wk "Toggle hidden")))

;;; 11-dired.el ends here

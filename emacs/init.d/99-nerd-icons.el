;;; 99-nerd-icons.el --- Add icons to Dired buffers -*- lexical-binding: t; -*-

;;; Commentary:
;; Enable Nerd Icons only where they add signal, starting with Dired.

;;; Code:

(use-package nerd-icons-dired
  :functions (nerd-icons-dired-mode)
  :hook
  (dired-mode . nerd-icons-dired-mode))

;;; 99-nerd-icons.el ends here

;;; 50-treesit.el --- Prefer tree-sitter aware major modes -*- lexical-binding: t; -*-

;;; Commentary:
;; Let treesit-auto switch supported languages over to their tree-sitter modes.

;;; Code:

(use-package treesit-auto
  :functions (global-treesit-auto-mode treesit-auto-add-to-auto-mode-alist)
  :custom
  (treesit-auto-install 'prompt)
  :config
  (treesit-auto-add-to-auto-mode-alist 'all)
  (global-treesit-auto-mode 1))

;;; 50-treesit.el ends here

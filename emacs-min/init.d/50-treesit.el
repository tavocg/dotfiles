;;; 50-treesit.el --- Prefer tree-sitter aware major modes -*- lexical-binding: t; -*-

;;; Commentary:
;; Let treesit-auto switch supported languages over to their tree-sitter modes.

;;; Code:

(defvar treesit-language-source-alist nil)

(use-package treesit-auto
  :functions (global-treesit-auto-mode treesit-auto-add-to-auto-mode-alist)
  :custom
  (treesit-auto-install 'prompt)
  :init
  ;; Emacs 30.1 on this system rejects the current upstream C grammar with ABI
  ;; version 15, so pin C to a compatible release.
  (setf (alist-get 'c treesit-language-source-alist)
        '("https://github.com/tree-sitter/tree-sitter-c" "v0.23.6"))
  :config
  (treesit-auto-add-to-auto-mode-alist 'all)
  (global-treesit-auto-mode))

;;; 50-treesit.el ends here

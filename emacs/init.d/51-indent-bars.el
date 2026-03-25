;;; 51-indent-bars.el --- Draw indentation guides in code buffers -*- lexical-binding: t; -*-

;;; Commentary:
;; Enable indent-bars in programming modes and let it reuse tree-sitter data
;; when available.

;;; Code:

(use-package indent-bars
  :functions (indent-bars-mode)
  :custom
  (indent-bars-treesit-support t)
  :hook
  (prog-mode . indent-bars-mode))

;;; 51-indent-bars.el ends here

;;; 51-indent-bars.el --- Draw indentation guides in code buffers -*- lexical-binding: t; -*-

;;; Commentary:
;; Enable indent-bars in programming modes and let it reuse tree-sitter data
;; when available.

;;; Code:

(use-package indent-bars
  :hook
  (prog-mode . indent-bars-mode)
  :functions
  (indent-bars-mode)
  :custom
  (indent-bars-prefer-character
   (memq initial-window-system '(pgtk ns)))
  (indent-bars-treesit-support t))

;;; 51-indent-bars.el ends here

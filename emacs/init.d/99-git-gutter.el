;;; 99-git-gutter.el --- Show Git changes in the fringe -*- lexical-binding: t; -*-

;;; Commentary:
;; Enable git-gutter in programming buffers so line additions, edits, and
;; deletions stay visible while editing.

;;; Code:

(use-package git-gutter
  :functions (git-gutter-mode)
  :hook
  (prog-mode . git-gutter-mode))

;;; 99-git-gutter.el ends here

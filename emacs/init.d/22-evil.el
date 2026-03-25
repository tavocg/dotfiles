;;; 22-evil.el --- Enable Vim-style editing -*- lexical-binding: t; -*-

;;; Commentary:
;; Turn on Evil and align its undo backend with undo-fu.

;;; Code:

(use-package evil
  :demand t
  :functions (evil-mode evil-set-undo-system)
  :config
  ;; Keep undo and redo behavior consistent with the rest of the config.
  (evil-set-undo-system 'undo-fu)
  (global-set-key (kbd "<escape>") #'keyboard-escape-quit)
  (evil-mode 1))

;;; 22-evil.el ends here

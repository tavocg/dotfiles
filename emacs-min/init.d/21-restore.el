;;; 21-restore.el --- Restore minibuffer and undo history -*- lexical-binding: t; -*-

;;; Commentary:
;; Persist history across sessions and route undo commands through undo-fu.

;;; Code:

(use-package savehist
  :ensure nil
  :init
  (savehist-mode 1))

(use-package undo-fu
  :functions (undo-fu-only-redo undo-fu-only-undo)
  :config
  (global-set-key (kbd "C-z") #'undo-fu-only-undo)
  (global-set-key (kbd "C-S-z") #'undo-fu-only-redo))

(use-package undo-fu-session
  :functions (undo-fu-session-global-mode)
  :config
  (undo-fu-session-global-mode 1))

;;; 21-restore.el ends here

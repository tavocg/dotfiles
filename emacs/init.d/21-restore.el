(use-package savehist
  :ensure nil
  :init
  (savehist-mode))

(use-package undo-fu
  :config
  (global-set-key (kbd "C-z")   'undo-fu-only-undo)
  (global-set-key (kbd "C-S-z") 'undo-fu-only-redo))

(use-package undo-fu-session
  :config
  (undo-fu-session-global-mode))

(use-package evil
  :demand t
  :config
  (evil-set-undo-system 'undo-fu)
  (global-set-key (kbd "<escape>") #'keyboard-escape-quit)
  (evil-mode))

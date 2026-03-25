(use-package flycheck
  :demand t
  :ensure t
  :config
  (add-hook 'after-init-hook #'global-flycheck-mode))

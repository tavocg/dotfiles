(use-package corfu
  :custom
  (corfu-cycle t)
  (corfu-auto t)
  (corfu-auto-prefix 2)
  (corfu-auto-delay 0.15)
  (corfu-quit-at-boundary 'separator)
  (corfu-preview-current 'insert)
  (corfu-preselect-first nil)

  :init
  (global-corfu-mode)
  (corfu-history-mode)
  (corfu-popupinfo-mode)

  :config
  (setq corfu-popupinfo-delay '(0.15 . 0.0)))

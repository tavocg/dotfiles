;;; 99-corfu.el --- Configure completion popups -*- lexical-binding: t; -*-

;;; Commentary:
;; Enable Corfu globally with automatic completion and lightweight popup help.

;;; Code:

(use-package corfu
  :functions (corfu-history-mode corfu-popupinfo-mode global-corfu-mode)
  :defines (corfu-popupinfo-delay)

  :custom
  (corfu-cycle t)
  (corfu-auto t)
  (corfu-auto-prefix 2)
  (corfu-auto-delay 0.15)
  (corfu-quit-at-boundary 'separator)
  (corfu-preview-current 'insert)
  (corfu-preselect-first nil)
  (corfu-popupinfo-delay '(0.15 . 0.0))

  :init
  (global-corfu-mode)
  (corfu-history-mode)
  (corfu-popupinfo-mode))

;;; 99-corfu.el ends here

;;; 99-flycheck.el --- Enable on-the-fly diagnostics -*- lexical-binding: t; -*-

;;; Commentary:
;; Start Flycheck after initialization so diagnostics are available in regular
;; editing sessions.

;;; Code:

(use-package flycheck
  :demand t
  :ensure t
  :functions (global-flycheck-mode)
  :hook
  (after-init . global-flycheck-mode))

;;; 99-flycheck.el ends here

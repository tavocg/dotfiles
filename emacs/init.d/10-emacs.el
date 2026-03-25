;;; 10-emacs.el --- Core built-in Emacs behavior -*- lexical-binding: t; -*-

;;; Commentary:
;; Set sane defaults for editing, prompts, and the default frame appearance.

;;; Code:

(use-package emacs
  :ensure nil
  :custom
  (warning-minimum-level :error)
  (initial-scratch-message nil)
  (inhibit-startup-message t)
  (use-short-answers t)
  (scroll-conservatively 10)
  (scroll-step 1)
  (indent-tabs-mode nil)
  (tab-always-indent 'complete)
  (text-mode-ispell-word-completion nil)
  (read-extended-command-predicate
   #'command-completion-default-include-p)
  :init
  (set-face-attribute
   'default nil
   :family "JetBrains Mono"
   :height 110)
  (set-face-attribute
   'fixed-pitch nil
   :family "JetBrains Mono"
   :height 110)
  (set-face-attribute
   'variable-pitch nil
   :family "Liberation Serif"
   :height 160)
  (scroll-bar-mode 0)
  (menu-bar-mode 0)
  (tool-bar-mode 0)
  (global-display-line-numbers-mode 1)
  (global-hl-line-mode 1)
  (electric-pair-mode 1)
  (column-number-mode 1)
  (cua-mode 1))

;;; 10-emacs.el ends here

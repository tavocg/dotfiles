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
  (read-extended-command-predicate #'command-completion-default-include-p)

  :init
  (set-face-attribute
   'default nil
   :family "JetBrains Mono"
   :height 110)
  (scroll-bar-mode 0)
  (menu-bar-mode 0)
  (tool-bar-mode 0)
  (global-display-line-numbers-mode)
  (global-hl-line-mode)
  (electric-pair-mode)
  (column-number-mode)
  (cua-mode)

  )

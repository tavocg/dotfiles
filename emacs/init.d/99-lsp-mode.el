;;; 99-lsp-mode.el --- Common LSP client configuration -*- lexical-binding: t; -*-

;;; Commentary:
;; Keep shared lsp-mode defaults in one place so language-specific files can
;; opt into LSP without repeating the same settings.

;;; Code:

(use-package lsp-mode
  :commands (lsp lsp-deferred lsp-enable-which-key-integration)
  :custom
  ;; lsp-mode can publish diagnostics through Flycheck, which already exists in
  ;; this config.
  (lsp-prefer-flymake nil)
  (lsp-keymap-prefix "C-c l")
  (lsp-completion-provider :capf)
  (lsp-headerline-breadcrumb-enable nil)
  (lsp-enable-snippet t)
  (lsp-idle-delay 0.6)
  :init
  ;; Larger process reads help language servers send bigger JSON payloads
  ;; without fragmenting as often.
  (setq read-process-output-max (* 1024 1024))
  :config
  (lsp-enable-which-key-integration t))

;;; 99-lsp-mode.el ends here

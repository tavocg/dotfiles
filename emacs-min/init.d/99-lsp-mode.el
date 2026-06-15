;;; 99-lsp-mode.el --- Common LSP client configuration -*- lexical-binding: t; -*-

;;; Commentary:
;; Keep shared lsp-mode defaults in one place so language-specific files can
;; opt into LSP without repeating the same settings.

;;; Code:

(declare-function lsp-deferred "lsp-mode")
(declare-function lsp-feature? "lsp-mode")
(declare-function lsp-format-buffer "lsp-mode")

(defun my/lsp-format-buffer-maybe ()
  "Format the current buffer when LSP formatting is available."
  (when (and (bound-and-true-p lsp-mode)
             (lsp-feature? "textDocument/formatting"))
    (lsp-format-buffer)))

(defun my/lsp-prog-mode-setup ()
  "Start LSP lazily in programming buffers and format them on save."
  (lsp-deferred)
  ;; Keep the formatting hook buffer-local so non-programming buffers stay
  ;; untouched.
  (add-hook 'before-save-hook #'my/lsp-format-buffer-maybe nil t))

(use-package lsp-mode
  :commands (lsp lsp-deferred lsp-enable-which-key-integration lsp-feature?)
  :hook
  (prog-mode . my/lsp-prog-mode-setup)
  :custom
  ;; Keep lsp-mode's standard buffer setup, but disable its company-mode path
  ;; since completion already goes through CAPF and Corfu here.
  (lsp-auto-configure t)
  (lsp-completion-enable t)
  (lsp-completion-provider :none)
  (lsp-diagnostics-provider :flycheck)
  (lsp-warn-no-matched-clients nil)
  (lsp-keymap-prefix "C-c l")
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

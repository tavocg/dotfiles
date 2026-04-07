;;; 99-minibuffer.el --- Improve minibuffer completion UI -*- lexical-binding: t; -*-

;;; Commentary:
;; Enable Vertico for candidate selection and Marginalia for richer minibuffer
;; annotations.

;;; Code:

(use-package vertico
  :functions (vertico-mode)
  :init
  (vertico-mode 1))

(use-package emacs
  :ensure nil
  :custom
  (enable-recursive-minibuffers t)
  (minibuffer-prompt-properties
   '(read-only t cursor-intangible t face minibuffer-prompt))
  :init
  (context-menu-mode))

(use-package marginalia
  :functions (marginalia-mode)
  :bind
  (:map minibuffer-local-map
        ("M-A" . marginalia-cycle)
        :map completion-list-mode-map
        ("M-A" . marginalia-cycle))
  :init
  (marginalia-mode))

;;; 99-minibuffer.el ends here

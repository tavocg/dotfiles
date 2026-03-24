(use-package vertico
  :init
  (vertico-mode))

(use-package emacs
  :custom
  (context-menu-mode t)
  (enable-recursive-minibuffers t)
  (minibuffer-prompt-properties
   '(read-only t cursor-intangible t face minibuffer-prompt)))

(use-package marginalia
  :bind (:map minibuffer-local-map
              ("M-A" . marginalia-cycle)
              :map completion-list-mode-map
              ("M-A" . marginalia-cycle))
  :init
  (marginalia-mode))

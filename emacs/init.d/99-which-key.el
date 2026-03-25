;;; 99-which-key.el --- Show available key prefixes -*- lexical-binding: t; -*-

;;; Commentary:
;; Display pending key sequences so the leader map stays discoverable.

;;; Code:

(use-package which-key
  :ensure t
  :functions (which-key-mode)
  :config
  (which-key-mode))

;;; 99-which-key.el ends here

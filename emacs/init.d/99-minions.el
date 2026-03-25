;;; 99-minions.el --- Compact the mode line minor mode list -*- lexical-binding: t; -*-

;;; Commentary:
;; Use Minions to keep minor mode indicators from overwhelming the mode line.

;;; Code:

(use-package minions
  :demand t
  :functions (minions-mode)
  :config
  (minions-mode))

;;; 99-minions.el ends here

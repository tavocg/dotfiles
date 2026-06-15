;;; 99-theme.el --- Load the main theme -*- lexical-binding: t; -*-

;;; Commentary:
;; Configure doom-themes and apply the preferred theme at startup.

;;; Code:

(use-package doom-themes
  :functions (doom-themes-org-config)
  :custom
  (doom-themes-enable-bold t)
  (doom-themes-enable-italic t)
  (doom-one-padded-modeline 4)
  :config
  (load-theme 'doom-one t)
  (doom-themes-org-config))

;;; 99-theme.el ends here

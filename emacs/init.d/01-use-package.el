;;; 01-use-package.el --- Bootstrap package management -*- lexical-binding: t; -*-

;;; Commentary:
;; Initialize package.el early and ensure use-package is available for the
;; rest of the configuration.

;;; Code:

(require 'package)

(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(package-initialize)

;; The rest of the init files assume use-package is ready to expand.
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)

;;; 01-use-package.el ends here

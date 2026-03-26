;;; 70-org.el --- Configure Org workflows -*- lexical-binding: t; -*-

;;; Commentary:
;; Configure editing, capture, and Babel defaults for Org buffers.

;;; Code:

(defun my/org-mode-setup ()
  "Apply per-buffer defaults for Org buffers."
  (display-line-numbers-mode 0))

(use-package org
	     :ensure nil
	     :after evil
	     :functions (evil-define-key org-open-at-point)

	     :defines
	     (org-babel-default-header-args:python
	       org-babel-python-command
	       org-capture-templates
	       org-confirm-babel-evaluate
	       org-format-latex-options
	       org-latex-pdf-process
	       org-preview-latex-image-directory)

	     :bind
	     (("C-c c" . org-capture))

	     :hook
	     (org-mode . my/org-mode-setup)

	     :custom
	     (org-latex-pdf-process '("tectonic %f"))
	     (org-preview-latex-image-directory
	       (expand-file-name "ltximg/" user-emacs-directory))
	     (org-capture-templates
	       '(("d" "Document" entry
		  (file+headline "~/.config/templates/document.org" "Documents")
		  "* %? :DOCUMENT:\n  %i\n  %a")))
	     (org-confirm-babel-evaluate nil)
	     ;; Use the dedicated command variable for Python instead of a non-standard
	     ;; header argument entry.
	     (org-babel-python-command "~/.local/share/venv/global/bin/python")
	     (org-babel-default-header-args:python
	       '((:session . "python")))

	     :config
	     (evil-define-key 'normal org-mode-map (kbd "TAB") #'org-cycle)
	     (evil-define-key 'normal org-mode-map (kbd "RET") #'org-open-at-point)
	     (setq org-format-latex-options
		   (plist-put org-format-latex-options :scale 1.4))
	     (org-babel-do-load-languages
	       'org-babel-load-languages
	       '((shell . t)
		 (python . t))))

(use-package org-appear
	     :defines
	     (org-appear-autoemphasis
	       org-appear-autolinks
	       org-appear-autosubmarkers
	       org-hide-emphasis-markers)
	     :custom
	     (org-hide-emphasis-markers t)
	     (org-appear-autoemphasis t)
	     (org-appear-autolinks t)
	     (org-appear-autosubmarkers t))

(use-package org-fragtog
	     :functions (org-fragtog-mode)
	     :hook
	     (org-mode . org-fragtog-mode))

;;(use-package olivetti
;;  :functions (olivetti-mode)
;;  :hook
;;  (org-mode . olivetti-mode)
;;  :config
;;  (setq olivetti-body-width 80))

(setq-default visual-fill-column-center-text t)

(use-package visual-fill-column
	     :defines
	     (visual-fill-column-width
	       visual-fill-column-center-text)
	     :hook
	     (org-mode . visual-fill-column-mode)
	     :config
	     (setq visual-fill-column-width 81
		   visual-fill-column-center-text t))

;;; 70-org.el ends here

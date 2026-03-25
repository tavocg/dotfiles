(use-package org
  :ensure nil
  :after evil
  :config

  (evil-define-key 'normal org-mode-map (kbd "TAB") #'org-cycle)
  (evil-define-key 'normal org-mode-map (kbd "RET") #'org-open-at-point)

  (setq org-latex-pdf-process '("tectonic %f"))
  (setq org-preview-latex-image-directory (concat user-emacs-directory "ltximg/"))
  (setq org-format-latex-options
        (plist-put org-format-latex-options :scale 1.4))

  (setq org-capture-templates
        '(("d" "Document" entry (file+headline "~/.config/templates/document.org" "Documents")
           "* %? :DOCUMENT:\n  %i\n  %a")))

  (global-set-key (kbd "C-c c") 'org-capture)

  (add-hook 'org-mode-hook (lambda () (display-line-numbers-mode 0)))

  (setq org-confirm-babel-evaluate nil)

  (setq org-babel-default-header-args:python
        '((:python . "~/.local/share/venv/global/bin/python")
          (:session . "python")))

  (org-babel-do-load-languages
   'org-babel-load-languages
   '((shell . t)
     (python . t))))

(use-package org-appear
  :config
  (setq org-hide-emphasis-markers t
        org-appear-autoemphasis t
        org-appear-autolinks t
        org-appear-autosubmarkers t))

(use-package org-fragtog
  :hook (org-mode-hook . org-fragtog-mode))

(use-package olivetti
  :hook (org-mode-hook . olivetti-mode))

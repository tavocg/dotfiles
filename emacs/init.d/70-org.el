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
  (global-set-key (kbd "C-c c") 'org-capture))

(use-package org-roam
  :ensure t
  :custom
  (org-roam-directory "~/Documents/roam")
  (org-roam-capture-templates
   '(("d" "Document" plain
      (file "~/.config/templates/document.org")
      :target (file "%<%Y%m%d%T>-${slug}.org")
      :unnarrowed t)))
  :bind
  (("C-c n l" . org-roam-buffer-toggle)
   ("C-c n f" . org-roam-node-find)
   ("C-c n g" . org-roam-graph)
   ("C-c n i" . org-roam-node-insert)
   ("C-c n c" . org-roam-capture)
   ("C-c n j" . org-roam-dailies-capture-today))
  :config
  (org-roam-db-autosync-mode)
  (require 'org-roam-protocol))

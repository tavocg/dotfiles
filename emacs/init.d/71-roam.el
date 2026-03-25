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

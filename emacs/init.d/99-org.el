(use-package org
  :ensure nil
  :defer t
  :config
  (add-to-list 'org-preview-latex-process-alist
               '(tectonic
                 :programs ("tectonic" "convert")
                 :description "pdf > png"
                 :message "You need tectonic and imagemagick."
                 :image-input-type "pdf"
                 :image-output-type "png"
                 :latex-compiler
                 ("tectonic -Z shell-escape-cwd=%o --outfmt pdf --outdir %o %f")
                 :image-converter
                 ("convert -density %D -trim -fuzz 2%% %f %O")))
  (setq org-preview-latex-default-process 'tectonic)
  (setq org-format-latex-options
        (plist-put org-format-latex-options :scale 1.4)))

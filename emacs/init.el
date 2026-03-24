(setq custom-file (concat user-emacs-directory "custom.el"))
(load custom-file 'noerror)

(dolist (file (sort (directory-files (expand-file-name "init.d" user-emacs-directory) t "\\.el$") #'string<))
  (load file))

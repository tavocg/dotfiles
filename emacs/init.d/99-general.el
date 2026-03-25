(use-package general
  :after evil
  :demand t
  :config
  (general-evil-setup)

  (general-create-definer my/leader-keys
    :states '(normal insert visual emacs)
    :keymaps 'override
    :prefix "SPC"
    :global-prefix "M-SPC")

  (my/leader-keys
    "b" '(:ignore t :wk "buffer")
    "bb" '(switch-to-buffer :wk "Switch buffer")
    "bd" '((lambda () (interactive) (kill-buffer (current-buffer))) :wk "Kill this buffer")
    "bn" '(next-buffer :wk "Next buffer")
    "bp" '(previous-buffer :wk "Previous buffer")
    "ff" '(find-file :wk "Find file")
    "fs" '(save-buffer :wk "Save buffer"))

  ;; Help
  (my/leader-keys
    "h" '(:ignore t :wk "Help")
    "hf" '(describe-function :wk "Describe function")
    "hv" '(describe-variable :wk "Describe variable")
    )

  ;; Org-Roam
  (my/leader-keys
    "n" '(:ignore t :wk "notes")
    "nl" '(org-roam-buffer-toggle :wk "Roam buffer toggle")
    "nf" '(org-roam-node-find :wk "Find node")
    "ng" '(org-roam-graph :wk "Graph")
    "ni" '(org-roam-node-insert :wk "Insert node")
    "nc" '(org-roam-capture :wk "Capture")
    "nj" '(org-roam-dailies-capture-today :wk "Daily journal")
    )

  ;; Config
  (my/leader-keys
    "c" '(:ignore t :wk "config")
    "cr" '((lambda () (interactive) (load-file user-init-file)) :wk "Reload config"))
  )

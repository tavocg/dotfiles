;;; 99-general.el --- Keybinding helpers and leader keys -*- lexical-binding: t; -*-

;;; Commentary:
;; Define the main leader-key layout used across the rest of the config.

;;; Code:

(defun my/kill-current-buffer ()
  "Kill the current buffer."
  (interactive)
  (kill-buffer (current-buffer)))

(defun my/reload-init-file ()
  "Reload the active init file."
  (interactive)
  (load-file user-init-file))

(use-package general
  :after evil
  :demand t
  :functions (general-def general-evil-setup)
  :config
  (general-evil-setup)

  ;; Keep the leader map definition in one place so later additions stay easy
  ;; to scan.
  (general-def
    :states '(normal insert visual emacs)
    :keymaps 'override
    :prefix "SPC"
    :global-prefix "M-SPC"

    "b" '(:ignore t :wk "buffer")
    "bb" '(switch-to-buffer :wk "Switch buffer")
    "bd" '(my/kill-current-buffer :wk "Kill this buffer")
    "bn" '(next-buffer :wk "Next buffer")
    "bp" '(previous-buffer :wk "Previous buffer")

    "f" '(:ignore t :wk "file")
    "ff" '(find-file :wk "Find file")
    "fs" '(save-buffer :wk "Save buffer")

    "h" '(:ignore t :wk "help")
    "hf" '(describe-function :wk "Describe function")
    "hv" '(describe-variable :wk "Describe variable")

    "n" '(:ignore t :wk "notes")
    "nl" '(org-roam-buffer-toggle :wk "Roam buffer toggle")
    "nf" '(org-roam-node-find :wk "Find node")
    "ng" '(org-roam-graph :wk "Graph")
    "ni" '(org-roam-node-insert :wk "Insert node")
    "nc" '(org-roam-capture :wk "Capture")
    "nj" '(org-roam-dailies-capture-today :wk "Daily journal")

    "w" '(:ignore t :wk "window")
    "wd" '(delete-window :wk "Delete window")

    "-" '(split-window-below :wk "Split below")
    "|" '(split-window-right :wk "Split right")

    "c" '(:ignore t :wk "config")
    "cr" '(my/reload-init-file :wk "Reload config")))

;;; 99-general.el ends here

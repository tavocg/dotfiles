(require 'package)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))
(package-initialize)
(package-refresh-contents)

(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(unless (package-installed-p 'undo-tree)
  (package-install 'undo-tree))
(require 'undo-tree)
(global-undo-tree-mode)

(unless (package-installed-p 'goto-chg)
  (package-install 'goto-chg))

(unless (package-installed-p 'evil-collection)
  (package-install 'evil-collection))
(setq evil-want-keybinding nil)

(unless (package-installed-p 'evil)
  (package-install 'evil))
(require 'evil)
(evil-mode 1)
(evil-set-undo-system 'undo-redo)

(use-package evil
    :init
    (setq evil-want-integration t)
    (setq evil-want-keybinding t)
    (setq evil-vsplit-window-right t)
    (setq evil-split-window-below t)
    (evil-mode))
(use-package evil-collection
    :after evil
    :config
    (setq evil-collection-mode-list '(dashboard dired ibuffer))
    (evil-collection-init))

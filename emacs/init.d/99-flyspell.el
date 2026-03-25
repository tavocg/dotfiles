;;; 99-flyspell.el --- Flyspell configuration ;;; -*- lexical-binding: t -*-

;;; Commentary:
;; Configuration for flyspell minor mode.

;;; Code:

(use-package flyspell
  :ensure nil
  :hook
  (text-mode . flyspell-mode)
  (prog-mode . flyspell-prog-mode)
  :custom
  (ispell-program-name "aspell")
  (ispell-extra-args '("--sug-mode=ultra" "--lang=en_US")))

(use-package flyspell-correct
  :ensure t
  :after flyspell
  :bind (:map flyspell-mode-map
              ("C-;" . flyspell-correct-wrapper))
  :custom
  (flyspell-correct-interface #'flyspell-correct-completing-read))

;;; 99-flyspell.el ends here

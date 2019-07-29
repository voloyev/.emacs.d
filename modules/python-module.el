;;; package --- Summary:
;;; Code:
;;; Commentary:
;; python module
(add-to-list 'load-path "~/.emacs.d/modules")

(use-package blacken
    :config
  (setq blacken-skip-string-normalization t)
  (setq blacken-line-length 80))

(use-package python-mode
    :ensure t
    :init
    :config
    (add-hook 'python-mode-hook 'blacken-mode))

(use-package anaconda-mode
    :ensure t
    :init
    (add-hook 'python-mode-hook 'anaconda-mode)
    (add-hook 'python-mode-hook 'anaconda-eldoc-mode))

(use-package pipenv
    :ensure t
    :hook (python-mode . pipenv-mode)
    :init
    (setq
     pipenv-projectile-after-switch-function
     #'pipenv-projectile-after-switch-extended))


(provide 'python-module)
;;; python-module ends here

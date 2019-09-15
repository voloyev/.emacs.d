;;; package --- Summary:
;;; Code:
;;; Commentary:
;; python module
(add-to-list 'load-path "~/.emacs.d/modules")

(use-package blacken
    :config
  (setq blacken-skip-string-normalization t)
  (setq blacken-line-length 100))

(use-package python-mode
    :ensure t)

(use-package py-autopep8
    :ensure t)

(use-package ein
    :ensure t)

(use-package elpy
    :ensure t
    :init
    (elpy-enable)
    (elpy-use-ipython)
    (setq elpy-modules (delq 'elpy-module-flymake elpy-modules))
    (add-hook 'elpy-mode-hook 'flycheck-mode)
    (add-hook 'elpy-mode-hook 'py-autopep8-enable-on-save))

(use-package pipenv
    :ensure t
    :hook (python-mode . pipenv-mode)
    :init
    (setq
     pipenv-projectile-after-switch-function
     #'pipenv-projectile-after-switch-extended))

(use-package pyvenv
    :ensure t)

(provide 'python-module)
;;; python-module ends here

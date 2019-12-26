;;; package --- Summary:
;;; Code:
;;; Commentary:
;; python module

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
    (setq elpy-modules (delq 'elpy-module-flymake elpy-modules))
    :hook (elpy-mode . flycheck-mode)
    :hook (elpy-mode . py-autopep8-enable-on-save))

(use-package pipenv
    :ensure t
    :hook (python-mode . pipenv-mode)
    :init
    (setq
     pipenv-projectile-after-switch-function
     #'pipenv-projectile-after-switch-extended))

(use-package poetry
    :ensure t
    :config (poetry-tracking-mode t))

(use-package pyvenv
    :ensure t)

(add-to-list 'company-backends 'company-jedi)

(provide 'python-module)
;;; python-module ends here

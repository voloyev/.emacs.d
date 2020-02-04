;;; package --- Summary:
;;; Code:
;;; Commentary:
;; python module

(use-package blacken
    :config
  (setq blacken-skip-string-normalization t)
  (setq blacken-line-length 100))

(use-package python-mode
    :straight t)

(use-package py-autopep8
    :straight t)

(use-package ein
    :straight t)

(use-package elpy
    :straight t
    :init
    (elpy-enable)
    (setq elpy-modules (delq 'elpy-module-flymake elpy-modules))
    :hook (elpy-mode . flycheck-mode)
    :hook (elpy-mode . py-autopep8-enable-on-save))

(when (require 'flycheck nil t)
  (setq elpy-modules (delq 'elpy-module-flymake elpy-modules))
  (add-hook 'elpy-mode-hook 'flycheck-mode))

(use-package pipenv
    :straight t
    :hook (python-mode . pipenv-mode)
    :init
    (setq
     pipenv-projectile-after-switch-function
     #'pipenv-projectile-after-switch-extended))

(use-package poetry
    :straight t
    :config (poetry-tracking-mode t))

(use-package pyvenv
    :straight t)

(use-package auto-virtualenv
    :straight t
    :config
    ;; Activate on changing buffers
    (add-hook 'window-configuration-change-hook 'auto-virtualenv-set-virtualenv)
    ;; Activate on focus in
    (add-hook 'focus-in-hook 'auto-virtualenv-set-virtualenv))

(add-to-list 'company-backends 'company-jedi)

(provide 'python-module)
;;; python-module ends here

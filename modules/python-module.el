;;; package --- Summary:
;;; Code:
;;; Commentary:
;; python module

(use-package python-black
    :demand t
    :ensure t
    :after python
    :bind("C-c SPC p b r" . python-black-region))

(use-package python-mode
    :ensure t)

(use-package lsp-python-ms
  :ensure t
  :hook (python-mode . (lambda ()
                          (require 'lsp-python-ms)
                          (lsp-deferred))))

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

(use-package auto-virtualenv
    :ensure t
    :config
    ;; Activate on changing buffers
    (add-hook 'window-configuration-change-hook 'auto-virtualenv-set-virtualenv)
    ;; Activate on focus in
    (add-hook 'focus-in-hook 'auto-virtualenv-set-virtualenv))

(provide 'python-module)
;;; python-module ends here

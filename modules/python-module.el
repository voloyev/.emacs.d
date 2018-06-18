;;; package --- Summary:
;;; Code:
;;; Commentary:
;; python module
;;"TODO:
;;- Add ability to automatic change python version a-la pyenv
;;- Add integration with elpy
;;"
(use-package elpy
    :ensure t)

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
;;(package-initialize)
(elpy-enable)

(setq python-shell-interpreter "python3"
      python-shell-interpreter-args "-i")

(provide 'python-module)
;;; python-module ends here

;;; package --- Summary:
;;; Code:
;;; Commentary:
;; python module

(use-package python-mode
    :ensure t
    :init
    (elpy-enable)
    (setq python-shell-interpreter "python3"
          python-shell-interpreter-args "-i"))

(use-package elpy
    :ensure t)

;; (defvar elpy-mode-map nil "Keymap for `elpy-mode-map'.")
;; (setq elpy-mode-map (make-sparse-keymap))
;; (define-key elpy-mode-map (kbd "M-.") 'elpy-goto-definition)

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

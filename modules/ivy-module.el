;;; package --- Summary:
;;; Code:
;;; Commentary:
;; ivy module
(use-package ivy
    :straight t
    :config
    (setq ivy-use-virtual-buffers t)
    (setq ivy-count-format "(%d/%d) ")
    :bind(("C-s"       . swiper)
          ("M-y"       . counsel-yank-pop)
          ("C-x b"     . ivy-switch-buffer)
          ("C-c i d f" . counsel-describe-function)
          ("C-c i d v" . counsel-describe-variable)))

(use-package counsel-projectile
    :straight t
    :bind(("C-c C-c SPC s"     . counsel-projectile-ag)
          ("C-c C-c SPC r"     . counsel-projectile-rg)
          ("C-c C-c SPC SPC"   . counsel-projectile)))

(counsel-projectile-mode t)

(provide 'ivy-module)
;;; ivy-module.el ends here

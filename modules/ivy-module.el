;;; package --- Summary:
;;; Code:
;;; Commentary:
;; ivy module
(use-package ivy
    :ensure t
    :config
    (setq ivy-use-virtual-buffers t)
    (setq ivy-count-format "(%d/%d) ")
    :bind(("C-s"   . swiper)
          ("M-y"   . counsel-yank-pop)
          ("C-x b" . ivy-switch-buffer)
          ("M-x "  . counsel-M-x)
          ("C-c i d f" . counsel-describe-function)
          ("C-c i d v" . counsel-describe-variable)))

(use-package counsel-projectile
    :ensure t
    :bind(("C-c p SPC s" . counsel-projectile-ag)
          ("C-c p SPC r" . counsel-projectile-rg)
          ("C-c p SPC SPC"   . counsel-projectile)))

(use-package counsel-etags
    :ensure t)

(counsel-projectile-mode t)

(provide 'ivy-module)
;;; ivy-module.el ends here

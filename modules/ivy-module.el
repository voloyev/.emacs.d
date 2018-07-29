;;; package --- Summary:
;;; Code:
;;; Commentary:
;; ivy module
(use-package ivy
    :ensure t
    :init
    (counsel-projectile-mode)
    :config
    (setq ivy-use-virtual-buffers t)
    (setq ivy-count-format "(%d/%d) ")
    :bind(("C-s" . swiper)
          ("M-y" . counsel-yank-pop)))

(use-package counsel-projectile
    :ensure t
    :bind(("C-c p s s" . counsel-projectile-ag)
          ("C-c p p" . counsel-projectile-switch-project)))

(counsel-projectile-mode)

(provide 'ivy-module)
;;; ivy-module.el ends here

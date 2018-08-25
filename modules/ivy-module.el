;;; package --- Summary:
;;; Code:
;;; Commentary:
;; ivy module
(setq projectile-keymap-prefix (kbd "C-c C-p"))

(use-package ivy
    :ensure t
    :config
    (setq ivy-use-virtual-buffers t)
    (setq ivy-count-format "(%d/%d) ")
    :bind(("C-s" . swiper)
          ("M-y" . counsel-yank-pop)))

(use-package counsel-projectile
    :ensure t
    :bind(("C-c p s s" . counsel-projectile-ag)
	        ("C-c p s r" . counsel-projectile-rg)
          ("C-c p p"   . counsel-projectile-switch-project)
          ("C-c SPC"   . counsel-projectile)))

(counsel-projectile-mode t)

(provide 'ivy-module)
;;; ivy-module.el ends here

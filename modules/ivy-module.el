;;; package --- Summary:
;;; Code:
;;; Commentary:
;; ivy module
(use-package ivy-mode
    :init
    (counsel-projectile-on)
    :config
    (setq ivy-use-virtual-buffers t)
    (setq ivy-count-format "(%d/%d) ")
    :bind(("C-s" . swiper)
          ("C-c m g" . counsel-git-grep)
          ("C-c m a" . counsel-projectile-ag)))

(provide 'ivy-module)
;;; ivy-module.el ends here

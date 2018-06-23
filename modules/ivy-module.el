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
    :bind(("C-s" . swiper)))

(use-package counsel-projectile
    :ensure t)

(use-package ivy-posframe
    :ensure t)

(setq ivy-display-function #'ivy-posframe-display-at-frame-bottom-window-center)
(push '(t . ivy-posframe-display) ivy-display-functions-alist)

(ivy-posframe-enable)

(provide 'ivy-module)
;;; ivy-module.el ends here

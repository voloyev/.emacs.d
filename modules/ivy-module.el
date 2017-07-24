;;; package --- Summary:
;;; Code:
;;; Commentary:
;; ivy module
(ivy-mode 1)
(setq ivy-use-virtual-buffers t)
(setq ivy-count-format "(%d/%d) ")
(global-set-key (kbd "C-s") 'swiper)
;;(global-set-key (kbd "M-x") 'counsel-M-x)
;;(global-set-key (kbd "C-x C-f") 'counsel-find-file)
(global-set-key (kbd "<f1> f") 'counsel-describe-function)
(global-set-key (kbd "<f1> v") 'counsel-describe-variable)
(global-set-key (kbd "<f1> l") 'counsel-find-library)
(global-set-key (kbd "C-<f1> i") 'counsel-info-lookup-symbol)
(global-set-key (kbd "C-<f1> u") 'counsel-unicode-char)
;; (define-key read-expression-map (kbd "C-r") 'counsel-expression-history)
(global-set-key (kbd "C-c g") 'counsel-git)
(global-set-key (kbd "C-c j") 'counsel-git-grep)
(global-set-key (kbd "C-c C-a") 'counsel-projectile-ag)
(global-set-key (kbd "C-x l") 'counsel-locate)
;;(counsel-projectile-on)
;;(global-set-key (kbd "C-c p p") 'counsel-projectile-switch-project)
(provide 'ivy-module)
;;; ivy-module.el ends here

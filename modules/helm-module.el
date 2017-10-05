;;; package --- Summary:
;;; Code:
;;; Commentary:
;; helm
(require 'helm-config)
(require 'helm-swoop)
(require 'helm-git-grep)

;; (global-set-key (kbd "M-i") 'helm-swoop)
;; (global-set-key (kbd "C-c m s p") 'helm-projectile-ag)
;; (global-set-key (kbd "C-c j") 'helm-git-grep)
;; (global-set-key (kbd "C-c m a") 'helm-multi-swoop-projectile)
(global-set-key (kbd "C-c C-p") 'helm-semantic-or-imenu)
;(global-set-key (kbd "M-I") 'helm-swoop-back-to-last-point)
;(global-set-key (kbd "C-c M-i") 'helm-multi-swoop)
;(global-set-key (kbd "C-x M-i") 'helm-multi-swoop-all)

(provide 'helm-module)
;;; helm-module ends here

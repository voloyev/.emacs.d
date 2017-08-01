;;; package --- Summary:
;;; Code:
;;; Commentary:
;; helm
(require 'helm-config)
;; Locate the helm-swoop folder to your path
 (require 'helm-swoop)
;; Change the keybinds to whatever you like :)
;;(require 'helm-projectile)
;;(helm-projectile-on)
;;(global-set-key (kbd "C-x C-f") 'helm-find-files)
;;(global-set-key (kbd "M-x") 'helm-M-x)

;(global-set-key (kbd "M-i") 'helm-swoop)
;(global-set-key (kbd "M-I") 'helm-swoop-back-to-last-point)
;(global-set-key (kbd "C-c M-i") 'helm-multi-swoop)
;(global-set-key (kbd "C-x M-i") 'helm-multi-swoop-all)
;(global-set-key (kbd "C-c & C-a") 'helm-projectile-ag)

;;(require 'helm-git-grep)
;(global-set-key (kbd "C-c g") 'helm-git-grep)
(global-set-key (kbd "C-c m s p") 'helm-multi-swoop-projectile)

(provide 'helm-module)
;;; helm-module ends here

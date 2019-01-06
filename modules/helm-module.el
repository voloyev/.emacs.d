;;; package --- Summary:
;;; Code:
;;; Commentary:
;; helm
(use-package helm
             :ensure t)

(use-package helm-config)

(use-package helm-git-grep
             :ensure t)

(global-set-key (kbd "C-c m a") 'helm-projectile-ag)
(global-set-key (kbd "C-c h g") 'helm-git-grep)
(global-set-key (kbd "C-c C-p") 'helm-semantic-or-imenu)
;; (global-set-key (kbd "M-I") 'helm-swoop-back-to-last-point)

(provide 'helm-module)
;;; helm-module ends here

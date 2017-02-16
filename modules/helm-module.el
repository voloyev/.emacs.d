;;; package --- Summary:
;;; Code:
;;; Commentary:
;; helm
(require 'helm)
;; Locate the helm-swoop folder to your path
(require 'helm-swoop)
;; Change the keybinds to whatever you like :)
(helm-projectile-on)
(global-set-key (kbd "M-i") 'helm-swoop)
(global-set-key (kbd "M-I") 'helm-swoop-back-to-last-point)
(global-set-key (kbd "C-c M-i") 'helm-multi-swoop)
(global-set-key (kbd "C-x M-i") 'helm-multi-swoop-all)
(global-set-key (kbd "C-c C-a") 'helm-projectile-ag)
;; When doing isearch, hand the word over to helm-swoop
(define-key isearch-mode-map (kbd "M-i") 'helm-swoop-from-isearch)
;; From helm-swoop to helm-multi-swoop-all
(define-key helm-swoop-map (kbd "M-i") 'helm-multi-swoop-all-from-helm-swoop)
;; Instead of helm-multi-swoop-all, you can also use helm-multi-swoop-current-mode
(define-key helm-swoop-map (kbd "M-m") 'helm-multi-swoop-current-mode-from-helm-swoop)
(require 'helm-git-grep)
(global-set-key (kbd "C-c g") 'helm-git-grep)
;;Invoke `helm-git-grep' from isearch.
(define-key isearch-mode-map (kbd "C-c g") 'helm-git-grep-from-isearch)
;;Invoke `helm-git-grep' from other helm.
(eval-after-load 'helm
                 '(define-key helm-map (kbd "C-c g") 'helm-git-grep-from-helm))
(global-set-key (kbd "C-c C-c p s") 'helm-multi-swoop-projectile)

;;helm on the bottom
(add-to-list 'display-buffer-alist
             `(,(rx bos "*helm" (* not-newline) "*" eos)
                (display-buffer-in-side-window)
                (inhibit-same-window . t)
                (window-height . 0.4)))
(provide 'helm-module)
;;; helm-module ends here

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
          ("M-x" . counsel-M-x)
          ("C-x C-f" . counsel-find-file)
          ("<f1> f" . counsel-describe-function)
          ("<f1> v" . counsel-describe-variable)
          ("<f1> l" . counsel-find-library)
          ("C-<f1> i" . counsel-info-lookup-symbol)
          ("C-<f1> u" . counsel-unicode-char)
          ("C-r" . counsel-expression-history)
          ("C-c m g" . counsel-git-grep)
          ("C-c m a" . counsel-projectile-ag)
          ("C-c p p" . counsel-projectile-switch-project)
          ("<f2>" . ivy-switch-buffer)
          ;;("C-x l" . counsel-locate)
          ;;("C-c g" . counsel-git)
          ))

(use-package counsel-gtags
    :config
    (add-hook 'c-mode-hook 'counsel-gtags-mode)
    (add-hook 'c++-mode-hook 'counsel-gtags-mode)
    (add-hook 'ruby-mode-hook 'counsel-gtags-mode)
    (add-hook 'enh-ruby-mode-hook 'counsel-gtags-mode)
    (add-hook 'python-mode-hook 'counsel-gtags-mode)
    (add-hook 'emacs-lisp-mode-hook 'counsel-gtags-mode)
    (add-hook 'lisp-mode-hook 'counsel-gtags-mode)

    :bind(("C-c C-c t t". counsel-gtags-find-definition)
          ("C-c C-c t r" . counsel-gtags-find-reference)
          ("C-c C-c t s" . counsel-gtags-find-symbol)
          ("C-c C-c t ," . counsel-gtags-go-backward)))

(provide 'ivy-module)
;;; ivy-module.el ends here

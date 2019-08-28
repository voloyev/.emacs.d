;;; package -- Summary
;;; Commentary:
;;; Lisp settings
;;; Code:
;; (load (expand-file-name "~/.roswell/helper.el"))

(use-package sly
    :ensure t
    :init
    (setq inferior-lisp-program "ros -Q run"))

(use-package elisp-slime-nav
    :ensure t
    :init
    (dolist (hook '(emacs-lisp-mode-hook ielm-mode-hook))
      (add-hook hook 'elisp-slime-nav-mode)))

(use-package racket-mode
    :ensure t)

(use-package smartparens
    :ensure t
    :hook(smartparens-strict-mode . lisp-mode)
    :hook(smartparens-strict-mode . emacs-lisp-mode)
    :hook(smartparens-strict-mode . clojure-mode)
    :hook(smartparens-strict-mode . racket-mode)
    :hook(smartparens-strict-mode . slime-mode)
    :hook(smartparens-strict-mode . sly-mode)
    :hook(smartparens-strict-mode . cider-mode))

(use-package lispy
    :ensure t
    :bind(("C-c C-v l" . lispy-mode)))

(use-package geiser
    :init
  (setq geiser-default-implementation 'racket))


(add-hook 'lisp-mode-hook 'sly-editing-mode)

(provide 'lisp-module)
;;; lisp-module ends here

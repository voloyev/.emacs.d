;; elisp settings

(use-package elisp-slime-nav
    :ensure t
    :init
    (dolist (hook '(emacs-lisp-mode-hook ielm-mode-hook))
      (add-hook hook 'elisp-slime-nav-mode)))

(use-package sly
    :ensure t
    :init
    (setq inferior-lisp-program "sbcl")
    (remove-hook 'lisp-mode-hook 'slime-lisp-mode-hook)
    :hook (sly-mode . common-lisp-mode))

(use-package racket-mode
    :ensure t)

(use-package paredit
    :ensure t
    :hook(paredit-mode . lisp-mode)
    :hook(paredit-mode . emacs-lisp-mode)
    :hook(paredit-mode . clojure-mode)
    :hook(paredit-mode . racket-mode)
    :hook(paredit-mode . sly-mode))

(provide 'lisp-module)
;;; lisp-module ends here

;;; package -- Summary
;;; Commentary:
;;; Lisp settings
;;; Code:
;; (load (expand-file-name "~/.roswell/helper.el"))

(use-package slime
    :ensure t
    :init
    (setq inferior-lisp-program "ros -Q run")
    (setq slime-contribs '(slime-fancy slime-company)))

(use-package elisp-slime-nav
    :ensure t
    :init
    (dolist (hook '(emacs-lisp-mode-hook ielm-mode-hook))
      (add-hook hook 'elisp-slime-nav-mode)))

(use-package racket-mode
    :ensure t)

(use-package paredit
    :ensure t
    :hook(paredit-mode . lisp-mode)
    :hook(paredit-mode . emacs-lisp-mode)
    :hook(paredit-mode . clojure-mode)
    :hook(paredit-mode . racket-mode)
    :hook(paredit-mode . slime-mode)
    :hook(paredit-mode . cider-mode))

(use-package lispy
    :ensure t
    :bind(("C-c C-v l" . lispy-mode)))

(add-hook 'lisp-mode-hook 'slime-lisp-mode-hook)

(provide 'lisp-module)
;;; lisp-module ends here

;;; package -- Summary
;;; Commentary:
;;; Lisp settings
;;; Code:
;; (load (expand-file-name "~/.roswell/helper.el"))
(remove-hook 'lisp-mode-hook 'slime-lisp-mode-hook)

(use-package sly
    :ensure t
    :init
    (setq org-babel-lisp-eval-fn 'sly-eval)
    (setq inferior-lisp-program "ros -Q run")
    :hook (lisp-mode . sly-editing))

;; (use-package elisp-slime-nav
;;     :ensure t
;;     :init
;;     (dolist (hook '(emacs-lisp-mode-hook ielm-mode-hook))
;;       (add-hook hook 'elisp-slime-nav-mode)))

(use-package racket-mode
    :ensure t)

(use-package lispy
    :ensure t
    :bind(("C-c C-v l" . lispy-mode)))

(use-package geiser
    :ensure t
    :init
    (setq geiser-default-implementation 'racket))

(provide 'lisp-module)
;;; lisp-module ends here

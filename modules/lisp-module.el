;;; package -- Summary
;;; Commentary:
;;; Lisp settings
;;; Code:
;; (load (expand-file-name "~/.roswell/helper.el"))
(remove-hook 'lisp-mode-hook 'slime-lisp-mode-hook)

(use-package sly
    :straight t
    :init
    (setq org-babel-lisp-eval-fn 'sly-eval)
    (setq inferior-lisp-program "ros -Q run")
    :hook (lisp-mode . sly-editing))

;; (use-package elisp-slime-nav
;;     :straight t
;;     :init
;;     (dolist (hook '(emacs-lisp-mode-hook ielm-mode-hook))
;;       (add-hook hook 'elisp-slime-nav-mode)))

(use-package racket-mode
    :straight t)

(use-package lispy
    :straight t
    :bind(("C-c C-v l" . lispy-mode)))

(use-package geiser
    :straight t
    :init
    (setq geiser-default-implementation 'racket))

(provide 'lisp-module)
;;; lisp-module ends here

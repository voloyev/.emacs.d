;;; package --- Summary
;;; Commentary:
;;; js module
;;; Code:
;; js2-mode
(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
(custom-set-variables '(coffee-tab-width 2))
(custom-set-variables '(js2-basic-offset 2))
(custom-set-variables '(js-basic-offset 2))
(custom-set-variables '(jsx-basic-offset 2))
(custom-set-variables '(vue-basic-offset 2))
(setq js-indent-level 2)
(setq js2-indent-level 2)
(setq vue-indent-level 2)

;; vue mode
(use-package vue-mode
    :ensure t)

;; typescript
(use-package tide
    :ensure t)

(defun setup-tide-mode ()
    (interactive)
    (tide-setup)
    (flycheck-mode +1)
    (setq flycheck-check-syntax-automatically '(save mode-enabled))
    (eldoc-mode +1)
    (tide-hl-identifier-mode +1)
    ;; company is an optional dependency. You have to
    ;; install it separately via package-install
    ;; `M-x package-install [ret] company`
    (company-mode +1))

;; aligns annotation to the right hand side
(setq company-tooltip-align-annotations t)

;; formats the buffer before saving
(add-hook 'before-save-hook 'tide-format-before-save)

(add-hook 'typescript-mode-hook #'setup-tide-mode)
(provide 'js-module)
;;; js-module.el ends here

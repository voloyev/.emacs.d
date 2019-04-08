;;; package --- Summary
;;; Commentary:
;;; js module
;;; Code:
;; js2-mode
(add-to-list 'auto-mode-alist '("\\.js\\'"  . js2-mode))
(add-to-list 'auto-mode-alist '("\\.vue\\'" . vue-mode))
(add-to-list 'auto-mode-alist '("\\.js\\'"  . auto-highlight-symbol-mode))

(custom-set-variables '(coffee-tab-width 2))
(custom-set-variables '(js2-basic-offset 2))
(custom-set-variables '(js-basic-offset 2))
(custom-set-variables '(jsx-basic-offset 2))
(custom-set-variables '(vue-basic-offset 2))

(setq js-indent-level 2)
(setq js2-indent-level 2)
(setq vue-indent-level 2)

(use-package vue-mode
    :ensure t
    :config
    (add-hook 'mmm-mode-hook
              (lambda ()
                (set-face-background 'mmm-default-submode-face nil))))

;;pretier
(use-package prettier-js
    :ensure t
    :init
    (add-hook 'js2-mode-hook 'prettier-js-mode)
    (add-hook 'js-mode-hook 'prettier-js-mode)
    (add-hook 'vue-mode-hook 'prettier-js-mode)
    (add-hook 'js2-jsx-mode-hook 'prettier-js-mode)
    (add-hook 'rjsx-mode-hook 'prettier-js-mode)
    (add-hook 'web-mode-hook #'(lambda ()
                                 (enable-minor-mode
                                  '("\\.vue?\\'" . prettier-js-mode)))))

(use-package js2-refactor
    :ensure t)

(use-package xref-js2
    :ensure t)

;; typescript
;; (use-package tide
;;     :ensure t
;;     :after (typescript-mode company flycheck)
;;     :hook ((typescript-mode . tide-setup)
;;            (typescript-mode . tide-hl-identifier-mode)
;;            (before-save . tide-format-before-save)))

;; (defun setup-tide-mode ()
;;   (interactive)
;;   (tide-setup)
;;   (flycheck-mode +1)
;;   (setq flycheck-check-syntax-automatically '(save mode-enabled))
;;   (eldoc-mode +1)
;;   (tide-hl-identifier-mode +1)
;;   (company-mode +1))

;; ;; aligns annotation to the right hand side
;; (setq company-tooltip-align-annotations t)

;; ;; formats the buffer before saving
;; (add-hook 'before-save-hook 'tide-format-before-save)

;; (add-hook 'typescript-mode-hook #'setup-tide-mode)

(add-hook 'js2-mode-hook #'js2-refactor-mode)
(js2r-add-keybindings-with-prefix "C-c C-r")
(define-key js2-mode-map (kbd "C-k") #'js2r-kill)

;; js-mode (which js2 is based on) binds "M-." which conflicts with xref, so
;; unbind it.
(define-key js-mode-map (kbd "M-.") nil)

(add-hook 'js2-mode-hook
          (lambda ()
            (add-hook 'xref-backend-functions #'xref-js2-xref-backend nil t)))

(add-hook 'js2-mode-hook #'setup-tide-mode)

(defun enable-minor-mode (my-pair)
  "Enable minor mode if filename match the regexp.  MY-PAIR is a cons cell (regexp . minor-mode)."
  (if (buffer-file-name)
      (if (string-match (car my-pair) buffer-file-name)
          (funcall (cdr my-pair)))))

(require 'company)
(require 'company-tern)

(use-package prettier-js
    :ensure t)

(add-to-list 'company-backends 'company-tern)
(add-hook 'js2-mode-hook (lambda ()
                           (tern-mode)
                           (company-mode)))
;; vue
(add-hook 'web-mode-hook #'(lambda ()
                             (enable-minor-mode
                              '("\\.vue?\\'" . tern-mode))))

;; Disable completion keybindings, as we use xref-js2 instead
(define-key tern-mode-keymap (kbd "M-.") nil)
(define-key tern-mode-keymap (kbd "M-,") nil)

(provide 'js-module)
;;; js-module.el ends here

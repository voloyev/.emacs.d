;;; package --- Summary
;;; Commentary:
;;; js module
;;; Code:
;; js2-mode

(use-package vue-mode
    :ensure t
    :mode ("\\.vue\\'" . vue-mode)
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

(use-package company-tern
    :ensure t)

(use-package js2-mode
    :ensure t
    :mode ("\\.js\\'"   . js2-jsx-mode)
    :hook (js2-jsx-mode . tern-mode)
    :hook (js2-jsx-mode . company-mode)
    :hook (js2-jsx-mode . smartparense-mode))

(use-package rjsx-mode
    :ensure t
    ;; :mode ("\\.js\\'"  . rjsx-mode)
    :hook (rjsx-mode   . tern-mode)
    :hook (rjsx-mode   . company-mode))


(use-package rjsx-mode
    :ensure t
    ;; :mode ("\\.js\\'"  . rjsx-mode)
    :hook (rjsx-mode   . tern-mode)
    :hook (rjsx-mode   . company-mode))

(use-package elm-mode
    :ensure t
    :mode ("\\.elm\\'" . elm-mode)
    :config (elm-format-on-save-mode t))

(custom-set-variables '(coffee-tab-width 2))
(custom-set-variables '(js2-basic-offset 2))
(custom-set-variables '(js-basic-offset 2))
(custom-set-variables '(jsx-basic-offset 2))
(custom-set-variables '(rjsx-basic-offset 2))
(custom-set-variables '(vue-basic-offset 2))

(setq js-indent-level 2)
(setq js2-indent-level 2)
(setq vue-indent-level 2)

(add-hook 'js2-mode-hook  #'lsp-deferred)
(add-hook 'js2-jsx-mode-hook  #'lsp-deferred)
(add-hook 'vue-mode-hook  #'lsp-deferred)

;; aligns annotation to the right hand side
(setq company-tooltip-align-annotations t)

;; tern setup
(add-to-list 'company-backends 'company-tern)

(provide 'js-module)
;;; js-module.el ends here

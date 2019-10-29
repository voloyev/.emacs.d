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
    (add-hook 'web-mode-hook #'(lambda ()
                                 (enable-minor-mode
                                  '("\\.vue?\\'" . prettier-js-mode))))
    :hook (js2-mode     . prettier-js-mode)
    :hook (js-mode      . prettier-js-mode)
    :hook (vue-mode     . prettier-js-mode)
    :hook (js2-jsx-mode . prettier-js-mode)
    :hook (rjsx-mode    . prettier-js-mode))

(use-package company-tern
    :ensure t)

(use-package js-mode
    :mode ("\\.js\\'" . js-mode)
    :hook (tern-mode . js-mode)
    :hook (smartparense-mode . js-mode))

(use-package js-mode
    :mode ("\\.jsx\\'" . js-mode)
    :hook (j2-minore-mode . js-mode))

;; (use-package elm-mode
;;     :ensure t
;;     :config (setq elm-format-on-save t)
;;     :hook (smartparens-mode . elm-mode))
(custom-set-variables '(coffee-tab-width 2))
(custom-set-variables '(js2-basic-offset 2))
(custom-set-variables '(js-basic-offset 2))
(custom-set-variables '(jsx-basic-offset 2))
(custom-set-variables '(rjsx-basic-offset 2))
(custom-set-variables '(vue-basic-offset 2))

(setq js-indent-level 2)
(setq js2-indent-level 2)
(setq vue-indent-level 2)

;; aligns annotation to the right hand side
(setq company-tooltip-align-annotations t)

;; elm-mode
(add-hook 'elm-mode-hook 'smartparens-mode-hook)
(setq elm-format-on-save t)
;; tern setup
(add-to-list 'company-backends 'company-tern)
(add-to-list 'company-backends 'company-elm)
;; (setq elm-format-on-save t)

(provide 'js-module)
;;; js-module.el ends here

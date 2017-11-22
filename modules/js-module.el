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

(use-package vue-mode
    :ensure t)

(provide 'js-module)
;;; js-module.el ends here

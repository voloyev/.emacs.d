(use-package lsp-mode
    :commands lsp
    :ensure t
    :init
    (setq lsp-auto-guess-root t)       ; Detect project root
    (setq lsp-prefer-flymake nil)      ; Use lsp-ui and flycheck
    (setq lsp-enable-xref t)
    (setq flymake-fringe-indicator-position 'right-fringe))


(use-package lsp-ui
    :commands lsp-ui-mode
    :ensure t)

(use-package company-lsp
    :commands company-lsp
    :ensure t)

(add-hook 'js2-mode-hook #'lsp)
(add-hook 'js2-jsx-mode-hook #'lsp)
(add-hook 'vue-mode-hook #'lsp)

(provide 'lsp-module)
;;; lsp-module ends here

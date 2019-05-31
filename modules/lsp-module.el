(use-package lsp-mode
    :commands lsp
    :ensure t
    :init
    (setq lsp-auto-guess-root t)       ; Detect project root
    (setq lsp-prefer-flymake nil)      ; Use lsp-ui and flycheck
    (setq lsp-enable-xref t)
    (setq flymake-fringe-indicator-position 'right-fringe)
    :hook
    (js2-mode . lsp)
    (js2-jsx-mode . lsp)
    (vue-mode . lsp)
    (ruby-mode . lsp)
    :commands lsp)

(use-package lsp-ui
    :commands lsp-ui-mode
    :ensure t)

(use-package company-lsp
    :commands company-lsp
    :ensure t)

(provide 'lsp-module)
;;; lsp-module ends here

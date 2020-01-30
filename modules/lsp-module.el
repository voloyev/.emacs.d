(use-package lsp-mode
    :ensure t
    :init
    (setq lsp-auto-guess-root t)       ; Detect project root
    (setq lsp-prefer-flymake nil)      ; Use lsp-ui and flycheck
    (setq lsp-enable-xref t)
    :hook (js-mode   . lsp-deferred)
    :hook (vue-mode  . lsp-deferred)
    :hook (ruby-mode . lsp-deferred)
    :commands (lsp lsp-deferred))

(use-package lsp-ui
    :commands lsp-ui-mode
    :ensure t)

(use-package company-lsp
    :commands company-lsp
    :ensure t)

(provide 'lsp-module)
;;; lsp-module ends here

(use-package lsp-mode
    :commands lsp
    :ensure t
    :hook (ruby-mode)
    :hook (js2-mode)
    :hook (js2-jsx-mode)
    :hook (vue-mode))

(use-package lsp-ui
    :commands lsp-ui-mode
    :ensure t)

(use-package company-lsp
    :commands company-lsp
    :ensure t)

(provide 'lsp-module)
;;; lsp-module ends here

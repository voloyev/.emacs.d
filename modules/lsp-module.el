(use-package lsp
    :config
  (require 'lsp-clients))

(use-package lsp-mode
    :commands lsp
    :ensure t)

(add-hook 'ruby-mode-hook 'lsp)
(add-hook 'js2-mode-hook 'lsp)
(add-hook 'js2-jsx-mode-hook 'lsp)
(add-hook 'vue-mode-hook 'lsp)

(use-package lsp-ui
    :commands lsp-ui-mode
    :ensure t)

(add-hook 'lsp-mode-hook 'lsp-ui-mode)

(use-package company-lsp
    :commands company-lsp
    :ensure t)

(push 'company-lsp company-backends)

(provide 'lsp-module)
;;; lsp-module ends here

(use-package lsp-mode
    :straight t
    :init
    (setq lsp-keymap-prefix "C-c C-c l")
    (setq lsp-auto-guess-root t)       ; Detect project root
    (setq lsp-prefer-flymake nil)      ; Use lsp-ui and flycheck
    (setq lsp-enable-xref t)
    (setq lsp-enable-indentation nil)
    :hook (js-mode   . lsp-deferred)
    :hook (vue-mode  . lsp-deferred)
    :hook (ruby-mode . lsp-deferred)
    :commands (lsp lsp-deferred))


(use-package lsp-ivy
    :straight t
    :commands lsp-ivy-workspace-symbol)

(use-package lsp-treemacs
    :straight t
    :commands lsp-treemacs-errors-list)

(use-package lsp-ui
    :straight t
    :commands lsp-ui-mode
    :straight t)

(use-package company-lsp
    :straight t
    :commands company-lsp
    :straight t)

(provide 'lsp-module)
;;; lsp-module ends here

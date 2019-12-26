;;; package --- Summary:
;;; Code:
;;; Commentary:
;; rust module

(use-package company-racer
    :ensure t)

(add-to-list 'company-backends 'company-racer)

(use-package rust-mode
    :mode "\\.rs\\'"
    :ensure t
    :init
    (add-hook 'rust-mode-hook #'racer-mode)
    (add-hook 'racer-mode-hook #'eldoc-mode)
    (add-hook 'racer-mode-hook #'company-mode)
    (add-hook 'rust-mode #'flycheck-inline-mode)
    (setq rust-format-on-save t))

(use-package flycheck-rust
    :ensure t
    :after flycheck
    :commands flycheck-rust-setup
    :init
    (add-hook 'flycheck-mode-hook #'flycheck-rust-setup))

(use-package racer
    :commands racer-mode
    :diminish racer-mode
    :ensure t)

(use-package cargo
    :commands cargo-minor-mode
    :diminish cargo-minor-mode
    :init
    (add-hook 'rust-mode-hook 'cargo-minor-mode))

(use-package toml-mode
    :mode (("\\.toml\\'" . toml-mode)))

(provide 'rust-module)
;;; rust-module.el ends here

;;; package --- Summary:
;;; Code:
;;; Commentary:
;; rust module

(use-package company-racer
    :straight t)

(add-to-list 'company-backends 'company-racer)

(use-package rust-mode
    :mode "\\.rs\\'"
    :straight t
    :init
    (add-hook 'rust-mode-hook #'racer-mode)
    (add-hook 'racer-mode-hook #'eldoc-mode)
    (add-hook 'racer-mode-hook #'company-mode)
    (setq rust-format-on-save t))

(use-package flycheck-rust
    :straight t
    :after flycheck
    :commands flycheck-rust-setup
    :init
    (add-hook 'flycheck-mode-hook #'flycheck-rust-setup))

(use-package racer
    :commands racer-mode
    :diminish racer-mode
    :straight t)

(use-package cargo
    :commands cargo-minor-mode
    :diminish cargo-minor-mode
    :init
    (add-hook 'rust-mode-hook 'cargo-minor-mode))

(use-package toml-mode
    :mode (("\\.toml\\'" . toml-mode)))

(provide 'rust-module)
;;; rust-module.el ends here

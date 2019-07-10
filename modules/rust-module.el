;;; package --- Summary:
;;; Code:
;;; Commentary:
;; rust module

(use-package company-racer
    :ensure t)

(with-eval-after-load 'company
  (add-to-list 'company-backends 'company-racer))

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
    :ensure t
    :commands racer-mode
    :diminish racer-mode
    :init
    :bind (("M-." . racer-find-definition)))

(use-package cargo
    :commands cargo-minor-mode
    :diminish cargo-minor-mode
    :init
    (add-hook 'rust-mode-hook 'cargo-minor-mode))

(use-package toml-mode
    :mode (("\\.toml\\'" . toml-mode)))

(use-package rust-playground
    :ensure t
    :bind(("C-c C-r e" . rust-playground-exec)))

(provide 'rust-module)
;;; rust-module.el ends here

;;; package --- Summary:
;;; Code:
;;; Commentary:
;; rust module
;; rust
;;(add-to-list 'auto-mode-alist '("\\.rs\\'" . rust-mode))

;; racer

;; (use-package rust-mode
;;     :ensure t
;;     :init
;;     (setq company-tooltip-align-annotations t)
;;     (add-hook 'rust-mode-hook #'racer-mode)
;;     (add-hook 'racer-mode-hook #'eldoc-mode)
;;     (add-hook 'flycheck-mode-hook #'flycheck-rust-setup)
;;     (add-hook 'racer-mode-hook #'company-mode))

;;; Change config start
;; -=[ Rust


(require 'company-racer)

(with-eval-after-load 'company
  (add-to-list 'company-backends 'company-racer))

(use-package rust-mode
    :mode "\\.rs\\'"
    :ensure t
    :init
    (add-hook 'rust-mode-hook #'racer-mode)
    (add-hook 'racer-mode-hook #'eldoc-mode)
    (add-hook 'racer-mode-hook #'company-mode)
    (setq rust-format-on-save t))

(use-package flycheck-rust
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
;;; change config end
;; (use-package rust-mode
;;     :mode "\\.rs\\'"
;;     :init
;;     (setq rust-format-on-save t))
;; (use-package lsp-mode
;;     :init
;;     (add-hook 'prog-mode-hook 'lsp-mode)
;;     :config
;;     (use-package lsp-flycheck
;;         :ensure f ; comes with lsp-mode
;;         :after flycheck))
;; (use-package lsp-rust
;;     :after lsp-mode)
(provide 'rust-module)
;;; rust-module.el ends here

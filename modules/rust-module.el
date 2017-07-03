;;; package --- Summary:
;;; Code:
;;; Commentary:
;; rust module
;; rust
(add-to-list 'auto-mode-alist '("\\.rs\\'" . rust-mode))

;; racer

(use-package rust-mode
    :init
    (setq company-tooltip-align-annotations t)
    (add-hook 'rust-mode-hook #'racer-mode)
    (add-hook 'racer-mode-hook #'eldoc-mode)
    (add-hook 'flycheck-mode-hook #'flycheck-rust-setup)
    (add-hook 'racer-mode-hook #'company-mode))

;; line number
(use-package nlinum
    :bind (("\C-cl" . nlinum-mode))
    :init
    (add-hook 'rust-mode-hook 'nlinum-mode))
(provide 'rust-module)
;;; rust-module.el ends here

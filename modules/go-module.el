;;; package --- Summary
;;; Commentary:
;;; ruby module
;;; Code:
(use-package flycheck-gometalinter
    :ensure t
    :config
    (progn
        (flycheck-gometalinter-setup)))
;; exec
(when (memq window-system '(mac ns))
    (exec-path-from-shell-initialize)
    (exec-path-from-shell-copy-env "GOPATH"))

;; company mode
(add-hook 'go-mode-hook
          (lambda ()
              (set (make-local-variable 'company-backends) '(company-go))
              (company-mode)))

;; gorepl-mode
(use-package gorepl-mode
    :ensure t
    :init
    (add-hook 'go-mode-hook #'gorepl-mode))

(provide 'go-module)
;;; go-module.el ends here

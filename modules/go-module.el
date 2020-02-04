;;; package --- Summary
;;; Commentary:
;;; ruby module
;;; Code:
(use-package flycheck-gometalinter
    :straight t
    :config
    (progn
      (flycheck-gometalinter-setup)))

;; company mode
(add-hook 'go-mode-hook
          (lambda ()
            (set (make-local-variable 'company-backends)
                 '(company-go))
            (company-mode)
            (setq gofmt-command "goimports")
            (add-hook 'before-save-hook 'gofmt-before-save)
            (setq tab-width 4)
            (setq indent-tabs-mode 1)))

;; gorepl-mode
(use-package gorepl-mode
    :straight t
    :init
    (add-hook 'go-mode-hook #'gorepl-mode))

(add-to-list 'load-path (concat
                         (getenv "GOPATH")
                         "/src/github.com/golang/lint/misc/emacs"))

(use-package golint
    :straight t)

(provide 'go-module)
;;; go-module.el ends here

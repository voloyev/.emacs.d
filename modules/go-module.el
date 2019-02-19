;;; package --- Summary
;;; Commentary:
;;; ruby module
;;; Code:
(add-to-list 'exec-path "~/workspace/go/bin")
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
            (company-mode)
            (add-hook 'before-save-hook 'gofmt-before-save)
            (setq tab-width 4)
            (setq indent-tabs-mode 1)))

;; gorepl-mode
(use-package gorepl-mode
    :ensure t
    :init
    (add-hook 'go-mode-hook #'gorepl-mode))

(add-to-list 'load-path (concat (getenv "GOPATH")  "/src/github.com/golang/lint/misc/emacs"))

(use-package golint
    :ensure t)

(provide 'go-module)
;;; go-module.el ends here

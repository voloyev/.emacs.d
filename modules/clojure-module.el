;;; package --- Summary:
;;; Code:
;;; Commentary:
;; clojure module
(use-package cider
    :ensure t
    :init
    (add-hook 'cider-repl-mode-hook #'company-mode)
    (add-hook 'cider-mode-hook #'company-mode)
    (add-hook 'cider-repl-mode-hook #'cider-company-enable-fuzzy-completion)
    (add-hook 'cider-mode-hook #'cider-company-enable-fuzzy-completion))

(use-package clojure-mode
    :ensure t
    :config
    (setq clojure-indent-style 'always-indent))

;; ;;lein exec path
;; (add-to-list 'exec-path "~/bin")

(provide 'clojure-module)
;;; clojure-module.el ends here

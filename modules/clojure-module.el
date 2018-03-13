;;; package --- Summary:
;;; Code:
;;; Commentary:
;; clojure module
(use-package cider
    :ensure t)
(use-package clojure-mode
    :ensure t)

(add-hook 'cider-repl-mode-hook #'company-mode)
(add-hook 'cider-mode-hook #'company-mode)
(add-hook 'cider-repl-mode-hook #'cider-company-enable-fuzzy-completion)
(add-hook 'cider-mode-hook #'cider-company-enable-fuzzy-completion)

(provide 'clojure-module)
;;; clojure-module.el ends here

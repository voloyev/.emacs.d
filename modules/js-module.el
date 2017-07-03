;;; package --- Summary
;;; Commentary:
;;; js module
;;; Code:
;; js2-mode
(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))

(use-package nlinum
    :bind (("\C-cl" . nlinum-mode))
    :init
    (add-hook 'js2-mode-hook 'nlinum-mode)
    (add-hook 'js2-jsx-mode-hook 'nlinum-mode))
(provide 'js-module)
;;; js-module.el ends here

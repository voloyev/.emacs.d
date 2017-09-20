;;; package --- Summary:
;;; Code:
;;; Commentary:
;; crystal module

;; crystal mode
(add-to-list 'load-path
             "~/.emacs.d/plugins/")
(autoload 'crystal-mode "crystal-mode" "Major mode for crystal files" t)
(add-to-list 'auto-mode-alist '("\\.cr$" . crystal-mode))
(add-to-list 'interpreter-mode-alist '("crystal" . crystal-mode))
(require 'flycheck-crystal)
(add-hook 'crystal-mode-hook 'flycheck-mode)
(provide 'crystal-module)
;;; crystal-module.el ends here

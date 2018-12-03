;;Indent settings
(setq-default indent-tabs-mode nil)
(setq tab-width                  2)
(setq-default tab-width          2)
(setq-default standart-indent    2)
(setq-default lisp-body-indent   2)

;; css and sccs indent level
(setq css-indent-offset 2)
(setq scss-indent-offset 2)
(global-set-key (kbd "RET") 'newline-and-indent)
(setq lisp-indent-function  'common-lisp-indent-function)

(provide 'indent-module)
;;; indent-module ends here

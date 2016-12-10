;;ruby
;;rvm
(require 'rvm)
;;; Code:
(rvm-use-default)
(rvm-activate-corresponding-ruby)
;;rbenv
;;(require 'rbenv)
;;(global-rbenv-mode)
(require 'ruby-tools)
(setq ruby-indent-level 2)
(add-hook 'ruby-mode-hook #'rubocop-mode)
(setq ruby-deep-indent-paren nil)
(require 'robe)
(add-hook 'ruby-mode-hook 'robe-mode)
;;(defadvice inf-ruby (before activate-rvm-for-robe activate)
;;    (rvm-activate-corresponding-ruby))
(eval-after-load 'company
    '(push 'company-robe company-backends))
;; shortkey for company-complete
(global-set-key (kbd "<f6>") 'company-complete)
(global-set-key (kbd "C-c r a") 'rvm-activate-corresponding-ruby)
(global-set-key (kbd "C-c r r") 'inf-ruby)
(require 'bundler)

;; rinari
(setq rinari-tags-file-name "TAGS")
(provide 'ruby-module)
;;; ruby-module.el ends here

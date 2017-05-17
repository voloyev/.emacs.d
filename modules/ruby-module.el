;;ruby
;;rvm
;;(require 'rvm)
;;; Code:
;;(rvm-use-default)
;;(rvm-activate-corresponding-ruby)
;;rbenv
(require 'rbenv)
(global-rbenv-mode)
(require 'ruby-tools)
(setq ruby-indent-level 2)
(add-hook 'ruby-mode-hook #'rubocop-mode)
(setq ruby-deep-indent-paren nil)
(require 'robe)
;; (add-hook 'ruby-mode-hook 'robe-mode)
(add-to-list 'auto-mode-alist
             '("\\(?:\\.rb\\|ru\\|rake\\|thor\\|jbuilder\\|gemspec\\|podspec\\|/\\(?:Gem\\|Rake\\|Cap\\|Thor\\|Vagrant\\|Guard\\|Pod\\)file\\)\\'" . enh-ruby-mode))
(add-hook 'enh-ruby-mode-hook 'robe-mode)
(add-hook 'enh-ruby-mode-hook 'yard-mode)

;; shortkey for company-complete
(global-set-key (kbd "C-c r a") 'rvm-activate-corresponding-ruby)
(global-set-key (kbd "C-c r r") 'inf-ruby)
(global-set-key (kbd "C-c C-c r s") 'robe-start)
(require 'bundler)

;; rinari
(setq rinari-tags-file-name "TAGS")
(provide 'ruby-module)
;;; ruby-module.el ends here

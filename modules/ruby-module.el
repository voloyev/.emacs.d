;;; package --- Summary
;;; Commentary:
;;; ruby module
;;; Code:
"if you use rvm
you should uncomment
this section.
;;(use-package rvm
;;:init
;;(rvm-use-default)
;;(rvm-activate-corresponding-ruby))
"
;;rbenv
(use-package rbenv
    :ensure t
    :config
    (global-rbenv-mode 1))

(use-package ruby-tools
    :init
    (setq ruby-indent-level 2)
    (setq ruby-deep-indent-paren nil))

(use-package robe
    :ensure t
    :init
    (add-to-list 'auto-mode-alist
                 '("\\(?:\\.rb\\|ru\\|rake\\|thor\\|jbuilder\\|gemspec\\|podspec\\|/\\(?:Gem\\|Rake\\|Cap\\|Thor\\|Vagrant\\|Guard\\|Pod\\)file\\)\\'" . ruby-mode))
    (add-hook 'ruby-mode-hook 'robe-mode)
    (add-hook 'ruby-mode-hook 'yard-mode)

    :bind(("C-c r a" . rvm-activate-corresponding-ruby)
          ("C-c r r" . inf-ruby-console-auto)
          ( "C-c C-c r s" . robe-start)
          ("C-c & h r" . enh-ruby-mode)))
(use-package bundler
    :ensure t)

(defadvice inf-ruby-console-auto (before activate)
  (rbenv-use-corresponding))

;; rinari
(use-package rinari
    :ensure t
    :init
    (setq rinari-tags-file-name "TAGS"))

;;slim-mode
(use-package slim-mode
    :ensure t
    :init
    (add-to-list 'auto-mode-alist '("\\.slim\\'" . slim-mode)))

(use-package ruby-refactor
    :ensure t
    :config
    (add-hook 'ruby-mode-hook 'ruby-refactor-mode-launch))

(setq ruby-insert-encoding-magic-comment nil)

(use-package minitest
    :ensure t
    :config
    (add-hook 'ruby-mode-hook 'minitest-mode))

(provide 'ruby-module)
;;; ruby-module.el ends here

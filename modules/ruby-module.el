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
(use-package ruby-mode
    :config (setq ruby-insert-encoding-magic-comment nil)
    :mode ("\\(?:\\.rb\\|ru\\|rake\\|thor\\|jbuilder\\|gemspec\\|podspec\\|/\\(?:Gem\\|Rake\\|Cap\\|Thor\\|Vagrant\\|Guard\\|Pod\\)file\\)\\'" . ruby-mode)
    :hook (robe-mode)
    :hook (yard-mode)
    :bind(("C-c r a"      . chruby-use-corresponding)
          ("C-c r r"      . inf-ruby-console-auto)
          ("C-c & h r"    . enh-ruby-mode)))

;;rbenv
(use-package rbenv
    :ensure t)

(use-package chruby
    :ensure t)

(use-package ruby-tools
    :init
  (setq ruby-indent-level 2)
  (setq ruby-deep-indent-paren nil))

(use-package robe
    :ensure t
    :bind(("C-c C-c r s"  . robe-start)))

(use-package bundler
    :ensure t)

(defadvice inf-ruby-console-auto (before activate)
  "Activate corespongeting ruby when use inf-ruby."
  (chruby-use-corresponding))

(add-to-list 'inf-ruby-implementations '("pry" . "pry"))
(setq inf-ruby-default-implementation "pry")
(setq inf-ruby-first-prompt-pattern "^\\[[0-9]+\\] pry\\((.*)\\)> *")
(setq inf-ruby-prompt-pattern "^\\[[0-9]+\\] pry\\((.*)\\)[>*\"'] *")

;; rinari
(use-package rinari
    :ensure t
    :init
    (setq rinari-tags-file-name "TAGS"))

;; slim-mode
(use-package slim-mode
    :ensure t
    :mode ("\\.slim\\'" . slim-mode))

(use-package minitest
    :ensure t
    :config
    :hook (ruby-mode)
    :hook (enh-ruby-mode))

;; hyde jekyll mode
(use-package hyde
    :ensure t)

(provide 'ruby-module)
;;; ruby-module.el ends here

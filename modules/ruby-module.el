;;; package --- Summary
;;; Commentary:
;;;; ruby module
"For propper usage of lsp mode in ruby
you should generate documentation for projects gems.
#+BEGIN_SRC bash
yard gems
#+END_SRC
if you use rvm or rbenv
you should uncomment
this section.
(use-package rvm
;;:init
  (rvm-use-default)
  (rvm-activate-corresponding-ruby))
"
;;; Code:
(straight-use-package 'rake)
(add-hook 'after-init-hook 'inf-ruby-switch-setup)

(use-package ruby-mode
  :init   (setq ruby-insert-encoding-magic-comment nil)
  :mode ("\\(?:\\.rb\\|ru\\|rake\\|thor\\|jbuilder\\|gemspec\\|podspec\\|/\\(?:Gem\\|Rake\\|Cap\\|Thor\\|Vagrant\\|Guard\\|Pod\\)file\\)\\'" . ruby-mode)
  :hook (robe-mode)
  :hook (yard-mode)
  :bind(("C-c r r"      . inf-ruby-console-auto)
        ("C-c & h r"    . enh-ruby-mode)))

(use-package chruby
  :straight t)

(use-package ruby-tools
  :straight t
  :init
  (setq ruby-indent-level 2)
  (setq ruby-deep-indent-paren nil))

(use-package rcodetools
  :init(define-key ruby-mode-map (kbd "C-c C-u C-c") 'xmp))

(use-package bundler
  :straight t)

(defadvice inf-ruby-console-auto (before activate)
  "Activate corespongeting ruby when use inf-ruby."
  (chruby-use-corresponding))

(use-package slim-mode
  :straight t
  :mode ("\\.slim\\'" . slim-mode))

(use-package haml-mode
  :mode ("\\.haml\\'" . haml-mode))

;; jekyll mode
(use-package hyde
  :straight t)

(use-package rspec-mode
  :straight t)

(add-hook 'projectile-after-switch-project-hook #'chruby-use-corresponding)

;; (add-to-list 'company-backends 'company-robe)

(provide 'ruby-module)
;;; ruby-module.el ends here

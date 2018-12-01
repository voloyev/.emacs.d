;;; package --- Summary
;;; Commentary:
;; Default setup of smartparens
;;; Code:

(use-package smartparens
    :config
    (show-smartparens-global-mode 1)

    (require 'smartparens-config)
    (--each '(restclient-mode-hook
              js-mode-hook
              vue-mode-hook
              js2-mode-hook
              python-mode-hook
              web-mode-hook
              ruby-mode-hook
              markdown-mode-hook
              org-mode-hook
              rust-mode-hook
              toml-mode-hook
              cc-mode-hook
              lisp-mode-hook
              emacs-lisp-mode-hook
              haml-mode-hook
              c-mode-hook
              go-mode-hook
              elixir-mode-hook
              enh-ruby-mode-hook
              crystal-mode-hook
              slim-mode-hook
              haml-mode-hook
              rust-mode-hook
              yaml-mode-hook
              nginx-mode-hook
              scss-mode-hook
              web-mode-hook
              conf-mode-hook
              emacs-lisp-mode-hook
              clojure-mode-hook)
        (add-hook it 'turn-on-smartparens-mode)))
(provide 'smartparens-module)
;;; smartparens-module ends here

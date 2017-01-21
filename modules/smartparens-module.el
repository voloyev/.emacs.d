;;; package --- Summary
;;; Commentary:
;; Default setup of smartparens
(require 'smartparens-config)
;;; Code:
(add-hook 'js2-mode-hook 'smartparens-mode)
(add-hook 'emacs-lisp-mode-hook 'smartparens-mode)
(add-hook 'web-mode-hook 'smartparens-mode)

(--each '(restclient-mode-hook
          js-mode-hook
          python-mode-hook
          web-mode-hook
          ruby-mode-hook
          markdown-mode-hook
          org-mode-hook
          rust-mode-hook
          cc-mode-hook
          lisp-mode-hook
          emacs-lisp-mode-hook
          haml-mode-hook
          c-mode-hook
          go-mode-hook)
    (add-hook it 'turn-on-smartparens-mode))
(provide 'smartparens-module)
;;; smartparens-module ends here

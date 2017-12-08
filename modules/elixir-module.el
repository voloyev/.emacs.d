;;; package --- Summary:
;;; Code:
;;; Commentary:
;; elixir module

;; Highlights *.elixir2 as well
(use-package elixir-mode
    :ensure t
    :config
    (add-to-list 'auto-mode-alist '("\\.elixir2\\'" . elixir-mode))
    (add-to-list 'auto-mode-alist '("\\.ex\\'" . elixir-mode))
    (setq alchemist-mix-command "mix")
    (setq alchemist-mix-test-task "espec")
    (setq alchemist-mix-test-default-options '()) ;; default
    (setq alchemist-iex-program-name "iex") ;; default: iex
    (setq alchemist-execute-command "elixir") ;; default: elixir
    (setq alchemist-compile-command "elixirc") ;; default: elixirc
    (setq alchemist-hooks-compile-on-save t))

(use-package alchemist
    :ensure t)

(provide 'elixir-module)
;;; elixir-module.el ends here

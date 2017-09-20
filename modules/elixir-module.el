;;; package --- Summary:
;;; Code:
;;; Commentary:
;; elixir module

;; Highlights *.elixir2 as well
(add-to-list 'auto-mode-alist '("\\.elixir2\\'" . elixir-mode))
(setq alchemist-mix-command "/usr/bin/mix")
(setq alchemist-mix-test-task "espec")
(setq alchemist-mix-test-default-options '()) ;; default
(setq alchemist-iex-program-name "/usr/bin/iex") ;; default: iex
(setq alchemist-execute-command "/usr/bin/elixir") ;; default: elixir
(setq alchemist-compile-command "/usr/bin/elixirc") ;; default: elixirc
(setq alchemist-hooks-compile-on-save t)

(provide 'elixir-module)
;;; elixir-module.el ends here
